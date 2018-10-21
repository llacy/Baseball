#server file (where all the work happens)
setwd("C:/Users/llacy/OneDrive/Baseball/Win Probability")
###Probability of win. attendence, weather, 10 game wrc+, home/road, day/night, days since off day, 2 start era- for sp

#create win probabilities based on score by inning
#teams win 1-0 lead in 3rd inning win XX% of the time
#user should have choice of timeframe (default last two seasons + current?)
#choose ballpark?
#read linescore for each game into R
#user inputs score (if not enough data for specific score, use run differential + inning?)

#df example
#home_team #road_team #home_score #road_score #inning #top #bottom

#install.packages('mlbgameday')
library(mlbgameday)

# Run larger requests in parallel. (only run this if need to re-download files)
#
#library(doParallel)
#library(foreach)
#no_cores <- detectCores() - 2
#cl <- makeCluster(no_cores)
#registerDoParallel(cl)
#df <- get_payload(start = "2016-04-03", end = "2016-10-02")
#stopImplicitCluster()
#rm(cl)
#ab <- df$atbat
#### 2016 data brought in 


# library(doParallel)
# library(foreach)
# no_cores <- detectCores() - 2
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
# df_pred <- get_payload(start = "2017-04-02", end = "2017-10-01")
# stopImplicitCluster()
# rm(cl)
# ab_pred <- df_pred$atbat
# rm(df_pred)
# ab = rbind(ab, ab_pred)
#write.csv(ab, file = 'mlb_linescore1617.csv', row.names = FALSE)
#write.csv(df$atbat, file = 'mlb_linescore.csv', row.names = FALSE)
#after downloading from mlb and writing the CSV I uploaded it to github via gitbash
#read in from github
library(RCurl)


x = getURL("https://raw.githubusercontent.com/llacy/Baseball/master/mlb_linescore.csv")
ab = read.csv(text = x, stringsAsFactors = FALSE)
#ab <- read.csv("mlb_linescore.csv")

#pitch <- df$pitch
#po <- df$po
#test <- ab %>% filter(gameday_link == 'gid_2016_06_01_arimlb_houmlb_1') %>% arrange(inning)

library(dplyr)
linescore <- ab %>% group_by(date,gameday_link,inning,inning_side) %>% 
  summarize(home_score = max(as.numeric(home_team_runs)), away_score = max(as.numeric(away_team_runs))) %>%
  arrange(date, gameday_link, inning, desc(inning_side))

#get home and away team
#gregexpr('_*mlb', linescore[i,'gameday_link'])
#pos = gregexpr('_*mlb',linescore[1,'gameday_link']) # Returns position of every match
#pos[[1]][1] #position of away team (team begins at 16 through 18)
#pos[[1]][2] #position of home team (team name begins at 23 through 25)
#substr(linescore$gameday_link, 16,18)[1] #away
#substr(linescore$gameday_link, 23,25)[1] #home

#returns positions of away team and home team
#this needs to be changed to a function
pos <- numeric(0)
home <- as.character()
away <- as.character()
for(i in 1:nrow(linescore)){
  pos[i] <- gregexpr('_*mlb', linescore[i,'gameday_link'])
  home[i] <- substr(linescore[i,'gameday_link'],pos[[i]][2]-3, pos[[i]][2]-1)  
  away[i] <- substr(linescore[i,'gameday_link'],pos[[i]][1]-3, pos[[i]][1]-1)
}

#attach home team and away team to df
linescore$home_team = home
linescore$away_team = away

#get winners for each game
winners <- linescore %>% 
  group_by(gameday_link) %>% 
  arrange(inning, desc(inning_side)) %>% 
  filter(row_number()==n()) %>%
  mutate(home_win = ifelse(home_score > away_score, 1,0)) %>%
  mutate(away_win = ifelse(away_score > home_score, 1,0)) %>%
  mutate(winner = ifelse(home_win == 1, 'home','away'))

#join winners back to linescore dataframe
linescore_w_winner = linescore %>%
  left_join(winners[,c('date','gameday_link','home_win','away_win','winner')], by=c('date','gameday_link'))

probs = linescore_w_winner %>%
  group_by(inning,inning_side,home_score, away_score) %>%
  summarize(home_team_wins = sum(home_win),
            away_team_wins = sum(away_win),
            games = n())
#setwd("C:/Users/llacy/OneDrive/Baseball/Win Probability/WinProbApp")
#write.csv(probs, "probs.csv", row.names = FALSE)

give_me_the_prob <- function(hs,as,inn, side){
  prob_sub = probs %>% filter(home_score == hs & away_score == as & inning == inn & inning_side == side)
  home_prob = prob_sub$home_team_wins / prob_sub$games
  away_prob = prob_sub$away_team_wins / prob_sub$games
  
  #if games < 5, use run differential?
  #choose ballpark?
  #choose hometeam?
  #choose timeframe? (subset based on date? and how far back?)
  homewin = (paste0("The probability the home team will win is: ",round(home_prob*100,0), "%"))
  awaywin = (paste0("The probability the away team will win is: ",round(away_prob*100,0), "%"))
  conf    = (paste0("Confidence: ", prob_sub$games, " games used in analysis"))
  df = data.frame("Home Probability" = homewin, "Away Probability" = awaywin, "Confidence" = conf)
  return(list("home" = homewin, "away" = awaywin, "confidence" = conf))
  #return(df)
}

give_me_the_prob(4,3,7,'top')

###break into training and test datasets
smp <- floor(0.7*nrow(linescore_w_winner)) #70 training
set.seed(123) #reproducible
train_ind <- sample(seq_len(nrow(linescore_w_winner)), size = smp)
train <- linescore_w_winner[train_ind,]
test <- linescore_w_winner[-train_ind,]

#Logistic Regression predicting outcome
logM = glm(home_win ~ as.Date(date, "%Y-%m-%d") + inning + inning_side + home_score + away_score + as.factor(home_team),
           family = "binomial", data = train)
summary(logM)

#logModel without team
logM2 = glm(home_win ~ as.Date(date, "%Y-%m-%d") + inning + inning_side + home_score + away_score,family = "binomial",
            data = train)
summary(logM2)

#without team or date
logM3 = glm(home_win ~ inning + inning_side + home_score + away_score,family = "binomial",data = train)
summary(logM3)


#need to check goodness of fit
with(linescore_w_winner,plot(home_score, log(home_win/(1-home_win))))
#predict on test set
test$predicted_outcome = predict(logM3,test, type = "response")
test$predicted_outcome_binary = ifelse(predicted_outcome >= 0.5,1,0)

#check accuracy
acc = xtabs(~home_win+predicted_outcome_binary, test)
accuracy = (acc[1,1] + acc[2,2]) / (acc[1,1] + acc[1,2] + acc[2,1] + acc[2,2]) 

#download 2017 data to do predictions

#map out beta distribution updating after each at bat/game predicting final season batting average?

