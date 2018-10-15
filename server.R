#call libraries
library(mlbgameday)
library(RCurl)
library(dplyr)

#download data
#x = getURL("https://raw.githubusercontent.com/llacy/Baseball/master/mlb_linescore.csv")
#ab = read.csv(text = x)
ab = ab

#put in linescore format
linescore <- ab %>% group_by(date,gameday_link,inning,inning_side) %>%
  summarize(home_score = max(as.numeric(home_team_runs)), away_score = max(as.numeric(away_team_runs))) %>%
  arrange(date, gameday_link, inning, desc(inning_side))

#returns positions of away team and home team for each game
#this should be a function and not a loop
pos <- numeric(0)
for(i in 1:nrow(linescore)){
  pos[i] <- gregexpr('_*mlb', linescore[i,'gameday_link'])
}

#scrape home team name and away team name (this should be a function and not a loop)
home <- as.character()
away <- as.character()
for(i in 1:nrow(linescore)){
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

#calculate winning percentage by inning,top/bottom, score
probs = linescore_w_winner %>%
  group_by(inning,inning_side,home_score, away_score) %>%
  summarize(home_team_wins = sum(home_win),
            away_team_wins = sum(away_win),
            games = n())

shinyServer( function(input,output){
  
  #function to spit out results
  give_me_the_prob <- function(hs,as,inn, side){
    prob_sub = probs %>% filter(home_score == hs & away_score == as & inning == inn & inning_side == side)
    home_prob = prob_sub$home_team_wins / prob_sub$games
    away_prob = prob_sub$away_team_wins / prob_sub$games
    
    
    homewin = (paste0("The probability the home team will win is: ",round(home_prob*100,0), "%"))
    awaywin = (paste0("The probability the away team will win is: ",round(away_prob*100,0), "%"))
    conf    = (paste0("Confidence: ", prob_sub$games, " games used in analysis"))
    df = data.frame("Home Probability" = homewin, "Away Probability" = awaywin, "Confidence" = conf)
    return(list("home" = homewin, "away" = awaywin, "confidence" = conf, "homeProb" = home_prob))
    #return(df)
  }

output$view <- renderPrint({
  give_me_the_prob(hs = input$home, as = input$away, inn = input$Inning, side = input$Side)
}) 
 
})