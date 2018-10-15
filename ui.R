#ui file

shinyUI(
  
  pageWithSidebar(
    
    headerPanel("Win Probabilities"),
    
    sidebarPanel(
      
      helpText("User inputs to probabilities model"),
      
      selectInput("Side", "Top or bottom?",  choices = c("top", "bottom")),
      
      numericInput("Inning", "What inning?", min = 1, max = 18, value=9, step = 1),
      
      br(),
      numericInput("away", "Away Score:", min = 0, step = 0, value = 0),
      
      br(),
      numericInput("home", "Home Score:", min = 0, step = 0, value = 0),
      
      br(),
      actionButton("update","Update View")
    ),
    
    mainPanel(
      h4("The probable outcome:"),
      verbatimTextOutput("view")
    
    )
  )
)

