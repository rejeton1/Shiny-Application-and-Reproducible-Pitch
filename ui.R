library(shiny)

shinyUI(fluidPage(
 titlePanel("Win Probability Prediction of FC Barcelona in 22/23"),
 sidebarLayout(
   sidebarPanel(
     h5("This is Shiny web application that predict the win probability 
        of FC Barcelona at specific game time based on the league(La Liga) 
        data in 22/23 season. We used Logistic regresson model for 
        our prediction, and the predict variables are 
        ‘The number of goals scored by FC Barcelona’, 
        ‘The number of goals scored by opponent team’, and 
        ‘The time in the game’. The number of goals by both side is 
        limited from 0 to 10, game time is limited to interger 
        ranging from 0 to 90 minutes."),
     h5("You can check the predicted win probability 
        by selecting input value below. Try it!"),
     numericInput('Score_FCB', "Score of FC Barcelona(0-10)", value=0, min=0, max=10, step = 1),
     numericInput('Score_Oppo', "Score of Opponent(0-10)", value=0, min=0, max=10, step = 1),
     sliderInput('Time', "Game Time(0~90min)", value=0, min =0, max=90, step = 1)
   ),
   mainPanel(
     h3('Prediction Graph of Win Probability of FCB according to Score'),
     plotlyOutput('plot'),
     h3('Win Probability'),
     textOutput('Win')
   )
  )
))
