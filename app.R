library(shiny)
if(system.file(package = 'shiny') == ""){
  install.packages('shiny')
}
if(system.file(package = 'devtools') == ""){
  install.packages('devtools')
}
if(system.file(package = 'plotly') == ""){
  install.packages('plotly')
}
library(shiny)
library(devtools)
library(plotly)
source("https://raw.githubusercontent.com/rejeton1/First_RMarkdown_Presentation/gh-pages/Course_Project.R")



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
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

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output){
  #plot part
  output$plot <- renderPlotly({
    fig1 <- plot_ly(x=data$FCB, y=data$Oppo, z=Win_Model$fitted.values, color=as.factor(data$Win)) %>%
      layout(scene=list(xaxis=list(title='FCB Score'), yaxis=list(title='Opponent Score'), 
                        zaxis=list(title='Win Probability')))
    fig1
  })
  
  #text output part
  FCBScore <- reactive({
    input$Score_FCB
  })
  OppoScore <- reactive({
    input$Score_Oppo
  })
  Time <- reactive({
    input$Time
  })
  
  output$Win <- renderText(predict(Win_Model, newdata = 
                                     data.frame(FCB=FCBScore(),
                                                Oppo=OppoScore(),
                                                Time=Time()), type = 'response')*100)
  
})

# Run the application 
shinyApp(ui = ui, server = server)
