library(shiny)
library(devtools)
library(plotly)
source("https://raw.githubusercontent.com/rejeton1/First_RMarkdown_Presentation/gh-pages/Course_Project.R")


shinyServer(function(input, output){
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