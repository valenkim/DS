
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(dplyr)
library(stringr)

shinyServer(function(input, output) {
  dataline <- reactive({
    paste(input$name, enc2utf8("번 버스 정류장 리스트"), enc2utf8(" (정류장 이름 - 정류장 id)"), sep="")
    #bustable <- filter(givestation, routenum==input$name)
    #stationid <- bustable[,3]
  })
  
  vect <- reactive({
    paste(enc2utf8(filter(givestation, routenum==input$name)[,3]), filter(givestation, routenum==input$name)[,5], sep="-")
  })
  output$title <- renderText({
    dataline()
  })
  output$title2 <- renderText({
    "노선정보 및 버스도착 예정시간"
  })
  output$datatext <- renderText({
    #for (i in 1:length(filter(givestation, routenum==input$name)[,3])) {
    vect()
    #}
  })
  bokhos <- reactive({
    findbh(input$name)
  })
  link <- reactive({
      arrivetext(input$name, input$name2)
    })
  output$linktext <- renderText({
      link()
    })
  output$bhtext <- renderText({
    bokhos()
  })
  
})

