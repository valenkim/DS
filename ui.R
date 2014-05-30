
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#enc2utf8()
library(shiny)
# Seoul Low Floow Bus
shinyUI(pageWithSidebar( 
  headerPanel(enc2utf8("서울시 저상버스 활용 웹")), 
  sidebarPanel(
    textInput("name", "BUS Number: ", value = "750A"),
    submitButton(enc2utf8("정류장 찾기!")),
    HTML("<br><br><br>"),
    textInput("name2", "Station ID: ", value = ""),
    submitButton(enc2utf8("버스도착시간 찾기!"))
  ),
  mainPanel(
    h2(textOutput('title')),
    verbatimTextOutput('datatext'),
    HTML("<br>"),
    h3(textOutput('title2')),
    verbatimTextOutput('bhtext'),
    verbatimTextOutput('linktext')
    #textOutput(enc2utf8('datatext'))# ,
    #tableOutput('table')
    )      
))
