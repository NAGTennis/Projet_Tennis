#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
load("../Data/table_score.RData")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # summary
  output$summary <- renderPrint({
    summary(table_score)
  })
  
  # table
  output$table <- renderDataTable({
    table_score
  })
  
  output$match_resume <- renderText({
    input$go
    isolate({
      noms=paste(input$nom1, " contre ", input$nom2)
      surface=ifelse(input$surface!="",paste(" sur ", input$surface),"")
      lieu=ifelse(input$tournois!="",paste(" a ", input$tournois),"")
      paste(noms,surface,lieu, " le " , input$date)
    })
  })
  output$nom1 <- renderImage({
    input$go
    isolate({
      if (file.exists(paste("../img/",str_to_lower(input$nom1),".png",sep=""))) {
        link=paste("../img/",str_to_lower(input$nom1),".png",sep="")
      }
      else {
        link=paste("../img/ghost.png",sep="")
      }
      return(list(
        src = link,
        alt=input$nom1
      ))
    })
  }, deleteFile = FALSE)
  
  output$nom2 <- renderImage({
    input$go
    isolate({
      if (file.exists(paste("../img/",str_to_lower(input$nom2),".png",sep=""))) {
        link=paste("../img/",str_to_lower(input$nom2),".png",sep="")
      }
      else {
        link=paste("../img/ghost.png",sep="")
      }
      return(list(
        src = link,
        alt=input$nom2
      ))
    })
  }, deleteFile = FALSE)
  
})
