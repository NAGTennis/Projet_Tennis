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
shinyServer(function(input, output, session) {
  
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
  
  output$nom_j1 <- renderText({
    input$go
    isolate({
      input$nom1
    })
  })
  
  output$nom_j2 <- renderText({
    input$go
    isolate({
      paste(input$nom2)
    })
  })
  
  output$image_j1 <- renderImage({
    input$go
    isolate({
      if (file.exists(paste("../img/joueurs/",input$nom1,".png",sep=""))) {
        link=paste("../img/joueurs/",input$nom1,".png",sep="")
      }
      else {
        link=paste("../img/joueurs/ghost.png",sep="")
      }
      return(list(
        src = link,
        alt=input$nom1,
        width='100%',
        height='auto'
      ))
    })
  }, deleteFile = FALSE)
  
  output$image_j2 <- renderImage({
    input$go
    isolate({
      if (file.exists(paste("../img/joueurs/",input$nom2,".png",sep=""))) {
        link=paste("../img/joueurs/",input$nom2,".png",sep="")
      }
      else {
        link=paste("../img/joueurs/ghost.png",sep="")
      }
      return(list(
        src = link,
        alt=input$nom2,
        width='100%',
        height='auto'
      ))
    })
  }, deleteFile = FALSE)
  
  output$image_tournoi <- renderImage({
    input$go
    isolate({
      if (file.exists(paste("../img/tournois/",input$tournois,".png",sep=""))) {
        link=paste("../img/tournois/",input$tournois,".png",sep="")
      }
      else {
        link=paste("../img/tournois/tournois.png",sep="")
      }
      return(list(
        src = link,
        alt=input$nom1,
        width='100%',
        height='auto'
      ))
    })
  }, deleteFile = FALSE)
  
  output$image_surface <- renderImage({
    input$go
    isolate({
      if (file.exists(paste("../img/surface/",input$surface,".png",sep=""))) {
        link=paste("../img/surface/",input$surface,".png",sep="")
      }
      else {
        link=paste("../img/surface/surface.png",sep="")
      }
      return(list(
        src = link,
        alt=input$surface,
        width='100%',
        height='auto'
      ))
    })
  }, deleteFile = FALSE)
  
  output$image_surface_tournois <- renderImage({
    input$go
    isolate({
      if (input$tournois!="") {
        if (file.exists(paste("../img/tournois/",input$tournois,".png",sep=""))) {
          link=paste("../img/tournois/",input$tournois,".png",sep="")
          link_alt=input$tournois
        }
        else {
          link=paste("../img/tournois/tournois.png",sep="")
          link_alt="Tournois"
        }
      }
      else if (input$surface!="") {
        if (file.exists(paste("../img/surface/",input$surface,".png",sep=""))) {
          link=paste("../img/surface/",input$surface,".png",sep="")
          link_alt=input$surface
        }
        else {
          link=paste("../img/surface/surface.png",sep="")
          link_alt="Surface"
        }
      }
      else {
        link=""
        link_alt=""
      }
      return(list(
        src = link,
        alt= link_alt,
        width='100%',
        height='auto'
      ))
    })
  }, deleteFile = FALSE)
  
  observeEvent(input$surface, {
    req(input$surface)
    choices <- if (input$surface == " ") c(" ", unique(Tennis_table[tourney_date>'20170101',.(tourney_name)])) else
      c(" ", unique(Tennis_table[tourney_date>'20170101' & surface == input$surface]$tourney_name))
    updateSelectInput(session, "tournois",
                      choices = choices, selected = input$tournois)
  })
  observeEvent(input$tournois, {
    req(input$tournois)
    choices <- if (input$tournois == " ") c(" ", unique(Tennis_table[tourney_date>'20170101',.(surface)])) else
      c(" ", unique(Tennis_table[tourney_date>'20170101'&tourney_name == input$tournois]$surface))
    updateSelectInput(session, "surface",
                      choices = choices, selected = input$surface)
  })
  
  
})