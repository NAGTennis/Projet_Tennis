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
#load("../Data/table_score.RData")
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # summary
  output$summary <- renderPrint({
    summary(Tennis_table_work)
  })
  
  # table
  output$table <- renderDataTable({
    Tennis_table_work
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
  
  output$nom_j1_bis <- renderText({
    input$go
    isolate({
      input$nom1
    })
  })
  
  output$nom_j2_bis <- renderText({
    input$go
    isolate({
      paste(input$nom2)
    })
  })
  
  output$image_j1 <- renderImage({
    input$go|input$predict
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
    input$go|input$predict
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
    input$go|input$predict
    isolate({
      if (input$tournois!=" ") {
        if (file.exists(paste("../img/tournois/",input$tournois,".png",sep=""))) {
          link=paste("../img/tournois/",input$tournois,".png",sep="")
          link_alt=input$tournois
        }
        else {
          link=paste("../img/tournois/tournois.png",sep="")
          link_alt="Tournois"
        }
      }
      else if (input$surface!=" ") {
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
  
  output$infos_j1 <- renderUI({
    
    input$go
    isolate({
    p1_name=input$nom1
    p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
    p2_name=input$nom2
    p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
    tourney_date=input$date
    tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
    surface=ifelse(input$surface!=" ",input$surface,0)
    age1=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25
    age2=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
    hand1=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',"Droitier","Gaucher")
    hand2=atp_players[Player_Id==p2_id]$Main_Forte
    ht1=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1])
    ht2=ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
    rang1=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]
    rang2=Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
    points1=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]
    points2=Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
    
    tags$ul(
      tags$li(paste("Age : ", age1, sep=""))
      ,tags$li(paste("Taille : ", ht1, sep=""))
      ,tags$li(paste("Main Forte : ", hand1, sep=""))
      ,tags$li(paste("Classement ATP : ", rang1, sep=""))
      ,tags$li(paste("Titres en carrière : ", f_NombreTitre(Tennis_table_work,i_name=p1_name,i_date=tourney_date), sep=""))
      ,tags$li(paste("Titres de la saison : ", f_NombreTitreSaison(Tennis_table_work,i_name=p1_name,i_date=tourney_date), sep=""))
      ,tags$li(paste("Victoires en carrière : ", f_NombreVictoire(Tennis_table_work,i_name=p1_name,i_round=0,i_match_num=0,i_date=tourney_date), sep=""))
      ,tags$li(paste("Victoires de la saison : ", f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p1_name,i_date=tourney_date), sep=""))
    )
    })
  })
  
  output$infos_j2 <- renderUI({
    
    input$go
    isolate({
    p1_name=input$nom1
    p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
    p2_name=input$nom2
    p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
    tourney_date=input$date
    tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
    surface=ifelse(input$surface!=" ",input$surface,0)
    age1=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25
    age2=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
    hand1=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',"Droitier","Gaucher")
    hand2=ifelse(atp_players[Player_Id==p2_id]$Main_Forte=='R',"Droitier","Gaucher")
    ht1=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1])
    ht2=ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
    rang1=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]
    rang2=Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
    points1=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]
    points2=Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
    
    tags$ul(
      tags$li(paste("Age : ", age2, sep=""))
      ,tags$li(paste("Taille : ", ht2, sep=""))
      ,tags$li(paste("Main Forte : ", hand2, sep=""))
      ,tags$li(paste("Classement ATP : ", rang2, sep=""))
      ,tags$li(paste("Titres en carrière : ", f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date), sep=""))
      ,tags$li(paste("Titres de la saison : ", f_NombreTitreSaison(Tennis_table_work,i_name=p2_name,i_date=tourney_date), sep=""))
      ,tags$li(paste("Victoires en carrière : ", f_NombreVictoire(Tennis_table_work,i_name=p2_name,i_round=0,i_match_num=0,i_date=tourney_date), sep=""))
      ,tags$li(paste("Victoires de la saison : ", f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p2_name,i_date=tourney_date), sep=""))
    )
    })
  })
  
  output$donnees_datatable <- renderDataTable({
    input$predict
    isolate({
      req(input$predict)
      p1_name=input$nom1
      p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
      p2_name=input$nom2
      p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
      tourney_date=input$date
      tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
      surface=ifelse(input$surface!=" ",input$surface,0)
      age=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25-as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
      hand=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',1,0)-ifelse(atp_players[Player_Id==p2_id]$Main_Forte=='R',1,0)
      ht=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1]) - ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
      rang=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
      points=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
      
      table_score=data.table(tourney_id=0
                             ,tourney_name=tourney_name
                             ,surface=surface
                             ,tourney_date=tourney_date
                             ,tourney_level=0
                             ,match_num=0
                             ,round=0
                             ,round_num=0
                             ,p1_id=p1_id
                             ,p1_name=p1_name
                             ,p2_id=p2_id
                             ,p2_name=p2_name
                             ,age=age,ht=ht,rank=rang,rank_points=points,hand=hand,seed=0)
      
      table_score[,c("hist_ace","hist_df","hist_1stIn","hist_1stWon","hist_2ndWon","hist_bpSaved","hist_bpMean","hist_bpConverted","hist_svptWon","hist_returnPtWon","hist_1stReturnWon","hist_2ndReturnWon"
                     ,"Dix_ace","Dix_df","Dix_1stIn","Dix_1stWon","Dix_2ndWon","Dix_bpSaved","Dix_bpMean","Dix_bpConverted","Dix_svptWon","Dix_returnPtWon","Dix_1stReturnWon","Dix_2ndReturnWon"
                     ,"h2h_ace","h2h_df","h2h_1stIn","h2h_1stWon","h2h_2ndWon","h2h_bpSaved","h2h_bpMean","h2h_bpConverted","h2h_svptWon","h2h_returnPtWon","h2h_1stReturnWon","h2h_2ndReturnWon"
                     ,"tourn_ace","tourn_df","tourn_1stIn","tourn_1stWon","tourn_2ndWon","tourn_bpSaved","tourn_bpMean","tourn_bpConverted","tourn_svptWon","tourn_returnPtWon","tourn_1stReturnWon","tourn_2ndReturnWon"
                     ,"fatigue"
                     ,"abandon_diff")
                  := c((f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num)-f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num))
                       ,(f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_row=10)-f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_row=10))
                       ,(f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_opponent=p2_name)-f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_opponent=p1_name))
                       ,(f_historique(Tennis_table_work,i_name=p1_name,i_tournament=tourney_name,i_date=tourney_date,i_round=round_num,i_matchnum=match_num)-f_historique(Tennis_table_work,i_name=p2_name,i_tournament=tourney_name,i_date=tourney_date,i_round=round_num,i_matchnum=match_num))
                       ,(f_fatigue(Tennis_table_work,i_name=p1_name,i_date=tourney_date,i_duree=NULL,i_nbmatchs=2,i_round=round,i_matchnum=match_num)[,NbJeuJoue]-f_fatigue(Tennis_table_work,i_name=p2_name,i_date=tourney_date,i_duree=NULL, i_nbmatchs=2, i_round=round, i_matchnum=match_num)[,NbJeuJoue])
                       ,(f_retour_abandon(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_retour_abandon(Tennis_table_work,i_name=p2_name,i_date=tourney_date))
                  )
                  ]
      table_score[,c("Head2Head"):=c(f_Headtohead(Tennis_table_work,i_name = p1_name,i_opponent = p2_name,i_date = tourney_date,i_round=round_num,i_matchnum=match_num))
                  ]
      table_score[,c("TxVictSurface10"):=c(f_VicT(Tennis_table_work,i_name=p1_name,i_opponent=p2_name,i_date = tourney_date,i_round=round_num, i_row=10,i_matchnum=match_num,i_surface=surface))
                  ]
      table_score[,c('NbTitre'):=c(f_NombreTitre(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date))][,c('NbTitreSaisonPrec') := c(f_NombreTitreSaison(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitreSaison(Tennis_table_work,i_name=p2_name,i_date=tourney_date))][,c('NbVictoire') := c(f_NombreVictoire(Tennis_table_work,i_name=p1_name,i_round=round_num,i_match_num=match_num,i_date=tourney_date)-f_NombreVictoire(Tennis_table_work,i_name=p2_name,i_round=round_num,i_match_num=match_num,i_date=tourney_date))][,c('NbVictoireSaisonPrec'):=c(f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p2_name,i_date=tourney_date))][,c('SurfacePred'):=c(f_surface(Tennis_table_work,i_name=p1_name,i_date=tourney_date,i_round=round_num,i_match_num=match_num,i_surface=surface)-f_surface(Tennis_table_work,i_name=p2_name,i_date=tourney_date,i_round=round_num,i_match_num=match_num,i_surface=surface))]
      for (j in seq_len(ncol(table_score)))
        set(table_score,which(is.nan(table_score[[j]])|is.na(table_score[[j]])|is.null(table_score[[j]])),j,0)
      
      return(table_score)
    })
  })

  output$stats_joueurs_10 <- renderAmCharts({
    input$go
    isolate({
    p1_name=input$nom1
    p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
    p2_name=input$nom2
    p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
    tourney_date=input$date
    tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
    surface=ifelse(input$surface!=" ",input$surface,0)
    age=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25-as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
    hand=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',1,0)-ifelse(atp_players[Player_Id==p2_id]$Main_Forte=='R',1,0)
    ht=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1]) - ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
    rang=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
    points=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
    
    tab=data.frame(
      c("Ace","Double faute","1st In","1st Won","2nd Won","BP Saved","BP Mean","BP Converted","Svpt Won","returnPt Won","1stReturn Won","2ndReturn Won")
      , t(f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=0,i_matchnum=0,i_row=10))
      , t(f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=0,i_matchnum=0,i_row=10))
      )
    colnames(tab)=c("categories",input$nom1,input$nom2)
    amBarplot(x = "categories", y = c(input$nom1,input$nom2), data = tab)
    })
  })
  
  output$stats_joueurs_h2h <- renderAmCharts({
    input$go
    isolate({
    p1_name=input$nom1
    p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
    p2_name=input$nom2
    p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
    tourney_date=input$date
    tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
    surface=ifelse(input$surface!=" ",input$surface,0)
    age=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25-as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
    hand=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',1,0)-ifelse(atp_players[Player_Id==p2_id]$Main_Forte=='R',1,0)
    ht=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1]) - ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
    rang=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
    points=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
    
    tab=data.frame(
      c("Ace","Double faute","1st In","1st Won","2nd Won","BP Saved","BP Mean","BP Converted","Svpt Won","returnPt Won","1stReturn Won","2ndReturn Won")
      , t(f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=0,i_matchnum=0,i_opponent=p2_name))
      , t(f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=0,i_matchnum=0,i_opponent=p1_name))
    )
    colnames(tab)=c("categories",input$nom1,input$nom2)
    amBarplot(x = "categories", y = c(input$nom1,input$nom2), data = tab)
    })
  })
  
  output$stats_joueurs_Tourn <- renderAmCharts({
    input$go
    isolate({
    p1_name=input$nom1
    p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
    p2_name=input$nom2
    p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
    tourney_date=input$date
    tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
    surface=ifelse(input$surface!=" ",input$surface,0)
    age=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25-as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
    hand=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',1,0)-ifelse(atp_players[Player_Id==p2_id]$Main_Forte=='R',1,0)
    ht=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1]) - ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
    rang=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
    points=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
    
    tab=data.frame(
      c("Ace","Double faute","1st In","1st Won","2nd Won","BP Saved","BP Mean","BP Converted","Svpt Won","returnPt Won","1stReturn Won","2ndReturn Won")
      , t(f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_tournament=tourney_name,i_date=tourney_date,i_round=0,i_matchnum=0))
      , t(f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_tournament=tourney_name,i_date=tourney_date,i_round=0,i_matchnum=0))
    )
    colnames(tab)=c("categories",input$nom1,input$nom2)
    amBarplot(x = "categories", y = c(input$nom1,input$nom2), data = tab)
    })
  })
  
  output$stats_resultat <- renderAmCharts({
    input$predict
    isolate({
      p1_name=input$nom1
      p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
      p2_name=input$nom2
      p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
      tourney_date=input$date
      tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
      surface=ifelse(input$surface!=" ",input$surface,0)
      age=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25-as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
      hand=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',1,0)-ifelse(atp_players[Player_Id==p2_id]$Main_Forte=='R',1,0)
      ht=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1]) - ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
      rang=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
      points=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
      
      tab=data.frame(
        c("Tx de victoire Head to Head")
        
        , f_Headtohead(Tennis_table_work,i_name = p1_name,i_opponent = p2_name,i_date = tourney_date,i_round=0,i_matchnum=0)
      )
      colnames(tab)=c("categories",input$nom1)
      amBarplot(x = "categories", y = c(input$nom1), data = tab)
    })
  })
  
  proba <- eventReactive(input$predict, {
      p1_name=input$nom1
      p1_id=ifelse(length(Tennis_table_work[w_name==p1_name]$w_id)>0,Tennis_table_work[w_name==p1_name]$w_id[1],Tennis_table_work[l_name==p1_name]$l_id[1])
      p2_name=input$nom2
      p2_id=ifelse(length(Tennis_table_work[w_name==p2_name]$w_id)>0,Tennis_table_work[w_name==p2_name]$w_id[1],Tennis_table_work[l_name==p2_name]$l_id[1])
      tourney_date=input$date
      tourney_name=ifelse(input$tournois!=" ",input$tournois,0)
      surface=ifelse(input$surface!=" ",input$surface,0)
      age=as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p1_id]$DateNaissance)))/365.25-as.numeric(difftime(anydate(tourney_date),anydate(atp_players[Player_Id==p2_id]$DateNaissance)))/365.25
      hand=ifelse(atp_players[Player_Id==p1_id]$Main_Forte=='R',1,0)-ifelse(atp_players[Player_Id==p2_id]$Main_Forte=='R',1,0)
      ht=ifelse(length(Tennis_table_work[w_name==p1_name]$w_ht)>0,Tennis_table_work[w_name==p1_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p1_name,.(ht=l_ht)][order(-ht)]$ht[1]) - ifelse(length(Tennis_table_work[w_name==p2_name]$w_ht)>0,Tennis_table_work[w_name==p2_name,.(ht=w_ht)][order(-ht)]$ht[1],Tennis_table_work[l_name==p2_name,.(ht=l_ht)][order(-ht)]$ht[1])
      rang=Rank[Player_Id==p1_id][order(-DateRanking)]$Numero[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Numero[1]
      points=Rank[Player_Id==p1_id][order(-DateRanking)]$Points[1]-Rank[Player_Id==p2_id][order(-DateRanking)]$Points[1]
      
      table_score=data.table(tourney_id=0
                             ,tourney_name=tourney_name
                             ,surface=surface
                             ,tourney_date=tourney_date
                             ,tourney_level=0
                             ,match_num=0
                             ,round=0
                             ,round_num=0
                             ,p1_id=p1_id
                             ,p1_name=p1_name
                             ,p2_id=p2_id
                             ,p2_name=p2_name
                             ,age=age,ht=ht,rank=rang,rank_points=points,hand=hand,seed=0)
      
      table_score[,c("hist_ace","hist_df","hist_1stIn","hist_1stWon","hist_2ndWon","hist_bpSaved","hist_bpMean","hist_bpConverted","hist_svptWon","hist_returnPtWon","hist_1stReturnWon","hist_2ndReturnWon"
                     ,"Dix_ace","Dix_df","Dix_1stIn","Dix_1stWon","Dix_2ndWon","Dix_bpSaved","Dix_bpMean","Dix_bpConverted","Dix_svptWon","Dix_returnPtWon","Dix_1stReturnWon","Dix_2ndReturnWon"
                     ,"h2h_ace","h2h_df","h2h_1stIn","h2h_1stWon","h2h_2ndWon","h2h_bpSaved","h2h_bpMean","h2h_bpConverted","h2h_svptWon","h2h_returnPtWon","h2h_1stReturnWon","h2h_2ndReturnWon"
                     ,"tourn_ace","tourn_df","tourn_1stIn","tourn_1stWon","tourn_2ndWon","tourn_bpSaved","tourn_bpMean","tourn_bpConverted","tourn_svptWon","tourn_returnPtWon","tourn_1stReturnWon","tourn_2ndReturnWon"
                     ,"fatigue"
                     ,"abandon_diff")
                  := c((f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num)-f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num))
                       ,(f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_row=10)-f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_row=10))
                       ,(f_historique(Tennis_table_work,i_name=p1_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_opponent=p2_name)-f_historique(Tennis_table_work,i_name=p2_name,i_surface=surface,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_opponent=p1_name))
                       ,(f_historique(Tennis_table_work,i_name=p1_name,i_tournament=tourney_name,i_date=tourney_date,i_round=round_num,i_matchnum=match_num)-f_historique(Tennis_table_work,i_name=p2_name,i_tournament=tourney_name,i_date=tourney_date,i_round=round_num,i_matchnum=match_num))
                       ,(f_fatigue(Tennis_table_work,i_name=p1_name,i_date=tourney_date,i_duree=NULL,i_nbmatchs=2,i_round=round,i_matchnum=match_num)[,NbJeuJoue]-f_fatigue(Tennis_table_work,i_name=p2_name,i_date=tourney_date,i_duree=NULL, i_nbmatchs=2, i_round=round, i_matchnum=match_num)[,NbJeuJoue])
                       ,(f_retour_abandon(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_retour_abandon(Tennis_table_work,i_name=p2_name,i_date=tourney_date))
                  )
                  ]
      table_score[,c("Head2Head"):=c(f_Headtohead(Tennis_table_work,i_name = p1_name,i_opponent = p2_name,i_date = tourney_date,i_round=round_num,i_matchnum=match_num))
                  ]
      table_score[,c("TxVictSurface10"):=c(f_VicT(Tennis_table_work,i_name=p1_name,i_opponent=p2_name,i_date = tourney_date,i_round=round_num, i_row=10,i_matchnum=match_num,i_surface=surface))
                  ]
      table_score[,c('NbTitre'):=c(f_NombreTitre(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date))][,c('NbTitreSaisonPrec') := c(f_NombreTitreSaison(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitreSaison(Tennis_table_work,i_name=p2_name,i_date=tourney_date))][,c('NbVictoire') := c(f_NombreVictoire(Tennis_table_work,i_name=p1_name,i_round=round_num,i_match_num=match_num,i_date=tourney_date)-f_NombreVictoire(Tennis_table_work,i_name=p2_name,i_round=round_num,i_match_num=match_num,i_date=tourney_date))][,c('NbVictoireSaisonPrec'):=c(f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p2_name,i_date=tourney_date))][,c('SurfacePred'):=c(f_surface(Tennis_table_work,i_name=p1_name,i_date=tourney_date,i_round=round_num,i_match_num=match_num,i_surface=surface)-f_surface(Tennis_table_work,i_name=p2_name,i_date=tourney_date,i_round=round_num,i_match_num=match_num,i_surface=surface))]
      
      table_score[,c("tourney_id","tourney_name","surface","tourney_date","tourney_level","match_num","round","round_num","p1_id","p1_name","p2_id","p2_name") := NULL]
      
      for (j in seq_len(ncol(table_score)))
        set(table_score,which(is.nan(table_score[[j]])|is.na(table_score[[j]])|is.null(table_score[[j]])),j,0)
      
      
      rf_pred=predict(rf,table_score,type='prob')
      
      return(rf_pred[,2])
    })
  
  output$proba <- renderText({
    ifelse(proba()>=0.5,proba(),1-proba())
  })
  
  
  
  output$winner_img <- renderImage({
    input$predict
    isolate({
      if (proba()>=0.5) {
        if (file.exists(paste("../img/joueurs/",input$nom1,".png",sep=""))) {
          link=paste("../img/joueurs/",input$nom1,".png",sep="")
        }
        else {
          link=paste("../img/joueurs/ghost.png",sep="")
        }
      }
      else {
        if (file.exists(paste("../img/joueurs/",input$nom2,".png",sep=""))) {
          link=paste("../img/joueurs/",input$nom2,".png",sep="")
        }
        else {
          link=paste("../img/joueurs/ghost.png",sep="")
        }
      }
      return(list(
        src = link,
        alt="winner",
        width='100%',
        height='auto'
      ))
    })
  }, deleteFile = FALSE)
  
  output$winner_name <- renderText({
    input$predict
    isolate({
      if (proba()>=0.5) {
        link=input$nom1
      }
      else {
        link=input$nom2
      }
      return(
        link
      )
    })
  })
  
  output$model_proba <- renderText({
    input$models
    isolate({
      req(input$models)
      if (input$models=="Régression logistique") {
        # err <- performance(ROCR_pred["RegLog"]$RegLog, measure = "err") 
        auc <- performance(ROCR_pred["RegLog"]$RegLog, measure = "auc")@y.values[[1]]
      }
      else if (input$models=="Ridge") { 
        # err <- performance(ROCR_pred["Ridge1"]$Ridge1, measure = "err") 
        auc <- performance(ROCR_pred["Ridge1"]$Ridge1, measure = "auc")@y.values[[1]]
      }
      else if (input$models=="Lasso") {
        # err <- performance(ROCR_pred["Lasso"]$Lasso, measure = "err") 
        auc <- performance(ROCR_pred["Lasso"]$Lasso, measure = "auc")@y.values[[1]]
      }
      else if (input$models=="Elasticnet") { 
        # err <- performance(ROCR_pred["ElNet"]$ElNet, measure = "err") 
        auc <- performance(ROCR_pred["ElNet"]$ElNet, measure = "auc")@y.values[[1]]
      }
      else if (input$models=="XGBoost") { 
        # err <- performance(ROCR_pred["XGBoost"]$XGBoost, measure = "err") 
        auc <- performance(ROCR_pred["XGBoost"]$XGBoost, measure = "auc")@y.values[[1]]
      }
      else if (input$models=="Random Forest") {
        # err <- performance(ROCR_pred["rf"]$rf, measure = "err") 
        auc <- performance(ROCR_pred["rf"]$rf, measure = "auc")@y.values[[1]]
      }
      # paste("Taux d'erreur :",err)
      paste("AUC :",auc)
    })
  })
  
  output$plot <- renderPlot({
    input$models
    isolate({
      req(input$models)
      if (input$models=="Régression logistique") {
        perf <- performance(ROCR_pred["RegLog"]$RegLog, measure = "tpr", x.measure = "fpr") 
        plot(perf, col=rainbow(10))
      }
      else if (input$models=="Ridge") {
        perf <- performance(ROCR_pred["Ridge1"]$Ridge1, measure = "tpr", x.measure = "fpr") 
        plot(perf, col=rainbow(10))
      }
      else if (input$models=="Lasso") {
        perf <- performance(ROCR_pred["Lasso"]$Lasso, measure = "tpr", x.measure = "fpr") 
        plot(perf, col=rainbow(10))
      }
      else if (input$models=="Elasticnet") {
        perf <- performance(ROCR_pred["ElNet"]$ElNet, measure = "tpr", x.measure = "fpr") 
        plot(perf, col=rainbow(10))
      }
      else if (input$models=="XGBoost") {
        perf <- performance(ROCR_pred["XGBoost"]$XGBoost, measure = "tpr", x.measure = "fpr") 
        plot(perf, col=rainbow(10))
      }
      else if (input$models=="Random Forest") {
        perf <- performance(ROCR_pred["rf"]$rf, measure = "tpr", x.measure = "fpr") 
        plot(perf, col=rainbow(10))
      }
    })
  })
})
