#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
######Ne plas oublier d'importer les tables de données
setkey(Rank,Player_Id)
setkey(atp_players,Player_Id)
Joueurs_actif <- unique(Rank[DateRanking>=20180101&Numero<=100,.(Player_Id)][atp_players,.(nom=paste(Prenom, Nom)),nomatch=0])
Type_surfaces <- c(" ",unique(Tennis_table[tourney_date>'20170101',.(surface)]))
Nom_tournois <- c(" ",unique(Tennis_table[tourney_date>'20170101',.(tourney_name)]))
tags$head(tags$link(rel = "stylesheet",type = "text/css", href = "./style.css"))

shinyUI(
  
  # navbarPage
  navbarPage("Projet Tennis",
             
             tabPanel("Présentation",
                      "Objectif : Donner le gagnant d'un match de tennis"
                      ),
             
             
             
             
             # premier onglet Data
             tabPanel("Données", 
                      navlistPanel(
                        widths = c(2, 10), 
                        tabPanel("Table", 
                                 # titre avec css
                                 h1("Jeu de données", style = "color : #0099ff;text-align:center"),
                                 # table
                                 dataTableOutput("table")),
                        tabPanel("Résumé",h1("Résumé des données", style = "color : #0099ff;text-align:center"),verbatimTextOutput("summary"))
                      )
             ), 
             
             # second onglet Visualisation
             tabPanel("Application", 
                      
                      fluidRow(
                        # premier colonne
                        column(width = 3, 
                               # wellPanel pour griser
                               wellPanel(
                                 # Nom du joueur 1
                                 selectizeInput(inputId = "nom1", label = "Nom du Joueur 1",choices = Joueurs_actif, options=list(create=FALSE)),
                                 # Nom du joueur 2
                                 selectizeInput(inputId = "nom2", label = "Nom du Joueur 2",choices = Joueurs_actif, options=list(create=FALSE)),
                                 # Type de Surface
                                 selectInput(inputId = "surface", label = "Surface",choices = Type_surfaces),
                                 # Type de Tournois
                                 selectInput(inputId = "tournois", label = "Tournois",choices = Nom_tournois),
                                 # Type de surface dynamique
                                 #htmlOutput("surface_select"),
                                 # Type de tournois dynamique
                                 #htmlOutput("tournois_select"),
                                 #Date du match
                                 dateInput(inputId = "date", label = "Date du match", value = Sys.Date(), format= "dd/mm/yyyy",language="French"),
                                 # bouton
                                 actionButton("go", "Valider")
                               )
                        )
                        ,
                        mainPanel(
                          wellPanel(fluidRow(height='500px',
                                             splitLayout(
                                               textOutput("nom_j1")
                                               ,HTML("<div style='text-align:center; font-size: 18px'>contre</div>")
                                               ,textOutput("nom_j2")
                                             )
                                             ,
                                             splitLayout(align='middle'
                                                         ,imageOutput("image_j1")
                                                         ,imageOutput("image_surface_tournois")
                                                         ,imageOutput("image_j2")
                                             )
                          )
                          ))
                      )
                      
             ),
             
             # onglet About
             tabPanel("About",
                      "Projet réalisé par Nardjesse, Greg et Axel."
             )
             
             #CSS
             ,tags$style(type = 'text/css', '#nom_j1, #nom_j2{color: #0099ff;font-size: 18px;text-align:center;overflow: hidden}')
             ,tags$style(type = 'text/css', '.image_j1, .image_j2{height:auto}')
             #,tags$style(type = 'text/css', '#image_surface_tournois {display: table-cell; vertical-align: middle; text-align:center; width: 33.333%; height: auto}')
             ,tags$style(type = 'text/css', '.shiny-split-layout>div {vertical-align: middle;}')
  )
)
