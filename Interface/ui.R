#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
Joueurs_actif <- unique(Rank[DateRanking>=20180101&Numero<=100,.(Player_Id)][atp_players,.(nom=paste(Prenom, Nom)),nomatch=0])
Type_surfaces <- c("",unique(Tennis_table[tourney_date>'20170101',.(surface)]))
Nom_tournois <- c("",unique(Tennis_table[tourney_date>'20170101',.(tourney_name)]))
shinyUI(
  # navbarPage
  navbarPage("Prédictions ATP",
             
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
                                 # Type de Surface
                                 selectInput(inputId = "tournois", label = "Tournois",choices = Nom_tournois),
                                 #Date du match
                                 dateInput(inputId = "date", label = "Date du match", value = Sys.Date(), format= "dd/mm/yyyy",language="French"),
                                 # bouton
                                 actionButton("go", "Valider")
                               )
                        )
                        ,
                        mainPanel(
                          splitLayout(
                            imageOutput("nom1"),
                            textOutput("match_resume"),
                            imageOutput("nom2")
                          )
                        )
                      )
                      
             ),
             # onglet sur la societe
             tabPanel("About",
                      "Projet réalisé par Nardjesse, Greg et Axel."
             )
             
             
  )
)
