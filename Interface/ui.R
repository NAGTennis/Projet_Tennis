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
models <- c("","Régression logistique", "Ridge", "Lasso", "Elasticnet", "XGBoost", "Random Forest")

# rmdfiles <- c("../Presentation.Rmd")
# sapply(rmdfiles, knit, quiet = T)
shinyUI(
  
  # navbarPage
  navbarPage(theme=shinytheme("flatly"),
             "Prédictions ATP",
             # Onglet Présentation
             tabPanel("Présentation", ""
                  # withMathJax(includeMarkdown("Presentation.md"))
             ), 
             
             # Onlget Modélisation
             tabPanel("Modélisation", 
                  fluidRow(
                    # premier colonne
                    column(width = 4, 
                           # wellPanel pour griser
                           wellPanel(
                             h3("Modélisations", style = "color : #0099ff"),
                             # Modèles
                             selectInput(inputId = "models", label = "Modèles",choices = models)
                             ,textOutput("model_proba")
                           )
                    )
                    ,column(width = 6 , plotOutput("plot"))
                    
                  )
             ), 
             
             # Onglet Application
             tabPanel("Application", 
                      
                      fluidRow(
                        # premier colonne
                        column(width = 3, 
                               # wellPanel pour griser
                               wellPanel(
                                 h3("Caractéristiques du match à prédire", style = "color : #0099ff"),
                                 # Nom du joueur 1
                                 selectizeInput(inputId = "nom1", label = "Nom du Joueur 1",choices = Joueurs_actif, options=list(create=FALSE)),
                                 # Nom du joueur 2
                                 selectizeInput(inputId = "nom2", label = "Nom du Joueur 2",choices = Joueurs_actif, options=list(create=FALSE)),
                                 # Type de Surface
                                 selectInput(inputId = "surface", label = "Surface",choices = Type_surfaces),
                                 # Type de Tournois
                                 selectInput(inputId = "tournois", label = "Tournois",choices = Nom_tournois),
                                 #Date du match
                                 dateInput(inputId = "date", label = "Date du match", value = Sys.Date(), format= "dd/mm/yyyy",language="French"),
                                 # bouton
                                 actionButton("go", "Valider")
                               )
                        )
                        ,conditionalPanel(condition ="input.go != ''"
                          ,column(width = 9,
                            wellPanel(
                              fluidRow(height='auto',
                                tabsetPanel(
                                  tabPanel("Match",
                                    wellPanel(style = "background-color: #ffffff;"
                                      ,splitLayout(
                                         textOutput("nom_j1")
                                         ,HTML("<div style='text-align:center; font-size: 20px'>contre</div>")
                                         ,textOutput("nom_j2")
                                       )
                                       ,
                                        splitLayout(align='middle'
                                          ,imageOutput("image_j1")
                                          ,imageOutput("image_surface_tournois")
                                          ,imageOutput("image_j2")
                                        )
                                    )
                                  )
                                  ,tabPanel("Statistiques", 
                                    wellPanel(style = "background-color: #ffffff;"
                                      ,splitLayout(
                                        textOutput("nom_j1_bis")
                                        ,textOutput("nom_j2_bis")
                                      )
                                      ,splitLayout(
                                        uiOutput("infos_j1")
                                        ,uiOutput("infos_j2")
                                      )
                                    ) 
                                  )
                                  ,tabPanel("Forme du moment", 
                                    wellPanel(style = "background-color: #ffffff;"
                                      ,fluidRow(height='auto',
                                        amChartsOutput("stats_joueurs_10")
                                      )
                                    ) 
                                  )
                                  ,tabPanel("Tournois", 
                                    wellPanel(style = "background-color: #ffffff;"
                                      ,fluidRow(height='auto',
                                        amChartsOutput("stats_joueurs_Tourn")
                                      )
                                    ) 
                                  )
                                  ,tabPanel("Head to Head", 
                                    wellPanel(style = "background-color: #ffffff;"
                                      ,fluidRow(height='auto',
                                        amChartsOutput("stats_joueurs_h2h")
                                      )
                                    ) 
                                  )
                                )
                                ,
                                # bouton de prédiction
                                splitLayout(align='middle'
                                  ,actionButton("predict", "Prédire",width='33.333%',style='font-size:133%')
                                )
                              )
                            )
                          )
                        )
                      )
                      ,
                      column(width=12  
                        # ,wellPanel(
                        #   h2("Résumé des informations du match",style = "color : #0099ff;text-align:center")
                        #   ,dataTableOutput("donnees_datatable")
                        #   ,verbatimTextOutput("proba")
                        # )
                        ,conditionalPanel(condition ="input.predict != ''"
                          ,wellPanel(
                              h1("Résultat du Match",style = "color : #0099ff;text-align:center")
                              ,splitLayout(align='middle'
                                ,verticalLayout(
                                  splitLayout(
                                    h3("Vainqueur :",style = "color : #0099ff")
                                    ,h3(textOutput("winner_name"))
                                  )
                                  ,splitLayout(
                                    h3("Probabilité :",style = "color : #0099ff")
                                    ,h3(textOutput("proba"))
                                  )
                                  ,splitLayout(
                                    amChartsOutput("stats_resultat")
                                  )
                                )
                                ,imageOutput("winner_img")
                              )
                          )
                        )
                      )
             ),
             
             # onglet About
             tabPanel("About",
                      "Projet réalisé par Nardjesse, Greg et Axel."
             )
             
             #CSS
             ,tags$style(type = 'text/css', '#nom_j1, #nom_j2,#nom_j1_bis, #nom_j2_bis{color: #0099ff;font-size: 20px;text-align:center;overflow: hidden}')
             ,tags$style(type = 'text/css', '#image_j1, #image_j2 {max-width: 300px}')
             ,tags$style(type = 'text/css', '#winner_img{max-width: 400px}')
             ,tags$style(type = 'text/css', '.shiny-split-layout>div {vertical-align: middle;}')
             ,tags$style(type = 'text/css', '#image_surface_tournois{max-width: 200px}')
             
             )
)
