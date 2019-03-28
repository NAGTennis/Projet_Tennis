#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
setkey(Rank,Player_Id)
setkey(atp_players,Player_Id)
Joueurs_actif <- unique(Rank[DateRanking>=20180101&Numero<=100,.(Player_Id)][atp_players,.(nom=paste(Prenom, Nom)),nomatch=0])

Type_surfaces <- c(" ",unique(Tennis_table[tourney_date>'20170101',.(surface)]))
Nom_tournois <- c(" ",unique(Tennis_table[tourney_date>'20170101',.(tourney_name)]))
models <- c("","Régression logistique", "Ridge", "Lasso", "Elasticnet", "XGBoost", "Random Forest")

don <- table_score %>% select(-p2_name,-p1_name,-round,-tourney_level,-tourney_date,
                              -tourney_name,-tourney_id,-surface,-coin,-p1_id,-p2_id,-round_num,-match_num)

res_pca <- PCA(don, graph = FALSE)
var <- get_pca_var(res_pca)

shinyUI(dashboardPage(skin="green",
                      
                      dashboardHeader(
                        title="Soutenance projet Tennis",
                        titleWidth = 450
                        
                        
                      ),
                      
                      
                      
                      
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Presentation", tabName = "Presentation",icon = icon("file-text"), 
                                   
                                   menuSubItem("Objectif",
                                               tabName="Objectif",
                                               icon=icon("tasks")
                                   ),
                                   
                                   menuSubItem("Données",
                                               tabName = "Données",
                                               icon=icon("table")
                                   )
                          ),
                          
                          menuItem("Description", tabName = "Description", icon = icon("area-chart"),
                                   menuSubItem("Graphiques",
                                               tabName="Graphiques",
                                               icon=icon("pie-chart")
                                   ),
                                   
                                   menuSubItem("ACP",
                                               tabName = "ACP",
                                               icon=icon("search-plus")
                                   )
                          ),
                          menuItem("Modelisation", tabName = "Modelisation", icon = icon("gears")),
                          menuItem("Application", tabName = "Application", icon = icon("play"))
                          
                          # ,tags$head(tags$style(HTML('.content-wrapper { height: 1200px;}')))
                          
                          
                          
                          
                          
                        )
                      ),
                      
                      
                      dashboardBody(
                        tabItems(
                          
                          tabItem(tabName = "Objectif",
                                  h1("Contexte et objectif"),
                                  br(),
                                  h4("Dans le cadre de la formation Data Scientist assurée par l'Ensae, nous avons décidé de réaliser un projet de data-science dans le domaine du sport, plus particulièrement celui du tennis."),
                                  h4("Le but de ce projet est d'essayer de",HTML("<strong><u>prédire au mieux l'issue d'un match de tennis opposant deux joueurs du circuit ATP.</u></strong>")),
                                  h4("Les étapes de la réalisation de ce projet ainsi que les nouvelles compétences utilisées peuvent être synthétisées de la façon suivante : "),
                                  br(),
                                  br(),
                                  br(img(src="SchemasV2.png",width = 800, align = "center")),
                                  br()
                                  
                                  
                          ),
                          
                          tabItem(tabName = "Données",
                                  h1("Les donnees à disposition"),
                                  br(h3("Dans un premier temps, nous disposions de 3 jeux de donnees : ")),
                                  br(img(src="NB1.png",width = 800, align = "center")),
                                  br(h3("Retraitement : ")),
                                  br(img(src="NB2.png",width = 600, align = "center"))
                                  
                                  
                          ),
                          
                          tabItem(tabName = "Graphiques",
                                  
                                  fluidRow(
                                    mainPanel(
                                      h1("Exploration des donnees"),
                                      br(),
                                      
                                      
                                      
                                      tabBox(
                                        title = "Quelques elements descriptifs",
                                        # The id lets us use input$tabset1 on the server to find the current tab
                                        id = "tabset1", height = "600px",width = 12,
                                        tabPanel("Pays", amChartsOutput("chart_pays")),
                                        tabPanel("Pays", amChartsOutput("chart_vict_pays")),
                                        tabPanel("Surface", amChartsOutput("chart_surface")),
                                        tabPanel("Fatigue", amChartsOutput("chart_fatigue")),
                                        tabPanel("Age", amChartsOutput("chart_vict_age"))
                                      )
                                    )
                                  )
                                  
                                  # mainPanel(
                                  #   h1("Quelques éléments descritpifs"),
                                  #   br("blabla"),
                                  #   box(width = 12,
                                  #       title = "Nationalité des joueurs",
                                  #       solidHeader = TRUE,collapsible = TRUE, background = NULL, collapsed = TRUE,
                                  #       fluidRow(
                                  #         column(width=6,amChartsOutput("chart_pays"))
                                  #       ,column(width=6,amChartsOutput("chart_vict_pays"))
                                  #       )  
                                  #   ),
                                  #   box(width = 12,
                                  #       title = "Surface de jeu",
                                  #       solidHeader = TRUE,collapsible = TRUE, background = NULL, collapsed = TRUE,
                                  #       amChartsOutput("chart_surface")
                                  #       
                                  #   ),
                                  #   box(width = 12,
                                  #       title = "Fatigue des joueurs",
                                  #       solidHeader = TRUE,collapsible = TRUE, background = NULL, collapsed = TRUE,
                                  #       amChartsOutput("chart_fatigue")
                                  #       
                                  #   )
                                  #   ,
                                  #   box(width = 12,
                                  #       title = "Age des joueurs",
                                  #       solidHeader = TRUE,collapsible = TRUE, background = NULL, collapsed = TRUE,
                                  #       amChartsOutput("chart_vict_age")
                                  #       
                                  #   ) 
                                  # )
                                  
                                  
                                  
                          ),
                          tabItem(tabName = "ACP",
                                  
                                  fluidRow(
                                    mainPanel(
                                      h2("Analyse en Composantes Principales"),
                                      br(),
                                      h4("Première exploitation des variables"),
                                      br(),
                                      
                                      tabBox(
                                        title = "Resultats ACP",
                                        height = "600px",width = 12,
                                        
                                        tabPanel("Valeurs propres", plotOutput("valpropre")),
                                        tabPanel("Individus", plotOutput("ind")),
                                        tabPanel("Ensemble", plotOutput("both"))
                                      )
                                    )
                                  ) 
                          ),
                          
                          tabItem(tabName="Modelisation",
                                  h1("Démarche"),
                                  br(),
                                  br(img(src="NB4.png",width = 600, align = "center")),
                                  br(),
                                  h1("Résultats"),
                                  h1("Exploitation du modèle retenu")
                                  
                                  
                                  
                          ),
                          
                          tabItem(tabName = "Application",
                                  fluidRow(
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
                                                                  fluidRow(style='height:auto',
                                                                           tabsetPanel(
                                                                             tabPanel("Match",
                                                                                      wellPanel(style = "background-color: #ffffff;"
                                                                                                ,splitLayout(cellWidths = c("40%", "20%", "40%"),
                                                                                                  textOutput("nom_j1")
                                                                                                  ,HTML("<div style='text-align:center; font-size: 20px'>contre</div>")
                                                                                                  ,textOutput("nom_j2")
                                                                                                )
                                                                                                ,
                                                                                                splitLayout(cellWidths = c("40%", "20%", "40%"),align='middle'
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
                                                               
                                                               ,splitLayout(align='middle',cellWidths = c("70%", "30%")
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
                                                                        h3("Variables explicatives (Top 5) :",style = "color : #0099ff")
                                                                        ,uiOutput("variable_resultat")
                                                                      )
                                                                    )
                                                                    ,imageOutput("winner_img")
                                                               )
                                                             )
                                           )
                                    )
                                    
                                    
                                  )
                                  
                          )
                          
                          
                          
                        )
                        #CSS
                        ,tags$style(type = 'text/css', '#nom_j1, #nom_j2,#nom_j1_bis, #nom_j2_bis{color: #0099ff;font-size: 20px;text-align:center;overflow: hidden}')
                        ,tags$style(type = 'text/css', '#image_j1, #image_j2 {max-width: 300px}')
                        ,tags$style(type = 'text/css', '#winner_img{max-width: 300px}')
                        ,tags$style(type = 'text/css', '.shiny-split-layout>div {vertical-align: middle;}')
                        ,tags$style(type = 'text/css', '#image_surface_tournois{max-width: 200px}')
                      )
                      
                      
                      
                      
)

)

