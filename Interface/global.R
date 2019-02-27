###############################
#### Packages
#install.packages("data.table")
library(data.table)
#install.packages("stringr")
library(stringr)
#install.packages("sqldf")
library(sqldf)
#install.packages("knitr")
library(knitr)
#install.packages("anytime")
library(anytime)
#install.packages("knitr")
library(knitr)
#install.packages('dplyr')
library(dplyr)
#install.packages('dtplyr')
library(dtplyr)
library(randomForest)
library(ggplot2)
###############################
#### Fonctions
f_replaceNA = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}
###f_historique renvoi les statistiques moyennes d'un joueur en fonctions de différentes variables
### Variable obligatoire :
####Tennis_table_work = table d'historique des matchs, doit contenir les colonnes :  "tourney_id"     "tourney_name"   "surface"        "tourney_date"   "tourney_level"  "match_num"      "w_id"           "w_name"         "w_age"         "w_ht"           "w_rank"         "w_rank_points"  "w_hand"         "w_seed"         "w_ace"          "w_df"           "w_1stIn"        "w_1stWon"      "w_2ndWon"       "w_bpSaved"      "w_bpMean"       "w_bpConverted"  "w_svptWon"      "w_returnPtWon"  "w_1stReturnWon" "w_2ndReturnWon" "l_id"          "l_name"         "l_age"          "l_ht"           "l_rank"         "l_rank_points"  "l_hand"         "l_seed"         "l_ace"          "l_df"          "l_1stIn"        "l_1stWon"       "l_2ndWon"       "l_bpSaved"      "l_bpMean"       "l_bpConverted"  "l_svptWon"      "l_returnPtWon"  "l_1stReturnWon"  "l_2ndReturnWon"
####i_name = Nom du joueur
### Variables Optionelles : 
####i_date = Date à partir de laquelle on considère l'historique
####i_surface = Type de surface -> 'Hard'|'Clay'|'Grass'|'None'
####i_tournament = Nom du tournoi -> 'Barcelona'|Bournemouth'|'US Open'
####i_opponent = Nom de l'opposant -> 'Raphael Nadal'|'Roger Federer'|'Alexander Zverev'
####i_row = Nombre de matchs à prendre en compte
####i_duree = Durée en jours d'historique à prendre en compte
f_historique <- function(Tennis_table_work,i_name,i_date=NULL,i_surface=NULL,i_tournament=NULL,i_opponent=NULL,i_row=NULL,i_duree=NULL, i_round=NULL, i_matchnum=NULL) {
  historique=rbind(Tennis_table_work[w_name==i_name,.(tourney_date,tourney_name,surface,match_num,round_num,opponent=l_name,ace=w_ace,df=w_df,stIn=w_1stIn,stWon=w_1stWon,ndWon=w_2ndWon,bpSaved=w_bpSaved,bpMean=w_bpMean,bpConverted=w_bpConverted,svptWon=w_svptWon,returnPtWon=w_returnPtWon,stReturnWon=w_1stReturnWon,ndReturnWon=w_2ndReturnWon)][order(-tourney_date)],
                   Tennis_table_work[l_name==i_name,.(tourney_date,tourney_name,surface,match_num,round_num,opponent=w_name,ace=l_ace,df=l_df,stIn=l_1stIn,stWon=l_1stWon,ndWon=l_2ndWon,bpSaved=l_bpSaved,bpMean=l_bpMean,bpConverted=l_bpConverted,svptWon=l_svptWon,returnPtWon=l_returnPtWon,stReturnWon=l_1stReturnWon,ndReturnWon=l_2ndReturnWon)])[order(-tourney_date)]
  
  if (!is.null(i_date)) {historique=historique[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
  #AJOUTER match_num<match_num ou round<i_round pour les tourney_date=i_date
  #OU i_date==tourney_date&round_num>i_round F=1 SF=2 QF=3 etc OU i_round=='RR' i_matchnum<match_num
  if ((!is.null(i_date))&(!is.null(i_duree))) {historique=historique[difftime(anydate(i_date),tourney_date)<=i_duree]}
  if (!is.null(i_surface)) if (i_surface!=0) {historique=historique[surface==i_surface]}
  if (!is.null(i_tournament)) {historique=historique[tourney_name==i_tournament]}
  if (!is.null(i_opponent)) {historique=historique[opponent==i_opponent]}
  if (!is.null(i_row)) {historique=historique[,head(.SD,i_row)]}
  
  Historique_Mean=historique[,.(ace,df,stIn,stWon,ndWon,bpSaved,bpMean,bpConverted,svptWon,returnPtWon,stReturnWon,ndReturnWon)][,lapply(.SD, function(x) mean(na.omit(x[is.finite(x)])))]
  f_replaceNA(Historique_Mean)
  return(Historique_Mean)
}
#### Calcul le nombre de jeux à partir du score
### Variable obligatoire :
####Tennis_table_work = table d'historique des matchs
####score = Nom de la variable de score
f_NombreDeJeu <- function(Tennis_table_work,score) {
  return(Tennis_table_work[,.(NbJeuJoue=sum(as.numeric(unlist(str_extract_all(gsub("\\(\\d\\)","",score), "[0-9]+"))))),by=c(colnames(Tennis_table_work))])
}
#### Calcul le nombre de jeux précdemment joués (ou le temps)
### Variable obligatoire :
####Tennis_table_work = table d'historique des matchs
####i_name = Nom du joueur
####i_date = Date de calcul
####i_duree = Duree à prendre en compte (en jour)
f_fatigue <- function(Tennis_table_work,i_name,i_date,i_duree=NULL, i_nbmatchs, i_round, i_matchnum) {
  #i_round_num=switch(i_round,'F'=1,'SF'=2,'QF'=3,'R16'=4,'R32'=5,'R64'=6,'R128'=7,'RR'=8,'BR'=9)
  historique=rbind(
    Tennis_table_work[w_name==i_name]
    ,Tennis_table_work[l_name==i_name]
  )[anydate(i_date)==anydate(tourney_date)][i_round<round_num|(i_round>7&match_num<i_matchnum)]
  
  # historique=rbind(
  #   Tennis_table_work[winner_name==i_name]
  #   ,Tennis_table_work[loser_name==i_name]
  #   )[anydate(i_date)>anydate(tourney_date)][difftime(anydate(i_date),anydate(tourney_date))<=i_duree]
  
  Historique_Sum=head(historique[order(round_num),.(NbJeuJoue,minutes)],i_nbmatchs)[,lapply(.SD, function(x) sum(na.omit(x[is.finite(x)])))]
  
  return(Historique_Sum)
}
#### Calcul le nombre de jeux précdemment joués (ou le temps)
### Variable obligatoire :
####Tennis_table_work = table d'historique des matchs
####i_name = Nom du joueur
####i_date = Date de calcul
####i_duree = Duree à prendre en compte (en jour)
f_NbSetsJoues<- function(score) {
  return(length(str_extract_all(gsub("\\(\\d\\)","",'7-5 6-2 prout 8-5(8)'), "[0-9]+")[[1]])/2)
}
f_endurance<- function(Tennis_table_work,i_name,i_date, i_round='BR') {
  i_round_num=switch(i_round,'F'=1,'SF'=2,'QF'=3,'R16'=4,'R32'=5,'R64'=6,'R128'=7,'RR'=8,'BR'=9)
  historique=Tennis_table_work[w_name==i_name|l_name==i_name][anydate(i_date)>tourney_date|(anydate(i_date)==tourney_date&i_round_num<round_num)]
  historique[,Nbsets:=f_NbSetsJoues(score)]
  match_court = nrow(historique[w_name==i_name&best_of>Nbsets])/nrow(historique[best_of>Nbsets])
  match_lon = nrow(historique[w_name==i_name&best_of==Nbsets])/nrow(historique[best_of==Nbsets])
  
  return((ifelse(match_lon>match_court,1,0)))
}
#### Renvoie si le joueur i_name est de retour d'abandon à la date i_date
f_retour_abandon <- function(Tennis_table, i_name, i_date) {
  abandon=Tennis_table[(w_name==i_name|l_name==i_name)][tourney_date<anydate(i_date)][head(order(-xtfrm(tourney_date),round_num,-match_num),1),c(ifelse(gsub("[^A-Za-z]","",score)!=""&l_name==i_name,1,0))]
  retour=ifelse(is.numeric(abandon),abandon,0)
  return(retour)
}
#1
f_NombreTitre <- function (Tennis_table, i_name, i_date) { 
  
  NbTitre <- Tennis_table[round=='F' & w_name == i_name][tourney_date<anydate(i_date)][,.(n_vic = .N)][head(1),c(ifelse('n_vic'!="",n_vic,0))]
  
  retour=ifelse(is.numeric(NbTitre),NbTitre,0)
  return(retour)
  
}

#2
f_NombreTitreSaison <- function(Tennis_table, i_name, i_date) {
  
  NbTitreSaison <- Tennis_table[round=='F' & w_name == i_name][year(tourney_date)==(year(i_date)-1)][,.(n_vic2 = .N)][head(1),c(ifelse('n_vic2'!="",n_vic2,0))]
  
  retour2=ifelse(is.numeric(NbTitreSaison),NbTitreSaison,0)
  return(retour2)
  
}

#3
f_NombreVictoire <- function(Tennis_table, i_name, i_date, i_match_num, i_round) {
  
  NbVIctoire <- Tennis_table[w_name==i_name][tourney_date<anydate(i_date) | (tourney_date == anydate(i_date) & round_num > i_round) | (tourney_date == anydate(i_date) & i_round > 7 & match_num < i_match_num)][,.(n_vic3 = .N)][head(1),c(ifelse('n_vic3'!="",n_vic3,0))]
  
  retour3=ifelse(is.numeric(NbVIctoire),NbVIctoire,0)
  return(retour3)
  
}

#4
f_NombreVictoireSaisonPrecedente <- function(Tennis_table, i_name, i_date) {
  
  NbVIcSaisonPrec <- Tennis_table[w_name==i_name][year(tourney_date) == (year(i_date)-1)][,.(n_vic4 = .N)][head(1),c(ifelse('n_vic4'!="",n_vic4,0))]
  
  retour4=ifelse(is.numeric(NbVIcSaisonPrec),NbVIcSaisonPrec,0)
  return(retour4)
  
}

#5
f_TerrainPredilection <- function(Tennis_table, i_name, i_date, i_match_num, i_round){
  
  tmp2 <- Tennis_table %>% rename(Nom = w_name) %>% 
    
    filter(Nom == i_name,(tourney_date<anydate(i_date) | (tourney_date == anydate(i_date) & round_num > i_round) | (tourney_date == anydate(i_date) & i_round > 7 & match_num < i_match_num)) ) %>% 
    
    group_by(Nom,surface) %>%
    
    summarise(NbVicTerrain = n())
  
  tmp3 <- Tennis_table %>% rename(Nom = l_name) %>% 
    
    filter(Nom == i_name,tourney_date<anydate(i_date) ) %>% group_by(Nom,surface) %>% 
    
    summarise(NbDefTerrain = n())
  
  
  tmp4 <- full_join(tmp2, tmp3, by = c("Nom","surface")) 
  
  tmp4[is.na(tmp4)] <- 0
  
  
  tmp5 <- tmp4 %>%  group_by(Nom) %>% mutate(deltaT = NbVicTerrain - NbDefTerrain) %>% 
    
    group_by(Nom) %>% filter(deltaT==max(deltaT))
  
  return(tmp5$surface)
  
}


f_surface <- function(i_surface, Tennis_table,i_name, i_date,i_match_num, i_round){
  
  terrain <- f_TerrainPredilection(Tennis_table,i_name, i_date, i_match_num, i_round)
  
  egal <- ifelse((i_surface %in% terrain),1,0)
  
  return(egal)
  
}
f_Headtohead <- function(Tennis_table_work,i_name,i_date=NULL,i_surface=NULL,i_tournament=NULL,i_opponent=NULL,i_row=NULL,i_duree=NULL, i_round=NULL, i_matchnum=NULL) {
  historique=rbind(Tennis_table_work[(w_name==i_name & l_name==i_opponent),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=l_name)],
                   Tennis_table_work[(l_name==i_name & w_name==i_opponent),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=w_name)])[order(-tourney_date)]
  if (!is.null(i_date)) {historique=historique[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
  
  #OU i_date==tourney_date&round_num>i_round F=1 SF=2 QF=3 etc OU i_round=='RR' i_matchnum<match_num
  #if ((!is.null(i_date))&(!is.null(i_duree))) {historique=historique[difftime(anydate(i_date),tourney_date)<=i_duree]}
  #if (!is.null(i_surface)) {historique=historique[surface==i_surface]}
  #if (!is.null(i_opponent)) {historique=historique[opponent==i_opponent]}
  if (!is.null(i_row)) {historique=historique[,head(.SD,i_row)]}
  Historique_Mean=historique[,.(w_name)][,lapply(.SD, function(x) mean(as.numeric((i_name==w_name))))]
  return(Historique_Mean)
}

f_VicT <- function(Tennis_table_work,i_name,i_date=NULL,i_surface=NULL,i_tournament=NULL,i_opponent=NULL,i_row=NULL,i_duree=NULL, i_round=NULL, i_matchnum=NULL) {
  historique1=Tennis_table_work[(w_name==i_name | l_name==i_name),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=l_name)][order(-tourney_date)]
  historique2=Tennis_table_work[(w_name==i_opponent | l_name==i_opponent),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=w_name)][order(-tourney_date)]
  if (!is.null(i_date)) {historique1=historique1[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)]}
  #AJOUTER match_num<match_num ou round<i_round pour les tourney_date=i_date
  #OU i_date==tourney_date&round_num>i_round F=1 SF=2 QF=3 etc OU i_round=='RR' i_matchnum<match_num
  if (!is.null(i_surface)) {historique1=historique1[surface==i_surface]}
  historique1_=historique1[1:min(nrow(historique1),i_row)]
  # if (!is.null(i_date)) {historique1=historique1[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
  #AJOUTER match_num<match_num ou round<i_round pour les tourney_date=i_date
  #OU i_date==tourney_date&round_num>i_round F=1 SF=2 QF=3 etc OU i_round=='RR' i_matchnum<match_num
  if (!is.null(i_date)) {historique2=historique2[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)]}
  if (!is.null(i_surface)) {historique2=historique2[surface==i_surface]}
  # if (!is.null(i_row)) {historique2=historique2[,head(.SD,i_row)]}
  # if (!is.null(i_date)) {historique2=historique2[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
  historique2_=historique2[1:min(nrow(historique2),i_row)]
  Historique_Mean1=mean(as.numeric(historique1_$w_name==i_name))
  Historique_Mean2=mean(as.numeric(historique2_$w_name==i_opponent))
  Historique_Mean=Historique_Mean1-Historique_Mean2
  Historique_Mean1_<-ifelse(is.nan(as.numeric(Historique_Mean1)),0,as.numeric(Historique_Mean1)) 
  Historique_Mean2_<-ifelse(is.nan(as.numeric(Historique_Mean2)),0,as.numeric(Historique_Mean2))
  Historique_Mean_=Historique_Mean1_-Historique_Mean2_
  return(Historique_Mean)
}
###############################
#### Tables
Tennis_hist<-fread(file='../Data/atp_matches.csv', header = T)
#On garde uniquement les lignes où on dispose des informations détaillées de match
Tennis_table=Tennis_hist[!is.na(minutes)&substr(tourney_name,0,9)!='Davis Cup']
atp_players<- fread(file ='../Data/atp_players.csv' ,header = F)
colnames(atp_players)=c("Player_Id","Prenom","Nom","Main_Forte","DateNaissance","Nationalite")
Rank<- fread(file ='../Data/rankings.csv' , header = F)
colnames(Rank)<-c("DateRanking","Numero","Player_Id","Points")
Rank[,Points:=as.numeric(Points)]
#Creation de Tennis_table_work
Tennis_table_work=Tennis_table[,.(tourney_id,tourney_name,surface,tourney_date=anydate(tourney_date),tourney_level,match_num,round,round_num=data.table(V1=c('F','SF','QF','R16','R32','R64','R128','RR','BR'),V2=1:9)$V2[match(round,data.table(V1=c('F','SF','QF','R16','R32','R64','R128','RR','BR'),V2=1:9)$V1)],minutes,score,best_of,
                                  w_id=winner_id,w_name=winner_name,w_age=winner_age,w_ht=winner_ht,w_rank=winner_rank,w_rank_points=winner_rank_points,w_hand=ifelse(winner_hand=='R',1,0) ,w_seed=winner_seed
                                  ,w_ace=w_ace/w_1stIn,w_df=w_df/w_svpt,w_1stIn=w_1stIn/w_svpt,w_1stWon=w_1stWon/w_1stIn,w_2ndWon=w_2ndWon/(w_svpt-w_1stIn),w_bpSaved=w_bpSaved/w_bpFaced,w_bpMean=l_bpFaced/l_SvGms,w_bpConverted=(l_bpFaced-l_bpSaved)/(l_bpFaced),w_svptWon=(w_1stWon+w_2ndWon)/w_svpt,w_returnPtWon=(l_svpt-l_1stWon-l_2ndWon)/l_svpt,w_1stReturnWon=(l_1stIn-l_1stWon)/l_1stIn,w_2ndReturnWon=((l_svpt-l_1stIn)-l_2ndWon)/(l_svpt-l_1stIn)
                                  ,l_id=loser_id,l_name=loser_name,l_age=loser_age,l_ht=loser_ht,l_rank=loser_rank,l_rank_points=loser_rank_points,l_hand=ifelse(loser_hand=='R',1,0),l_seed=loser_seed
                                  ,l_ace=l_ace/l_1stIn,l_df=l_df/l_svpt,l_1stIn=l_1stIn/l_svpt,l_1stWon=l_1stWon/l_1stIn,l_2ndWon=l_2ndWon/(l_svpt-l_1stIn),l_bpSaved=l_bpSaved/l_bpFaced,l_bpMean=w_bpFaced/w_SvGms,l_bpConverted=(w_bpFaced-w_bpSaved)/(w_bpFaced),l_svptWon=(l_1stWon+l_2ndWon)/l_svpt,l_returnPtWon=(w_svpt-w_1stWon-w_2ndWon)/w_svpt,l_1stReturnWon=(w_1stIn-w_1stWon)/w_1stIn,l_2ndReturnWon=((w_svpt-w_1stIn)-w_2ndWon)/(w_svpt-w_1stIn)
)]
#Modification Date et suppression des valeurs NaN sur les bp_saved
Tennis_table_work[,date:=anydate(tourney_date)][,.(date,tourney_date)]
Tennis_table_work[is.nan(w_bpSaved),':='(w_bpSaved=1)]
Tennis_table_work[is.nan(l_bpSaved),':='(l_bpSaved=1)]
Tennis_table_work=f_NombreDeJeu(Tennis_table_work,"score")

###############################
#### Models
load(file="../Models/RF500.RData")
load(file="../Models/ROCR_pred.RData")