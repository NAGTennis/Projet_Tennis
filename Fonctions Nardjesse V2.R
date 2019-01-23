
library(data.table)
install.packages("stringr")
library(stringr)
install.packages("sqldf")
library(sqldf)
install.packages("knitr")
library(knitr)
install.packages("anytime")
library(anytime)
###################
install.packages('dplyr')
library(dplyr)
library(dtplyr)
install.packages('lubridate')
library(lubridate)

Tennis_hist <- tbl_dt(Base_Finale)
table(Tennis_table_work$round)

#On garde uniquement les lignes où on dispose des informations détaillées de match
Tennis_table=Tennis_hist[!is.na(minutes)&substr(tourney_name,0,9)!='Davis Cup']

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

f_NombreDeJeu <- function(Tennis_table_work,score) {
  return(Tennis_table_work[,.(NbJeuJoue=sum(as.numeric(unlist(str_extract_all(gsub("\\(\\d\\)","",score), "[0-9]+"))))),by=c(colnames(Tennis_table_work))])
}

f_endurance<- function(Tennis_table_work,i_name,i_date, i_round='BR') {
  i_round_num=switch(i_round,'F'=1,'SF'=2,'QF'=3,'R16'=4,'R32'=5,'R64'=6,'R128'=7,'RR'=8,'BR'=9)
  historique=Tennis_table_work[w_name==i_name|l_name==i_name][anydate(i_date)>tourney_date|(anydate(i_date)==tourney_date&i_round_num<round_num)]
  historique[,Nbsets:=f_NbSetsJoues(score)]
  match_court = nrow(historique[w_name==i_name&best_of>Nbsets])/nrow(historique[best_of>Nbsets])
  match_lon = nrow(historique[w_name==i_name&best_of==Nbsets])/nrow(historique[best_of==Nbsets])
  
  return(ifelse(match_lon>match_court,1,0))
}

f_retour_abandon <- function(Tennis_table, i_name, i_date) {
  abandon=Tennis_table[(w_name==i_name|l_name==i_name)][tourney_date<anydate(i_date)][head(order(-xtfrm(tourney_date),round_num,-match_num),1),c(ifelse(gsub("[^A-Za-z]","",score)!=""&l_name==i_name,1,0))]
  retour=ifelse(is.numeric(abandon),abandon,0)
  return(retour)
}

i_name='Rafael Nadal'; i_date='2018-01-01'
f_retour_abandon(Tennis_table_work,'Rafael Nadal','2018-01-01')


######### NARDJESSE #########
#1
f_NombreTitre <- function (Tennis_table, i_name, i_date) {
  
  tmp <-  Tennis_table  %>% 
    filter(round=='F',w_name==i_name, tourney_date<anydate(i_date)) %>% group_by(w_name) %>% 
    summarise(nb_vic = n())
  
  return(tmp$nb_vic)
  
}

data_test <- Tennis_table_work %>% mutate(NbTitre = f_NombreTitre(Tennis_table_work,w_name,tourney_date))


#2
f_NombreTitreSaison <- function(Tennis_table, i_name, i_date) {
  tmp <-  Tennis_table  %>% 
    filter(round=='F',w_name==i_name, year(tourney_date)== year(i_date)) %>% group_by(w_name) %>% 
    summarise(nb_vic = n())
  
  return(tmp$nb_vic)
  
}

f_NombreTitre(Tennis_table_work,'Rafael Nadal','2017-12-31')
f_NombreTitreSaison(Tennis_table_work,'Rafael Nadal','2017-12-31')

#3
f_NombreVictoire <- function(Tennis_table, i_name, i_date, i_match_num, i_round) {
  
  tmp <- Tennis_table %>% 
    filter(w_name==i_name, (tourney_date<anydate(i_date) |tourney_date == anydate(i_date) & round_num > i_round |tourney_date == anydate(i_date) & i_round > 7 & match_num < i_match_num)) %>%
    group_by(w_name) %>% summarise(nb_vic = n())
  return(tmp$nb_vic)
  
}

f_NombreVictoire (Tennis_table_work,'Rafael Nadal','2004-01-12','12','5')
f_NombreVictoire (Tennis_table_work,'Rafael Nadal','2004-01-12','30','2')


#4
f_NombreVictoireSaison <- function(Tennis_table, i_name, i_date) {
  
  tmp <- Tennis_table %>% filter(w_name==i_name, year(tourney_date) == year(i_date)) %>%
    group_by(w_name) %>% summarise(nb_vic = n())
  return(tmp$nb_vic)
  
}


#5
f_NombreVictoireSaisonPrecedente <- function(Tennis_table, i_name, i_date) {
  
  tmp <- Tennis_table %>% filter(w_name==i_name, year(tourney_date) == year(i_date)-1) %>%
    group_by(w_name) %>% summarise(nb_vic = n())
  return(tmp$nb_vic)
  
}

f_NombreVictoireSaison(Tennis_table_work,'Rafael Nadal','2018-12-31')
f_NombreVictoireSaisonPrecedente(Tennis_table_work,'Rafael Nadal','2018-12-31')

#6
f_TerrainPredilection <- function(Tennis_table, i_name, i_date, i_match_num, i_round){
  
  tmp2 <- Tennis_table %>% rename(Nom = w_name) %>% 
    filter(Nom ==i_name,(tourney_date<anydate(i_date) |tourney_date == anydate(i_date) & round_num > i_round |tourney_date == anydate(i_date) & i_round > 7 & match_num < i_match_num) ) %>% 
    group_by(Nom,surface) %>%
    summarise(NbVicTerrain = n())%>% arrange(Nom)  
  tmp3 <- Tennis_table %>% rename(Nom = l_name) %>% 
    filter(Nom ==i_name,tourney_date<anydate(i_date) ) %>% group_by(Nom,surface) %>% 
    summarise(NbDefTerrain = n())%>% arrange(Nom) 
  
  tmp4 <- full_join(tmp2, tmp3) 
  tmp4[is.na(tmp4)] <- 0
  
  tmp5 <- tmp4 %>%  group_by(Nom) %>% mutate(deltaT = NbVicTerrain - NbDefTerrain) %>% 
    group_by(Nom) %>% filter(deltaT==max(deltaT))
  
  return(tmp5$surface)
  
} #Rajouter un distinct pour n'avoir qu'un seul Terrain de predilection pour gérer les égalités?


f_TerrainPredilection(Tennis_table_work,'Rafael Nadal','2004-01-12','12','5')

#7
f_surface <- function(i_surface, Tennis_table,i_name, i_date,i_match_num, i_round){
terrain <- f_TerrainPredilection(Tennis_table,i_name, i_date, i_match_num, i_round)
egal <- ifelse((terrain == i_surface),1,0)
return(egal)

}

Tennis_table = Tennis_table_work;i_name='Alexander Zverev';i_date='20170101'
f_surface('Clay', Tennis_table_work,'Rafael Nadal','2004-01-12','12','5')
f_surface('Hard', Tennis_table_work,i_name='Rafael Nadal',i_date='20170101')


#############################


for (i in 1:nrow(Tennis_table_work)) {
  
  
  
}



test_date <- '2018-01-01'
anydate(test_date)


test2 <- Tennis_table_work %>% filter(w_name == 'Rafael Nadal', round == 'F', year(tourney_date) == year('2018-12-31')-1)
test3 <- Tennis_table_work %>% filter(w_name == 'Rafael Nadal', year(tourney_date) == year('2018-12-31')-1)
test3 <- Tennis_table_work %>% filter(w_name == 'Rafael Nadal')
test3 <- test3 %>% arrange(tourney_date)
table(test2$round)

table(test2$tourney_name,test2$tourney_level)

############
table_score=Tennis_table_work[,.(tourney_id,tourney_name,surface,tourney_date,tourney_level,match_num,round,round_num,w_id,w_name,w_age,w_ht,w_rank,w_rank_points,w_hand,w_seed,l_id,l_name,l_age,l_ht,l_rank,l_rank_points,l_hand,l_seed)]
nb_row=nrow(table_score)
table_score[,coin:=round((runif(nb_row,0,1)),0)]



table_score[coin==1,c("p1_id","p1_name","p1_age","p1_ht","p1_rank","p1_rank_points","p1_hand","p1_seed","p2_id","p2_name","p2_age","p2_ht","p2_rank","p2_rank_points","p2_hand","p2_seed") := list(w_id,w_name,w_age,w_ht,w_rank,w_rank_points,w_hand,w_seed,l_id,l_name,l_age,l_ht,l_rank,l_rank_points,l_hand,l_seed)]
table_score[coin==0,c("p1_id","p1_name","p1_age","p1_ht","p1_rank","p1_rank_points","p1_hand","p1_seed","p2_id","p2_name","p2_age","p2_ht","p2_rank","p2_rank_points","p2_hand","p2_seed") := list(l_id,l_name,l_age,l_ht,l_rank,l_rank_points,l_hand,l_seed,w_id,w_name,w_age,w_ht,w_rank,w_rank_points,w_hand,w_seed)]

table_score[,c("w_id","w_name","w_age","w_ht","w_rank","w_rank_points","w_hand","w_seed","l_id","l_name","l_age","l_ht","l_rank","l_rank_points","l_hand","l_seed"):=NULL]

table_score[,c("age","ht","rank","rank_points","hand","seed"):=list(p1_age-p2_age,p1_ht-p2_ht,p1_rank-p2_rank,p1_rank_points-p2_rank_points,p1_hand-p2_hand,p1_seed-p2_seed)]
table_score[,c("p1_age","p2_age","p1_ht","p2_ht","p1_rank","p2_rank","p1_rank_points","p2_rank_points","p1_hand","p2_hand","p1_seed","p2_seed"):=NULL]

################

table_scoreV1 <- table_score[1:1000,]

table_scoreV1[,c('NbTitre', 'NbTitreSaison','NbVictoire','NbVictoireSaison','NbVictoireSaisonPrec', 'SurfacePred')
            := c((f_NombreTitre(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date))
                 ,(f_NombreTitreSaison(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date))
                 ,(f_NombreVictoire (Tennis_table_work,i_name=p1_name,i_round=round_num,i_matchnum=match_num,i_date=tourney_date)-f_NombreVictoire (Tennis_table_work,i_name=p2_name,i_round=round_num,i_matchnum=match_num,i_date=tourney_date))
                 ,(f_NombreVictoireSaison(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreVictoireSaison(Tennis_table_work,i_name=p2_name,i_date=tourney_date))
                 ,(f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p2_name,i_date=tourney_date))
                 ,(f_surface(Tennis_table_work,i_name=p1_name,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_surface=surface)-f_surface(Tennis_table_work,i_name=p2_name,i_date=tourney_date,i_round=round_num,i_matchnum=match_num,i_surface=surface))),by=c(colnames(table_score))]



test <- table_scoreV1[,c('NbTitre'):= c(f_NombreTitre(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date)), by=c(colnames(table_scoreV1))]

test <- table_scoreV1  %>% mutate(NbTitre2 = f_NombreTitre(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date))

table(test$NbTitre)
class(Tennis_table_work$w_name)
class(Tennis_table_work$w_name)
