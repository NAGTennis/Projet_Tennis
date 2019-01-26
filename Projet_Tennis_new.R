#install.packages("data.table")
library(data.table)
#install.packages("stringr")
library(stringr)
#install.packages("sqldf")
library(sqldf)
#install.packages("anytime")
library(anytime)
#test
#Mis en place de l'environnement de travail
setwd("C:/Users/Greg/Documents/Certificat Data Science/Projet/BDD/tennis_atp-master")
setwd("//FRSHARES0250.france.intra.corp/Planif_users/b06713/Data Science ENSAE/Certificat Data Science/tennis_atp-master")
Tennis_hist<-fread(file="atp_matches.csv", header = T,fill=T)
Tennis_table<-fread(file="atp_matches.csv", header = T,fill=T)
head(Tennis_table)
#test2
#Import et premier retraitement sur le nom des variables
#virer davis cup 
#changer condition pour les dates
#renommer les variables pour caller sur Axel
#reflechir à un segmentation des joueurs

tennis2017<- read.csv(file ='atp_matches_2017.csv' ,header = T)
tennis2016<- read.csv(file ='atp_matches_2016.csv' ,header = T)
tennis2015<- read.csv(file ='atp_matches_2015.csv' , header = T)
tennis2014<- read.csv(file ='atp_matches_2014.csv' , header = T)
tennis2013<- read.csv(file ='atp_matches_2013.csv' , header = T)
tennis2012<- read.csv(file ='atp_matches_2012.csv' , header = T)
tennis2011<- read.csv(file ='atp_matches_2011.csv' , header = T)
tennis2010<- read.csv(file ='atp_matches_2010.csv' , header = T)

Tennis<-rbind(tennis2017,tennis2016,tennis2015,tennis2014,tennis2013,tennis2012)
dim(Tennis)
head(tennis2010)
atp_players<- read.csv(file ='atp_players.csv' ,header = F)

colnames(atp_players)=c("Player_Id","Prenom","Nom","Main_Forte","DateNaissance","Nationalite")


Rank<- read.csv(file ='atp_rankings_00s.csv' ,
                      header = F)
colnames(Rank)<-c("DateRanking","Numero","Player_Id","Points")

#Changement du format en data.table
Tennis_table<-as.data.table(Tennis)

#Retraitemnt des NA
NA_vect<-(is.na(Tennis_table$winner_id))
Tennis_table<-Tennis_table[!(NA_vect)]
Tennis_table<-Tennis_table[,.(tourney_id,tourney_name,surface,score,tourney_date=anydate(tourney_date),tourney_level,match_num,round,round_num=data.table(V1=c('F','SF','QF','R16','R32','R64','R128','RR','BR'),V2=1:9)$V2[match(round,data.table(V1=c('F','SF','QF','R16','R32','R64','R128','RR','BR'),V2=1:9)$V1)],
                              w_id=winner_id,w_name=winner_name,w_entry=winner_entry,w_ioc=winner_ioc,w_age=winner_age,w_ht=winner_ht,w_rank=winner_rank,w_rank_points=winner_rank_points,w_hand=ifelse(winner_hand=='R',1,0) ,w_seed=winner_seed
                              ,w_ace=w_ace/w_1stIn,w_SvGms,w_svpt,w_df=w_df/w_svpt,w_1stIn=w_1stIn/w_svpt,w_1stWon=w_1stWon/w_1stIn,w_2ndWon=w_2ndWon/(w_svpt-w_1stIn),w_bpSaved=w_bpSaved/w_bpFaced,w_bpMean=l_bpFaced/l_SvGms,w_bpConverted=(l_bpFaced-l_bpSaved)/(l_bpFaced),w_svptWon=(w_1stWon+w_2ndWon)/w_svpt,w_returnPtWon=(l_svpt-l_1stWon-l_2ndWon)/l_svpt,w_1stReturnWon=(l_1stIn-l_1stWon)/l_1stIn,w_2ndReturnWon=((l_svpt-l_1stIn)-l_2ndWon)/(l_svpt-l_1stIn)
                              ,l_id=loser_id,l_name=loser_name,l_age=loser_age,l_ht=loser_ht,l_entry=loser_entry,l_ioc=loser_ioc,l_rank=loser_rank,l_rank_points=loser_rank_points,l_hand=loser_hand,l_seed=loser_seed
                              ,l_ace=l_ace/l_1stIn,l_SvGms,l_svpt,l_df=l_df/l_svpt,l_1stIn=l_1stIn/l_svpt,l_1stWon=l_1stWon/l_1stIn,l_2ndWon=l_2ndWon/(l_svpt-l_1stIn),l_bpSaved=l_bpSaved/l_bpFaced,l_bpMean=w_bpFaced/w_SvGms,l_bpConverted=(w_bpFaced-w_bpSaved)/(w_bpFaced),l_svptWon=(l_1stWon+l_2ndWon)/l_svpt,l_returnPtWon=(w_svpt-w_1stWon-w_2ndWon)/w_svpt,l_1stReturnWon=(w_1stIn-w_1stWon)/w_1stIn,l_2ndReturnWon=((w_svpt-w_1stIn)-w_2ndWon)/(w_svpt-w_1stIn)
)]
a<-year(Tennis_table$tourney_date)
Tennis_table<-Tennis_table[year(tourney_date)>1990]

#Cr?ation d'un RankingRate : permet d'avoir le rang en taux, en fonction du rang 
#le plus haut et du rang le plus bas dans un tournoi
# Max<-Tennis_table[, MaxRankPointWin := max(max(winner_rank_points),max(l_rank_points))][,MaxRankPointWin]
# Min<- Tennis_table[, MinRank:=min(max(w_rank),min(l_rank)),by=tourney_id]
# Tennis_table[,':='(RankingRateWin=w_rank_points/MaxRankPointWin,
#                        RankingRateLose=l_rank_points/MaxRankPointWin)]

#Cr?ation du nombre de jeu et du nombre de Set


# comprendre gsub http://www.endmemo.com/program/R/gsub.php 
Tennis_table[,Score_v2:=gsub("\\(\\d\\)","",score)]
Tennis_table[,':='(Score_v2=gsub("\\(\\d\\)","",score),
                      NombreJeu=sum(as.numeric(unlist((str_extract_all(Score_v2, "[0-9]+")))))),
                 by=.(tourney_id, match_num, tourney_date)]

#Tennis2017_table[,':='(NombreJeu=sum(as.numeric(unlist(Score_v2)))), by=.(tourney_id, match_num, tourney_date)]

#test<-unlist(Tennis_table$Score_v2)
#test<-as.numeric(test)
#class(Tennis_table$Score_v2)
#unlist(Tennis_table$Score_v2)
#Tennis_table[,for (i in 1:nrow(Tennis_table)){ Abandon[i]:=  c("RET","WO") %in%
 #     gsub("[[:space:]]","",
  #              gsub("\\d+","",
   #                       gsub("[[:punct:]]" ,""
    #                                    ,Tennis_table$Score_v2)))},]

#Cr?ation d'un flag abandon => on ne fait pas la distinction entre abandonn? pendant le match 
#et abandonner avant le match, mode du flag ? valider
Abandon<-gsub("[[:space:]]","",gsub("\\d+","",gsub("[[:punct:]]" ,"",Tennis_table$Score_v2)))
Abandon
Abandonv2<-(Abandon != "")
Abandonv2
Abandonv3 <-as.numeric(Abandonv2)
Abandonv3
add<-cbind(Abandon,Abandonv3)
#Tennis_table[Abandon=="DEF"]
#Abandon=c(rep(x = "FALSE", nrow(Tennis_table)))

#for (i in (1:nrow(Tennis_table))){
#if (c("RET","WO","Walkover","DEF") %in% check[i]) Abandon[i]=check[i] 
#}

str(Abandon)
#To do : etudier les modalit?s de la variables Abandon -> comprendre quoi correspond ? quoi
levels(as.factor(Abandon))

#Varibale ? rajouter/cr?er : 
#Fatigue du joueur -> fonction du nombre de matchs fait ces derniers jours et du nombre de jeu disput? ces derniers temps
#Ranking position -> prendre en compte la position du joueur, faire eventuellement un ratio avec les points
#Tr?s bon serveur ou pas -> influence dans les TieBreak
#Endurant ou pas (exemple s il a tendance a gagn? ces matchs en 5 set ou 3 pour les filles)
#Nombre de jeu par match, enc as de Tie break rajouter un jeu suppl?mentaire 
#
#
#

#Reorganisation de la table pour que le le winner ait 50% de chance d'?tre joueur 1/ joueur 2 => avoid sur-apprentissage
set.seed(123)
Coin<- round((runif(nrow(Tennis_table),0,1)),0)
#ReorgName(Tennis_table)
head(Tennis_table)
#Si possible optimiser
# for (i in 1:nrow(Tennis_table)) {
#   if (Coin[i] == 1) {
#     Tennis_table[i,':='(player1_id=w_id, player1_seed=w_seed, player1_entry= w_entry,
#                         player1_name=w_name,player1_hand=w_hand,player1_ht=w_ht,player1_ioc=w_ioc,
#                             player1_age=w_age,player1_rank=w_rank,player1_rank_points=w_rank_points,
#                         player1_ace=w_ace,player1_df=w_df,player1_svpt=w_svpt,player1_1stIn=w_1stIn,player1_1stWon=w_1stWon,
#                         player1_2ndWon=w_2ndWon,player1_SvGms=w_SvGms,player1_bpSaved=w_bpSaved,
#                         #player1_bpFacedn=w_bpFaced,
#                         player1_bpMean=w_bpMean ,player1_bpConverted=w_bpConverted,player1_svptWon=w_svptWon , 
#                         player1_returnPtWon=w_returnPtWon,player1_1stReturnWon=w_1stReturnWon,player1_2ndReturnWon=w_2ndReturnWon,
# 
#                         player2_id=l_id, player2_seed=l_seed, player2_entry= l_entry,
#                         player2_name=l_name,player2_hand=l_hand,player2_ht=l_ht,player2_ioc=l_ioc,
#                             player2_age=l_age,player2_rank=l_rank,player2_rank_points=l_rank_points,
#                             player2_ace=l_ace,player2_df=l_df,player2_svpt=l_svpt,player2_1stIn=l_1stIn,player2_1stWon=l_1stWon,
#                             player2_2ndWon=l_2ndWon,player2_SvGms=l_SvGms,player2_bpSaved=l_bpSaved,
#                         #player2_bpFacedn=l_bpFaced,
#                         player2_bpMean=l_bpMean ,player2_bpConverted=l_bpConverted,player2_svptWon=l_svptWon , 
#                         player2_returnPtWon=l_returnPtWon,player2_1stReturnWon=l_1stReturnWon,player2_2ndReturnWon=l_2ndReturnWon),]
#   }
# if (Coin[i]==0 ){
#   Tennis_table[i, ':='(player2_id=w_id, player2_seed=w_seed, player2_entry= w_entry,
#                        player2_name=w_name,player2_hand=w_hand,player2_ht=w_ht,player2_ioc=w_ioc,
#                        player2_age=w_age,player2_rank=w_rank,player2_rank_points=w_rank_points,
#                        player2_ace=w_ace,player2_df=w_df,player2_svpt=w_svpt,player2_1stIn=w_1stIn,player2_1stWon=w_1stWon,
#                        player2_2ndWon=w_2ndWon,player2_SvGms=w_SvGms,player2_bpSaved=w_bpSaved,
#                        #player2_bpFacedn=w_bpFaced,
#                        player2_bpMean=w_bpMean ,player2_bpConverted=w_bpConverted,player2_svptWon=w_svptWon , 
#                        player2_returnPtWon=w_returnPtWon,player2_1stReturnWon=w_1stReturnWon,player2_2ndReturnWon=w_2ndReturnWon,
#                        player1_id=l_id, player1_seed=l_seed, player1_entry= l_entry,
#                        player1_name=l_name,player1_hand=l_hand,player1_ht=l_ht,player1_ioc=l_ioc,
#                        player1_age=l_age,player1_rank=l_rank,player1_rank_points=l_rank_points,
#                        player1_ace=l_ace,player1_df=l_df,player1_svpt=l_svpt,player1_1stIn=l_1stIn,player1_1stWon=l_1stWon,
#                        player1_2ndWon=l_2ndWon,player1_SvGms=l_SvGms,player1_bpSaved=l_bpSaved,
#                        #player1_bpFacedn=l_bpFaced,
#                        player1_bpMean=l_bpMean ,player1_bpConverted=l_bpConverted,player1_svptWon=l_svptWon , 
#                        player1_returnPtWon=l_returnPtWon,player1_1stReturnWon=l_1stReturnWon,player1_2ndReturnWon=l_2ndReturnWon
#   ),]
# }
# }


for (i in 1:nrow(Tennis_table)) {
  if (Tennis_table$w_id[i] <Tennis_table$l_id[i]) {
    Tennis_table[i,':='(player1_id=w_id, player1_seed=w_seed, player1_entry= w_entry,
                        player1_name=w_name,player1_hand=w_hand,player1_ht=w_ht,player1_ioc=w_ioc,
                        player1_age=w_age,player1_rank=w_rank,player1_rank_points=w_rank_points,
                        player1_ace=w_ace,player1_df=w_df,player1_svpt=w_svpt,player1_1stIn=w_1stIn,player1_1stWon=w_1stWon,
                        player1_2ndWon=w_2ndWon,player1_SvGms=w_SvGms,player1_bpSaved=w_bpSaved,
                        #player1_bpFacedn=w_bpFaced,
                        player1_bpMean=w_bpMean ,player1_bpConverted=w_bpConverted,player1_svptWon=w_svptWon , 
                        player1_returnPtWon=w_returnPtWon,player1_1stReturnWon=w_1stReturnWon,player1_2ndReturnWon=w_2ndReturnWon,
                        
                        player2_id=l_id, player2_seed=l_seed, player2_entry= l_entry,
                        player2_name=l_name,player2_hand=l_hand,player2_ht=l_ht,player2_ioc=l_ioc,
                        player2_age=l_age,player2_rank=l_rank,player2_rank_points=l_rank_points,
                        player2_ace=l_ace,player2_df=l_df,player2_svpt=l_svpt,player2_1stIn=l_1stIn,player2_1stWon=l_1stWon,
                        player2_2ndWon=l_2ndWon,player2_SvGms=l_SvGms,player2_bpSaved=l_bpSaved,
                        #player2_bpFacedn=l_bpFaced,
                        player2_bpMean=l_bpMean ,player2_bpConverted=l_bpConverted,player2_svptWon=l_svptWon , 
                        player2_returnPtWon=l_returnPtWon,player2_1stReturnWon=l_1stReturnWon,player2_2ndReturnWon=l_2ndReturnWon),]
  }
  if (Tennis_table$w_id[i] >Tennis_table$l_id[i]){
    Tennis_table[i, ':='(player2_id=w_id, player2_seed=w_seed, player2_entry= w_entry,
                         player2_name=w_name,player2_hand=w_hand,player2_ht=w_ht,player2_ioc=w_ioc,
                         player2_age=w_age,player2_rank=w_rank,player2_rank_points=w_rank_points,
                         player2_ace=w_ace,player2_df=w_df,player2_svpt=w_svpt,player2_1stIn=w_1stIn,player2_1stWon=w_1stWon,
                         player2_2ndWon=w_2ndWon,player2_SvGms=w_SvGms,player2_bpSaved=w_bpSaved,
                         #player2_bpFacedn=w_bpFaced,
                         player2_bpMean=w_bpMean ,player2_bpConverted=w_bpConverted,player2_svptWon=w_svptWon , 
                         player2_returnPtWon=w_returnPtWon,player2_1stReturnWon=w_1stReturnWon,player2_2ndReturnWon=w_2ndReturnWon,
                         player1_id=l_id, player1_seed=l_seed, player1_entry= l_entry,
                         player1_name=l_name,player1_hand=l_hand,player1_ht=l_ht,player1_ioc=l_ioc,
                         player1_age=l_age,player1_rank=l_rank,player1_rank_points=l_rank_points,
                         player1_ace=l_ace,player1_df=l_df,player1_svpt=l_svpt,player1_1stIn=l_1stIn,player1_1stWon=l_1stWon,
                         player1_2ndWon=l_2ndWon,player1_SvGms=l_SvGms,player1_bpSaved=l_bpSaved,
                         #player1_bpFacedn=l_bpFaced,
                         player1_bpMean=l_bpMean ,player1_bpConverted=l_bpConverted,player1_svptWon=l_svptWon , 
                         player1_returnPtWon=l_returnPtWon,player1_1stReturnWon=l_1stReturnWon,player1_2ndReturnWon=l_2ndReturnWon
    ),]
  }
}

head(Tennis_table)
pb<-Tennis_table[is.na(player1_name)]
head(pb)
ok<-Tennis_table[!(is.na(player1_name))]
ok


#ReorgName<-function(x) {
 # Coin<- round((runif(nrow(Tennis_table),0,1)),0)
  
  #for (i in 1:nrow(Tennis_table)) 
   # if (Coin[i] == 1) {
    #  setnames(Tennis_table,
     #          old= c('w_id', 'w_seed', 'w_entry','w_name', 'w_hand', 'w_ht', 'w_ioc',
      #                'w_age', 'w_rank','w_rank_points','l_id', 'l_seed', 'l_entry','l_name',
       #               'l_hand', 'l_ht', 'l_ioc', 'l_age', 'l_rank','l_rank_points'),
        #       new = c('player1_id', 'player1_seed', 'player1_entry','player1_name', 'player1_hand', 'player1_ht', 
         #              'player1_ioc', 'player1_age', 'player1_rank','player1_rank_points','player2_id', 'player2_seed',
          #             'player2_entry','player2_name','player2_hand', 'player2_ht', 'player2_ioc', 'player2_age', 
           #            'player2_rank','player2_rank_points'))
      
#}
  #if (Coin[i]==0 ){
   # setnames(Tennis_table[i],
     #        c('w_id', 'w_seed', 'w_entry','w_name', 'w_hand', 'w_ht', 'w_ioc',
      #         'w_age', 'w_rank','w_rank_points','l_id', 'l_seed', 'l_entry','l_name',
       #        'l_hand', 'l_ht', 'l_ioc', 'l_age', 'l_rank','l_rank_points'),
        #     c('player2_id', 'player2_seed', 'player2_entry',
         #      'player2_name','player2_hand', 'player2_ht', 'player2_ioc', 'player2_age', 'player2_rank',
          #     'player2_rank_points','player1_id', 'player1_seed', 'player1_entry','player1_name', 'player1_hand',
           #    'player1_ht', 'player1_ioc', 'player1_age', 'player1_rank','player1_rank_points'))
  #}
#}
#ReorgName(Tennis_table)


head(Tennis_table)
# Rtraitement des dates
#strDates <- c("01/05/1965", "08/16/1975")
#dates <- as.Date(strDates, "%m/%d/%Y")

# mode(Tennis_table$tourney_date)
# Tennis_table$tourney_date
# Tennis_table[, tourney_date_char:=as.character(tourney_date)][,':='(annee=substr(tourney_date_char,1,4),
#                                                                      mois=substr(tourney_date_char,5,6),
#                                                                      jour=substr(tourney_date_char,7,nchar(tourney_date_char)))][,]
# Tennis_table[,DateTourney:=as.Date(paste(jour,mois,annee,sep="/"),"%d/%m/%Y")]
# 
# head(Tennis_table)                       

#n derni?res confrontation, reflechir ? comment prendre en compte le n 
#compter le nombre de victoire 
#1) Faire la fonction puis faire un Lapply => s'applique ? tout le jeu de donn?es
#2) Faire une boucle for pour avoir un vecteur de la taille de la table puis un cbind

# 
# Hist<-NULL
# 
# 
# sCORE<-function(n,player1,player2,date){
#   
#   temp<-setorder(Tennis_table[(tourney_date<date) & 
#                                     ((player1_id==player1 & player2_id ==player2) | (player1_id==player2 & player2_id ==player1))],-tourney_date)[1:n]
#                                     
# ratioPlayer1[i]<-mean(as.numeric(player1==temp$w_id))
# ratioPlayer2[i]<-mean(as.numeric(player2==temp$w_id))
# NbrVictPlayer1[i]<-sum(as.numeric(player1==temp$w_id))
# NbrVictPlayer2[i]<-sum(as.numeric(player2==temp$w_id)) 
# 
# 
# }
n=5
ratioPlayer1<-c(rep(-1,nrow(Tennis_table)))
ratioPlayer2<-c(rep(-1,nrow(Tennis_table)))
NbrVictPlayer1<-c(rep(-1,nrow(Tennis_table)))
NbrVictPlayer2<-c(rep(-1,nrow(Tennis_table)))
Nbrmatch<-c(rep(-1,nrow(Tennis_table)))
res<-as.data.frame(cbind(ratioPlayer1,ratioPlayer2,NbrVictPlayer1,NbrVictPlayer2,Nbrmatch))
#Problème sur la boucle for, elle s'arrête à 78

HeadToHead<-function(n=5,date,player1,player2,round_num){
  # i<-266
  # date=Tennis_table$tourney_date[i]
  # player1=Tennis_table$player1_id[i]
  # player2=Tennis_table$player2_id[i]
  # round_num=Tennis_table$round_num[i]
  temp<-setorder(Tennis_table[((tourney_date<date) | (tourney_date==date & round_num<round_num)) 
                              & ((player1_id==player1 & player2_id ==player2) |
                                   (player1_id==player2 & player2_id ==player1))
                              ],-tourney_date)
  
  temp<-temp[1:as.numeric(min(n,nrow(temp)))]
  
  #reflechir à comment remplir ce DF
  Nbrmatch[i]<-nrow(temp)
  #Nbrmatch[i]
  res$ratioPlayer1[i]<-mean(as.numeric(player1==temp$w_id))
  #ratioPlayer1[i]
  res$ratioPlayer2[i]<-mean(as.numeric(player2==temp$w_id))
  # ratioPlayer2[i]
  res$NbrVictPlayer1[i]<-sum(as.numeric(player1==temp$w_id))
  res$NbrVictPlayer2[i]<-sum(as.numeric(player2==temp$w_id)) 
  return(res)
}
head(HeadToHead(n=5,date=Tennis_table$tourney_date[7],player1=Tennis_table$player1_id[7],
           player2=Tennis_table$player2_id[7], round_num=Tennis_table$round_num[7]))
test<-apply(Tennis_table,1,
             FUN=HeadToHead,n=5,date=tourney_date,player1 = player1_id, player2=player2_id,round_num = round_num)
for (i in 1:nrow(Tennis_table)) {
  date_i<-Tennis_table$tourney_date[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  round_num_i<-Tennis_table$round_num[i]
  #Problème
  temp<-setorder(Tennis_table[((tourney_date<date_i) | (tourney_date==date_i & round_num_i<round_num)) 
                              & ((player1_id==player1 & player2_id ==player2) |
                                 (player1_id==player2 & player2_id ==player1))
                              ],-tourney_date)
  
  temp<-temp[1:as.numeric(min(n,nrow(temp)))]
  
  Nbrmatch[i]<-nrow(temp)
  #Nbrmatch[i]
  ratioPlayer1[i]<-mean(as.numeric(player1==temp$w_id))
  #ratioPlayer1[i]
  ratioPlayer2[i]<-mean(as.numeric(player2==temp$w_id))
 # ratioPlayer2[i]
  NbrVictPlayer1[i]<-sum(as.numeric(player1==temp$w_id))
  NbrVictPlayer2[i]<-sum(as.numeric(player2==temp$w_id)) 
  
}
head(ratioPlayer2)
add<-as.data.frame(add)
add<-cbind(add,ratioPlayer1,ratioPlayer2,NbrVictPlayer1,NbrVictPlayer2)
head(add)
# NbrVictPlayer1
# table(Nbrmatch)
# Nbrmatch==-1
# ratioPlayer1
# NbrVictPlayer1
# Tennis_table[,':='(ratioPlayer1=ratioPlayer1,ratioPlayer2=ratioPlayer2,NbrVictPlayer1=NbrVictPlayer1,NbrVictPlayer2=NbrVictPlayer2,Nbrmatch=Nbrmatch)]
# head(Tennis_table)
# pb1<-Tennis_table[(Nbrmatch==-1)]
# test<-lapply(Tennis_table,sCORE(5,player1_id,player2_id,tourney_date))
# TEST<-Tennis_table[, yo:=lappy(.SD,sum)]
# test_func<-Tennis_table[annee==2017,RateScore:=lapply(.SD,sCORE(5,player1_id,player2_id,tourney_date))]

#Ou alors faire un vecteur à l'arrache et faire une left join sur player1_id player2_id et tourney_date

# 
# Tennis_table[player1==]
#   (sum(case when w_id=player1 then 1 else 0)/nrow(temp)) as RatioVictPlayer1
# temp2<-sqldf("SELECT    player1_id,
#                         player2_id,
#                         sum(case when w_id=player1 then 1 else 0) as VictPlayer1,
#                         sum(case when w_id=player2 then 1 else 0) as VictPlayer2
# 
#                from temp;")
  #df2.lastname, df1.var1, df2.state FROM df1 INNER JOIN df2 ON df1.personid = df2.id WHERE df2.state = 'TX'")
  #sqldf("SELECT df2.firstname, df2.lastname, df1.var1, df2.state FROM df1 INNER JOIN df2 ON df1.personid = df2.id WHERE df2.state = 'TX'")
  
#}
#n derni?res confrontations sur cette surface
n=7
ratioPlayer1Surface<-c(rep(0.5,nrow(Tennis_table)))
ratioPlayer2Surface<-c(rep(0,nrow(Tennis_table)))
NbrVictPlayer1Surface<-c(rep(0,nrow(Tennis_table)))
NbrVictPlayer2Surface<-c(rep(0,nrow(Tennis_table)))
ratioPlayer1Surface_<-c(rep(0,nrow(Tennis_table)))
ratioPlayer2Surface_<-c(rep(0,nrow(Tennis_table)))
NbrVictPlayer1Surface_<-c(rep(0,nrow(Tennis_table)))
NbrVictPlayer2Surface_<-c(rep(0,nrow(Tennis_table)))
for (i in 1:nrow(Tennis_table)) {
  
  date_i<-Tennis_table$tourney_date[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  surfaceJeu<-Tennis_table$surface[i]
  temp1<-setorder(Tennis_table[(tourney_date<date_i) | (tourney_date==date_i & round_num_i<round_num) & ((player1_id==player1)  | (player2_id==player1)
                               & (surface==surfaceJeu))],-tourney_date)[1:min(n,nrow(temp1))]
  temp2<-setorder(Tennis_table[(tourney_date<date_i) | (tourney_date==date_i & round_num_i<round_num) & ((player1_id==player2)  | (player2_id==player2)
                               & (surface==surfaceJeu))],-tourney_date)[1:min(n,nrow(temp1))]
  #amélioration
  ratioPlayer1Surface[i]<-mean(as.numeric(player1==temp1$w_id))
  ratioPlayer2Surface[i]<-mean(as.numeric(player2==temp2$w_id))
  NbrVictPlayer1Surface[i]<-sum(as.numeric(player1==temp1$w_id))
  NbrVictPlayer2Surface[i]<-sum(as.numeric(player2==temp2$w_id)) 
  
  ratioPlayer1Surface_[i]<-mean(as.numeric(player1==temp1$l_id))
  ratioPlayer2Surface_[i]<-mean(as.numeric(player2==temp2$l_id))
  NbrVictPlayer1Surface_[i]<-sum(as.numeric(player1==temp1$l_id))
  NbrVictPlayer2Surface_[i]<-sum(as.numeric(player2==temp2$l_id)) 
  
}
add<-cbind(add,ratioPlayer1Surface,ratioPlayer2Surface,NbrVictPlayer1Surface,NbrVictPlayer2Surface)
head(temp1)
NbrVictPlayer1Surface
NbrVictPlayer1Surface_
Tennis_table[,':='(ratioPlayer1Surface=ratioPlayer1Surface,ratioPlayer2Surface=ratioPlayer2Surface,
                   NbrVictPlayer1Surface=NbrVictPlayer1Surface,NbrVictPlayer2Surface=NbrVictPlayer2Surface)]
head(Tennis_table)
#10 derni?res confrontations sur toutes les sufraces 

n=10
ratioPlayer1last<-c(rep(0,nrow(Tennis_table)))
ratioPlayer2last<-c(rep(0,nrow(Tennis_table)))
NbrVictPlayer1last<-c(rep(0,nrow(Tennis_table)))
NbrVictPlayer2last<-c(rep(0,nrow(Tennis_table)))
for (i in 1:nrow(Tennis_table)) {
  
  date_i<-Tennis_table$tourney_date[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  
  temp1<-setorder(Tennis_table[(tourney_date<date_i) | (tourney_date==date_i & round_num_i<round_num) & ((player1_id==player1)  | (player2_id==player1))]
                  ,-tourney_date)[1:as.numeric(min(nrow(temp1),n))]
  temp2<-setorder(Tennis_table[(tourney_date<date_i) | (tourney_date==date_i & round_num_i<round_num) & ((player2_id==player2 | player1_id==player2))]
                  ,-tourney_date)[1:as.numeric(min(nrow(temp1),n))]
  
  ratioPlayer1last[i]<-mean(as.numeric(player1==temp1$w_id))
  ratioPlayer2last[i]<-mean(as.numeric(player2==temp2$w_id))
  NbrVictPlayer1last[i]<-sum(as.numeric(player1==temp1$w_id))
  NbrVictPlayer2last[i]<-sum(as.numeric(player2==temp2$w_id)) 
  
  # ratioPlayer1last_[i]<-mean(as.numeric(player1==temp1$l_id))
  # ratioPlayer2last_[i]<-mean(as.numeric(player2==temp2$l_id))
  # NbrVictPlayer1last_[i]<-sum(as.numeric(player1==temp1$l_id))
  # NbrVictPlayer2last_[i]<-sum(as.numeric(player2==temp2$l_id)) 
  
}
add<-cbind(add,ratioPlayer1last,ratioPlayer2last,NbrVictPlayer1last,NbrVictPlayer2last)
head(temp1)
ratioPlayer1last
Tennis_table[,':='(ratioPlayer1last=ratioPlayer1last,ratioPlayer2last=ratioPlayer2last,
                   NbrVictPlayer1last=NbrVictPlayer1last,NbrVictPlayer2last=NbrVictPlayer2last)]
head(Tennis_table)



#Surface de pr?dilection 
#Taux de victoire par tournoi Greg

sqldf("SELECT DISTINCT tourney_id, tourney_name, w_id
      FROM Tennis_table 
      GROUP BY tourney_id
      WHERE w_id not in (SELECT l_id  FROM Tennis_table GROUP BY tourney_id)
      ")

l<-sqldf("SELECT DISTINCT tourney_id, tourney_name, l_id
      FROM Tennis_table 
      GROUP BY tourney_id")
w<-sqldf("SELECT DISTINCT tourney_id, tourney_name, w_id
      FROM Tennis_table 
             GROUP BY tourney_id")
Tournoi <-as.data.table(sqldf("SELECT DISTINCT tourney_id, tourney_name
      FROM Tennis_table"))
X=nrow(Tournoi)
Y=3
Hist_w<-data.frame(matrix(NA,ncol=Y,nrow=X))
#Transfoermmer les 2 dernieres colonnes en alphanumeriques
colnames(Hist_w)<-c("w_id", "tourney_id", "tourney_name")
Hist_w$tourney_id<-as.character(Hist_w$tourney_id)
Hist_w$tourney_name<-as.character(Hist_w$tourney_name)
for (i in 1:nrow(Tournoi)){
  #i=1
  Tourney<-as.data.frame(Tournoi[i,1])
  #Tourney[1]
  colnames(Tourney)<-c("id")
  temp<-sqldf("SELECT  w_id, tourney_id, tourney_name
      FROM w
        WHERE tourney_id in (SELECT id from  Tourney)
        and w_id NOT IN (select l_id from loser where tourney_id in (SELECT id from  Tourney)) ")
  temp$tourney_id <- as.character( temp$tourney_id)
  temp$tourney_name <- as.character( temp$tourney_name)
  #temp
  Hist_w[i,'w_id']<-temp[1,'w_id']
  Hist_w[i,'tourney_id']<-temp[1,'tourney_id']
  Hist_w[i,'tourney_name']<-temp[1,'tourney_name']
}
head(Hist_w)
dim(Hist_w)

#concevoir l'utilisation
WinRateByTourney1<-c(seq(0,nrow(Tennis_table)))
WinRateByTourney2<-c(seq(0,nrow(Tennis_table)))
for (i in (1:nrow(Tennis_table))){
  
  date<-Tennis_table$tourney_date[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  TournoiName<-Tennis_table$tourney_name[i]
  temp1<-setorder(Tennis_table[(tourney_date<date) & (player1_id==player1 | player2_id==player1) & tourney_name==TournoiName] 
                  ,-tourney_date)[1:as.numeric(min(nrow(temp1),n))]
  temp2<-setorder(Tennis_table[(tourney_date<date) & (player2_id==player2 | player1_id==player2) & tourney_name==TournoiName]
                  ,-tourney_date)[1:as.numeric(min(nrow(temp2),n))] 
  WinRateByTourney1[i]<-mean(as.numeric(player1==temp1$w_id))
  WinRateByTourney2[i]<-mean(as.numeric(player2==temp2$w_id))
}



dim(WinRateByTourney1)
head(WinRateByTourney1)
head(Tennis_table)
#retirer les qualifs ? 
  #faire un compte sur le nombre de victoire diviser par le nombre de match, prendre en compte les qualifs ?
  #Traduction de round
#Taux de break Greg
 #head to head 
X=nrow(Tennis_table)
Y=4
Bp1<-data.frame(matrix(NA,ncol=Y,nrow=X))
Bp2<-data.frame(matrix(NA,ncol=Y,nrow=X))
colnames(Bp1)<-c("SumbpFaced", "NbrbpFacedMoyen", "SumbpSaved","BpSavedRate")
colnames(Bp2)<-c("SumbpFaced", "NbrbpFacedMoyen", "SumbpSaved","BpSavedRate")
save.image("Tennis_save.RData")
load("Tennis_save.RData")
#PROBLEME ICI
for (i in (1:nrow(Tennis_table))){
  i<-5
  date<-Tennis_table$tourney_date[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  temp<-setorder(Tennis_table[(tourney_date<date) & (player1_id==player1 | player2_id==player1) & (player1_id==player2 | player2_id==player2),
                              ':='(player1=player1, player2=player2)] 
                  ,-tourney_date)[1:as.numeric(min(nrow(temp1),n))]

  sqldf("SELECT sum( case when player1_id==player1 then player1_bpFacedn
                                            when player2_id==player1 then player2_bpFacedn end) as SumbpFaced,
                                  sum( case when player1_id==player1 then player1_bpFacedn
                                            when player2_id==player1 then player2_bpFacedn end)/count(player1_id) as NbrbpFacedMoyen,
                                  sum( case when player1_id==player1 then player1_bpSaved
                                            when player2_id==player1 then player2_bpSaved end) as SumbpSaved,
                                  sum( case when player1_id==player1 then player1_bpSaved
                                            when player2_id==player1 then player2_bpSaved end)/sum( case when player1_id==player1 then player1_bpFacedn
                                            when player2_id==player1 then player2_bpFacedn end) as BpSavedRate
      FROM temp 
      WHERE player1_id==player1 | player2_id==player1")
  RateBreakHdH2[i]<-sqldf("SELECT sum()/count(player1_id)
                              FROM temp 
                              WHERE player1_id==player2 | player2_id==player2")
  RateBreakSuccedHdH1<-sqldf("SELECT sum()
                            FROM temp 
      WHERE player1_id==player1 | player2_id==player1")/RateBreakHdH1[i]
  RateBreakSuccedHdH2<-sqldf("SELECT sum()
                            FROM temp 
                             WHERE player1_id==player2 | player2_id==player2")/RateBreakHdH2[i]
}

#Réadaptr pour taux de break

for (i in 1:nrow(Tennis_table)) {
  
  date_i<-Tennis_table$tourney_date[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  round_num_i<-Tennis_table$round_num[i]
  temp<-setorder(Tennis_table[(tourney_date<date_i) | (tourney_date==date_i & round_num_i<round_num) & 
                                ((player1_id==player1 & player2_id ==player2) |
                                   (player1_id==player2 & player2_id ==player1))],-tourney_date)
  
  temp<-temp[1:as.numeric(min(n,nrow(temp)))]
  
  Nbrmatch[i]<-nrow(temp)
  #Nbrmatch[i]
  ratioPlayer1[i]<-mean(as.numeric(player1==temp$w_id))
  #ratioPlayer1[i]
  ratioPlayer2[i]<-mean(as.numeric(player2==temp$w_id))
  # ratioPlayer2[i]
  NbrVictPlayer1[i]<-sum(as.numeric(player1==temp$w_id))
  NbrVictPlayer2[i]<-sum(as.numeric(player2==temp$w_id)) 
  
}
add<-cbind(add,ratioPlayer1,ratioPlayer2,NbrVictPlayer1,NbrVictPlayer2)
#recup le données du joueurs
#prendre sur un nombre de match fini -> 50 ? 
#mettre le joueur en player 1 ou faire boucle if pour prendre les données du joueurs concernés
#Calculer le nombre de match, le nombre de balle de break e le nombre de balle de break converti 
HeadToHead
n derni?res confrontations sur cette surface
#10 derni?res confrontations sur toutes les sufraces 


#sqldf("SELECT df2.firstname, df2.lastname, df1.var1, df2.state FROM df1 INNER JOIN df2 ON df1.personid = df2.id WHERE df2.state = 'TX'")
#mettre joueur 1 en i_name et jour 2 en opposant
f_Headtohead <- function(Tennis_table_work,i_name,i_date=NULL,i_surface=NULL,i_tournament=NULL,i_opponent=NULL,i_row=NULL,i_duree=NULL, i_round=NULL, i_matchnum=NULL) {
  historique=rbind(Tennis_table_work[(w_name==i_name & l_name==i_opponent),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=l_name)],
                   Tennis_table_work[(l_name==i_name & w_name==i_opponent),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=w_name)])[order(-tourney_date)]
  if (!is.null(i_date)) {historique=historique[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
  
  print(historique)#AJOUTER match_num<match_num ou round<i_round pour les tourney_date=i_date
  #OU i_date==tourney_date&round_num>i_round F=1 SF=2 QF=3 etc OU i_round=='RR' i_matchnum<match_num
  #if ((!is.null(i_date))&(!is.null(i_duree))) {historique=historique[difftime(anydate(i_date),tourney_date)<=i_duree]}
  #if (!is.null(i_surface)) {historique=historique[surface==i_surface]}
  #if (!is.null(i_opponent)) {historique=historique[opponent==i_opponent]}
  if (!is.null(i_row)) {historique=historique[,head(.SD,i_row)]}
  Historique_Mean=historique[,.(w_name)][,lapply(.SD, function(x) mean(as.numeric((i_name==w_name))))]
  return(Historique_Mean)
}
test<-f_Headtohead(Tennis_table_work,i_name = "Rafael Nadal",i_opponent = "Roger Federer",i_date = "2018-01-01",i_round=7, i_row=5,i_match=16)
test
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
test<-f_VicT(Tennis_table_work,i_name="Rafael Nadal",i_opponent="Roger Federer",i_date = "2018-05-05",i_round=7, i_row=10,i_surface="Clay")
test

# f_VicTSurface <- function(Tennis_table_work,i_name,i_date=NULL,i_surface=NULL,i_tournament=NULL,i_opponent=NULL,i_row=NULL,i_duree=NULL, i_round=NULL, i_matchnum=NULL) {
#   historique1=Tennis_table_work[((w_name==i_name | l_name==i_name) & surface==i_surface),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=l_name)][order(-tourney_date)]
#   historique2=Tennis_table_work[((w_name==i_opponent | l_name==i_opponent) & surface==i_surface),.(w_name,tourney_date,tourney_name,surface,match_num,round_num,opponent=w_name)][order(-tourney_date)]
#   if (!is.null(i_date)) {historique1=historique1[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
#   #AJOUTER match_num<match_num ou round<i_round pour les tourney_date=i_date
#   #OU i_date==tourney_date&round_num>i_round F=1 SF=2 QF=3 etc OU i_round=='RR' i_matchnum<match_num
#   if (!is.null(i_surface)) {historique1=historique1[surface==i_surface]}
#   if (!is.null(i_row)) {historique1=historique1[,head(.SD,i_row)]}
#   if (!is.null(i_date)) {historique1=historique1[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
#   #AJOUTER match_num<match_num ou round<i_round pour les tourney_date=i_date
#   #OU i_date==tourney_date&round_num>i_round F=1 SF=2 QF=3 etc OU i_round=='RR' i_matchnum<match_num
#   if (!is.null(i_date)) {historique2=historique2[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
#   if (!is.null(i_surface)) {historique2=historique2[surface==i_surface]}
#   if (!is.null(i_row)) {historique2=historique2[,head(.SD,i_row)]}
#   if (!is.null(i_date)) {historique2=historique2[tourney_date<anydate(i_date)|(anydate(i_date)==tourney_date&round_num>i_round)|(anydate(i_date)==tourney_date&i_round>7&match_num<i_matchnum)]}
#   
#   Historique_Mean1=historique1[,.(w_name)][,lapply(.SD, function(x) mean(as.numeric((i_name==w_name))))]
#   Historique_Mean2=historique2[,.(w_name)][,lapply(.SD, function(x) mean(as.numeric((i_opponent==w_name))))]
#   Historique_Mean1_<-ifelse(is.nan(as.numeric(Historique_Mean1)),0,as.numeric(Historique_Mean1)) 
#   <-ifelse(is.nan(as.numeric(Historique_Mean2)),0,as.numeric(Historique_Mean1))
#   Historique_Mean=Historique_Mean1_-Historique_Mean2_
#   return(Historique_Mean)
# }
test
###
###