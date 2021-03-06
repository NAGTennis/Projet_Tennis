install.packages("data.table")
library(data.table)
install.packages("stringr")
library(stringr)
install.packages("sqldf")
library(sqldf)

#Mis en place de l'environnement de travail
setwd("C:/Users/Greg/Documents/Certificat Data Science/Projet/BDD/tennis_atp-master")
setwd("//FRSHARES0250.france.intra.corp/Planif_users/b06713/Data Science ENSAE/Certificat Data Science/tennis_atp-master")
#test2
#Import et premier retraitement sur le nom des variables

tennis2017<- read.csv(file ='atp_matches_2017.csv' ,header = T)
tennis2016<- read.csv(file ='atp_matches_2016.csv' ,header = T)
tennis2015<- read.csv(file ='atp_matches_2015.csv' , header = T)
tennis2014<- read.csv(file ='atp_matches_2014.csv' , header = T)
tennis2013<- read.csv(file ='atp_matches_2013.csv' , header = T)
tennis2012<- read.csv(file ='atp_matches_2012.csv' , header = T)
tennis2011<- read.csv(file ='atp_matches_2011.csv' , header = T)
tennis2010<- read.csv(file ='atp_matches_2010.csv' , header = T)

Tennis_hist<-read.csv(file='atp_matches.csv', header = T)
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
Tennis_hist_table<-as.data.table(Tennis_hist)
Tennis_hist_table[order(tourney_id,tourney_date,match_num),]

#Cr?ation d'un RankingRate : permet d'avoir le rang en taux, en fonction du rang 
#le plus haut et du rang le plus bas dans un tournoi
Max<-Tennis_table[, MaxRankPointWin := max(max(winner_rank_points),max(loser_rank_points))][,MaxRankPointWin]
Min<- Tennis_table[, MinRank:=min(max(winner_rank),min(loser_rank)),by=tourney_id]
Tennis_table[,':='(RankingRateWin=winner_rank_points/MaxRankPointWin,
                       RankingRateLose=loser_rank_points/MaxRankPointWin)]

#Cr?ation du nombre de jeu et du nombre de Set


# comprendre gsub http://www.endmemo.com/program/R/gsub.php 
Tennis_table[,Score_v2:=gsub("\\(\\d\\)","",score)]
Tennis_table[,':='(Score_v2=gsub("\\(\\d\\)","",score),
                      NombreJeu=sum(as.numeric(unlist((str_extract_all(Score_v2, "[0-9]+")))))),
                 by=.(tourney_id, match_num, tourney_date)]

#Tennis2017_table[,':='(NombreJeu=sum(as.numeric(unlist(Score_v2)))), by=.(tourney_id, match_num, tourney_date)]
head(Tennis_table)

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
for (i in 1:nrow(Tennis_table)) {
  if (Coin[i] == 1) {
    Tennis_table[i,':='(player1_id=winner_id, player1_seed=winner_seed, player1_entry= winner_entry,player1_name=winner_name,player1_hand=winner_hand,player1_ht=winner_ht,player1_ioc=winner_ioc,
                            player1_age=winner_age,player1_rank=winner_rank,player1_rank_points=winner_rank_points,
                        player1_ace=w_ace,player1_df=w_df,player1_svpt=w_svpt,player1_1stIn=w_1stIn,player1_1stWon=w_1stWon,
                        player1_2ndWon=w_2ndWon,player1_SvGms=w_SvGms,player1_bpSaved=w_bpSaved,player1_bpFacedn=w_bpFaced,
                            player2_id=loser_id, player2_seed=loser_seed, player2_entry= loser_entry,player2_name=loser_name,player2_hand=loser_hand,player2_ht=loser_ht,player2_ioc=loser_ioc,
                            player2_age=loser_age,player2_rank=loser_rank,player2_rank_points=loser_rank_points,
                            player2_ace=l_ace,player2_df=l_df,player2_svpt=l_svpt,player2_1stIn=l_1stIn,player2_1stWon=l_1stWon,
                            player2_2ndWon=l_2ndWon,player2_SvGms=l_SvGms,player2_bpSaved=l_bpSaved,player2_bpFacedn=l_bpFaced),]
  }
if (Coin[i]==0 ){
  Tennis_table[i, ':='(player2_id=winner_id, player2_seed=winner_seed, player2_entry= winner_entry,player2_name=winner_name,player2_hand=winner_hand,player2_ht=winner_ht,player2_ioc=winner_ioc,
                       player2_age=winner_age,player2_rank=winner_rank,player2_rank_points=winner_rank_points,
                       player2_ace=w_ace,player2_df=w_df,player2_svpt=w_svpt,player2_1stIn=w_1stIn,player2_1stWon=w_1stWon,
                       player2_2ndWon=w_2ndWon,player2_SvGms=w_SvGms,player2_bpSaved=w_bpSaved,player2_bpFacedn=w_bpFaced,
                       player1_id=loser_id, player1_seed=loser_seed, player1_entry= loser_entry,player1_name=loser_name,player1_hand=loser_hand,player1_ht=loser_ht,player1_ioc=loser_ioc,
                       player1_age=loser_age,player1_rank=loser_rank,player1_rank_points=loser_rank_points,
                       player1_ace=l_ace,player1_df=l_df,player1_svpt=l_svpt,player1_1stIn=l_1stIn,player1_1stWon=l_1stWon,
                       player1_2ndWon=l_2ndWon,player1_SvGms=l_SvGms,player1_bpSaved=l_bpSaved,player1_bpFacedn=l_bpFaced),]
}
}

pb<-Tennis_table[is.na(player1_name)]
head(pb)
ok<-Tennis_table[!(is.na(player1_name))]
ok


#ReorgName<-function(x) {
 # Coin<- round((runif(nrow(Tennis_table),0,1)),0)
  
  #for (i in 1:nrow(Tennis_table)) 
   # if (Coin[i] == 1) {
    #  setnames(Tennis_table,
     #          old= c('winner_id', 'winner_seed', 'winner_entry','winner_name', 'winner_hand', 'winner_ht', 'winner_ioc',
      #                'winner_age', 'winner_rank','winner_rank_points','loser_id', 'loser_seed', 'loser_entry','loser_name',
       #               'loser_hand', 'loser_ht', 'loser_ioc', 'loser_age', 'loser_rank','loser_rank_points'),
        #       new = c('player1_id', 'player1_seed', 'player1_entry','player1_name', 'player1_hand', 'player1_ht', 
         #              'player1_ioc', 'player1_age', 'player1_rank','player1_rank_points','player2_id', 'player2_seed',
          #             'player2_entry','player2_name','player2_hand', 'player2_ht', 'player2_ioc', 'player2_age', 
           #            'player2_rank','player2_rank_points'))
      
#}
  #if (Coin[i]==0 ){
   # setnames(Tennis_table[i],
     #        c('winner_id', 'winner_seed', 'winner_entry','winner_name', 'winner_hand', 'winner_ht', 'winner_ioc',
      #         'winner_age', 'winner_rank','winner_rank_points','loser_id', 'loser_seed', 'loser_entry','loser_name',
       #        'loser_hand', 'loser_ht', 'loser_ioc', 'loser_age', 'loser_rank','loser_rank_points'),
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

mode(Tennis_table$tourney_date)
Tennis_table$tourney_date
Tennis_table[, tourney_date_char:=as.character(tourney_date)][,':='(annee=substr(tourney_date_char,1,4),
                                                                     mois=substr(tourney_date_char,5,6),
                                                                     jour=substr(tourney_date_char,7,nchar(tourney_date_char)))][,]
Tennis_table[,DateTourney:=as.Date(paste(jour,mois,annee,sep="/"),"%d/%m/%Y")]

head(Tennis_table)                       

#n derni?res confrontation, reflechir ? comment prendre en compte le n 
#compter le nombre de victoire 
n<-5
player1<-104745
player2<-105777
date<-as.Date("15/09/2017","%d/%m/%Y")
date
#1) Faire la fonction puis faire un Lapply => s'applique ? tout le jeu de donn?es
#2) Faire une boucle for pour avoir un vecteur de la taille de la table puis un cbind

# 
# Hist<-NULL
# 
# 
# sCORE<-function(n,player1,player2,date){
#   
#   temp<-setorder(Tennis_table[(DateTourney<date) & 
#                                     ((player1_id==player1 & player2_id ==player2) | (player1_id==player2 & player2_id ==player1))],-DateTourney)[1:n]
#                                     
# ratioPlayer1[i]<-mean(as.numeric(player1==temp$winner_id))
# ratioPlayer2[i]<-mean(as.numeric(player2==temp$winner_id))
# NbrVictPlayer1[i]<-sum(as.numeric(player1==temp$winner_id))
# NbrVictPlayer2[i]<-sum(as.numeric(player2==temp$winner_id)) 
# 
# 
# }
n=5
ratioPlayer1<-c(rep(-1,nrow(Tennis_table)))
ratioPlayer2<-c(rep(-1,nrow(Tennis_table)))
NbrVictPlayer1<-c(rep(-1,nrow(Tennis_table)))
NbrVictPlayer2<-c(rep(-1,nrow(Tennis_table)))
Nbrmatch<-c(rep(-1,nrow(Tennis_table)))
temp1<-temp
#Problème sur la boucle for, elle s'arrête à 78
for (i in 1:nrow(Tennis_table)) {

  date_i<-Tennis_table$DateTourney[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  
  temp<-setorder(Tennis_table[(DateTourney<date_i) & 
                                ((player1_id==player1 & player2_id ==player2) |
                                   (player1_id==player2 & player2_id ==player1))],-DateTourney)
  
  temp<-temp[1:as.numeric(min(n,nrow(temp)))]
  
  Nbrmatch[i]<-nrow(temp)
  #Nbrmatch[i]
  ratioPlayer1[i]<-mean(as.numeric(player1==temp$winner_id))
  #ratioPlayer1[i]
  ratioPlayer2[i]<-mean(as.numeric(player2==temp$winner_id))
 # ratioPlayer2[i]
  NbrVictPlayer1[i]<-sum(as.numeric(player1==temp$winner_id))
  NbrVictPlayer2[i]<-sum(as.numeric(player2==temp$winner_id)) 
  
}
NbrVictPlayer1
table(Nbrmatch)
Nbrmatch==-1
ratioPlayer1
NbrVictPlayer1
Tennis_table[,':='(ratioPlayer1=ratioPlayer1,ratioPlayer2=ratioPlayer2,NbrVictPlayer1=NbrVictPlayer1,NbrVictPlayer2=NbrVictPlayer2,Nbrmatch=Nbrmatch)]
head(Tennis_table)
pb1<-Tennis_table[(Nbrmatch==-1)]
test<-lapply(Tennis_table,sCORE(5,player1_id,player2_id,DateTourney))
TEST<-Tennis_table[, yo:=lappy(.SD,sum)]
test_func<-Tennis_table[annee==2017,RateScore:=lapply(.SD,sCORE(5,player1_id,player2_id,DateTourney))]

#Ou alors faire un vecteur à l'arrache et faire une left join sur player1_id player2_id et tourney_date

# 
# Tennis_table[player1==]
#   (sum(case when winner_id=player1 then 1 else 0)/nrow(temp)) as RatioVictPlayer1
# temp2<-sqldf("SELECT    player1_id,
#                         player2_id,
#                         sum(case when winner_id=player1 then 1 else 0) as VictPlayer1,
#                         sum(case when winner_id=player2 then 1 else 0) as VictPlayer2
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
  
  date_i<-Tennis_table$DateTourney[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  surfaceJeu<-Tennis_table$surface[i]
  
  temp1<-setorder(Tennis_table[(DateTourney<date) & (player1_id==player1  | player2_id==player1)
                               & surface==surfaceJeu],-DateTourney)[1:min(n,nrow(temp1))]
  temp2<-setorder(Tennis_table[(DateTourney<date) & (player2_id==player2 | player1_id==player2)
                               & surface==surfaceJeu],-DateTourney)[1:as.numeric(min(n,nrow(temp2)))]
  
  ratioPlayer1Surface[i]<-mean(as.numeric(player1==temp1$winner_id))
  ratioPlayer2Surface[i]<-mean(as.numeric(player2==temp2$winner_id))
  NbrVictPlayer1Surface[i]<-sum(as.numeric(player1==temp1$winner_id))
  NbrVictPlayer2Surface[i]<-sum(as.numeric(player2==temp2$winner_id)) 
  
  ratioPlayer1Surface_[i]<-mean(as.numeric(player1==temp1$loser_id))
  ratioPlayer2Surface_[i]<-mean(as.numeric(player2==temp2$loser_id))
  NbrVictPlayer1Surface_[i]<-sum(as.numeric(player1==temp1$loser_id))
  NbrVictPlayer2Surface_[i]<-sum(as.numeric(player2==temp2$loser_id)) 
  
}
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
  
  date_i<-Tennis_table$DateTourney[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  
  temp1<-setorder(Tennis_table[(DateTourney<date) & (player1_id==player1 | player2_id==player1)]
                  ,-DateTourney)[1:as.numeric(min(nrow(temp1),n))]
  temp2<-setorder(Tennis_table[(DateTourney<date) & (player2_id==player2 | player1_id==player2)]
                  ,-DateTourney)[1:as.numeric(min(nrow(temp1),n))]
  
  ratioPlayer1last[i]<-mean(as.numeric(player1==temp1$winner_id))
  ratioPlayer2last[i]<-mean(as.numeric(player2==temp2$winner_id))
  NbrVictPlayer1last[i]<-sum(as.numeric(player1==temp1$winner_id))
  NbrVictPlayer2last[i]<-sum(as.numeric(player2==temp2$winner_id)) 
  
  # ratioPlayer1last_[i]<-mean(as.numeric(player1==temp1$loser_id))
  # ratioPlayer2last_[i]<-mean(as.numeric(player2==temp2$loser_id))
  # NbrVictPlayer1last_[i]<-sum(as.numeric(player1==temp1$loser_id))
  # NbrVictPlayer2last_[i]<-sum(as.numeric(player2==temp2$loser_id)) 
  
}
head(temp1)
ratioPlayer1last
Tennis_table[,':='(ratioPlayer1last=ratioPlayer1last,ratioPlayer2last=ratioPlayer2last,
                   NbrVictPlayer1last=NbrVictPlayer1last,NbrVictPlayer2last=NbrVictPlayer2last)]
head(Tennis_table)



#Surface de pr?dilection 
#Taux de victoire par tournoi Greg

sqldf("SELECT DISTINCT tourney_id, tourney_name, winner_id
      FROM Tennis_table 
      GROUP BY tourney_id
      WHERE winner_id not in (SELECT loser_id  FROM Tennis_table GROUP BY tourney_id)
      ")

loser<-sqldf("SELECT DISTINCT tourney_id, tourney_name, loser_id
      FROM Tennis_table 
      GROUP BY tourney_id")
winner<-sqldf("SELECT DISTINCT tourney_id, tourney_name, winner_id
      FROM Tennis_table 
             GROUP BY tourney_id")
Tournoi <-as.data.table(sqldf("SELECT DISTINCT tourney_id, tourney_name
      FROM Tennis_table"))
X=nrow(Tournoi)
Y=3
Hist_Winner<-data.frame(matrix(NA,ncol=Y,nrow=X))
#Transfoermmer les 2 dernieres colonnes en alphanumeriques
colnames(Hist_Winner)<-c("winner_id", "tourney_id", "tourney_name")
Hist_Winner$tourney_id<-as.character(Hist_Winner$tourney_id)
Hist_Winner$tourney_name<-as.character(Hist_Winner$tourney_name)
for (i in 1:nrow(Tournoi)){
  #i=1
  Tourney<-as.data.frame(Tournoi[i,1])
  #Tourney[1]
  colnames(Tourney)<-c("id")
  temp<-sqldf("SELECT  winner_id, tourney_id, tourney_name
      FROM winner
        WHERE tourney_id in (SELECT id from  Tourney)
        and winner_id NOT IN (select loser_id from loser where tourney_id in (SELECT id from  Tourney)) ")
  temp$tourney_id <- as.character( temp$tourney_id)
  temp$tourney_name <- as.character( temp$tourney_name)
  #temp
  Hist_Winner[i,'winner_id']<-temp[1,'winner_id']
  Hist_Winner[i,'tourney_id']<-temp[1,'tourney_id']
  Hist_Winner[i,'tourney_name']<-temp[1,'tourney_name']
}
head(Hist_Winner)
dim(Hist_Winner)

#concevoir l'utilisation
WinRateByTourney1<-c(seq(0,nrow(Tennis_table)))
WinRateByTourney2<-c(seq(0,nrow(Tennis_table)))
for (i in (1:nrow(Tennis_table))){
  
  date<-Tennis_table$DateTourney[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  TournoiName<-Tennis_table$tourney_name[i]
  temp1<-setorder(Tennis_table[(DateTourney<date) & (player1_id==player1 | player2_id==player1) & tourney_name==TournoiName] 
                  ,-DateTourney)[1:as.numeric(min(nrow(temp1),n))]
  temp2<-setorder(Tennis_table[(DateTourney<date) & (player2_id==player2 | player1_id==player2) & tourney_name==TournoiName]
                  ,-DateTourney)[1:as.numeric(min(nrow(temp2),n))] 
  WinRateByTourney1[i]<-mean(as.numeric(player1==temp1$winner_id))
  WinRateByTourney2[i]<-mean(as.numeric(player2==temp2$winner_id))
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
  date<-Tennis_table$DateTourney[i]
  player1<-Tennis_table$player1_id[i]
  player2<-Tennis_table$player2_id[i]
  temp<-setorder(Tennis_table[(DateTourney<date) & (player1_id==player1 | player2_id==player1) & (player1_id==player2 | player2_id==player2),
                              ':='(player1=player1, player2=player2)] 
                  ,-DateTourney)[1:as.numeric(min(nrow(temp1),n))]

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


#recup le données du joueurs
#prendre sur un nombre de match fini -> 50 ? 
#mettre le joueur en player 1 ou faire boucle if pour prendre les données du joueurs concernés
#Calculer le nombre de match, le nombre de balle de break e le nombre de balle de break converti 


#sqldf("SELECT df2.firstname, df2.lastname, df1.var1, df2.state FROM df1 INNER JOIN df2 ON df1.personid = df2.id WHERE df2.state = 'TX'")

