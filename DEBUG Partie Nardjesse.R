

#1
f_NombreTitre <- function (Tennis_table, i_name, i_date) { 
  
  NbTitre <- Tennis_table[round=='F'][w_name == i_name][tourney_date<anydate(i_date)][,.(n_vic = .N)][head(1),c(ifelse('n_vic'!="",n_vic,0))]
  
  retour=ifelse(is.numeric(NbTitre),NbTitre,0)
  return(retour)
  
}

#2
f_NombreTitreSaison <- function(Tennis_table, i_name, i_date) {
  
  NbTitreSaison <- Tennis_table[round=='F'][w_name == i_name][year(tourney_date)==year(i_date)][,.(n_vic2 = .N)][head(1),c(ifelse('n_vic2'!="",n_vic2,0))]
  
  retour2=ifelse(is.numeric(NbTitreSaison),NbTitreSaison,0)
  return(retour2)
  
}

#3
f_NombreVictoire <- function(Tennis_table, i_name, i_date, i_match_num, i_round) {
  
  NbVIctoire <- Tennis_table[w_name==i_name][tourney_date<anydate(i_date) |tourney_date == anydate(i_date) & round_num > i_round |tourney_date == anydate(i_date) & i_round > 7 & match_num < i_match_num][,.(n_vic3 = .N)][head(1),c(ifelse('n_vic3'!="",n_vic3,0))]
  
  retour3=ifelse(is.numeric(NbVIctoire),NbVIctoire,0)
  return(retour3)
    
}

#4
f_NombreVictoireSaisonPrecedente <- function(Tennis_table, i_name, i_date) {
  
  NbVIcSaisonPrec <- Tennis_table[w_name==i_name][year(tourney_date) == year(i_date)-1][,.(n_vic4 = .N)][head(1),c(ifelse('n_vic4'!="",n_vic4,0))]
    
  retour4=ifelse(is.numeric(NbVIcSaisonPrec),NbVIcSaisonPrec,0)
  return(retour4)
  
}

#5
f_TerrainPredilection <- function(Tennis_table, i_name, i_date, i_match_num, i_round){
  
  tmp2 <- Tennis_table %>% rename(Nom = w_name) %>% 
    
    filter(Nom ==i_name,(tourney_date<anydate(i_date) |tourney_date == anydate(i_date) & round_num > i_round |tourney_date == anydate(i_date) & i_round > 7 & match_num < i_match_num) ) %>% 
    
    group_by(Nom,surface) %>%
    
    summarise(NbVicTerrain = n())%>% arrange(Nom)  
  
  tmp3 <- Tennis_table %>% rename(Nom = l_name) %>% 
    
    filter(Nom ==i_name,tourney_date<anydate(i_date) ) %>% group_by(Nom,surface) %>% 
    
    summarise(NbDefTerrain = n())%>% arrange(Nom) 
  
  
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





table_scoreV2 <- table_score[78225:78325,]
table_scoreV2[,c('NbTitre'):=c(f_NombreTitre(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitre(Tennis_table_work,i_name=p2_name,i_date=tourney_date)),by=c(colnames(table_scoreV2))][,c('NbTitreSaison') := c(f_NombreTitreSaison(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreTitreSaison(Tennis_table_work,i_name=p2_name,i_date=tourney_date)),by=c(colnames(table_scoreV2))][,c('NbVictoire') := c(f_NombreVictoire(Tennis_table_work,i_name=p1_name,i_round=round_num,i_match_num=match_num,i_date=tourney_date)-f_NombreVictoire(Tennis_table_work,i_name=p2_name,i_round=round_num,i_match_num=match_num,i_date=tourney_date)),by=c(colnames(table_scoreV2))][,c('NbVictoireSaisonPrec'):=c(f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p1_name,i_date=tourney_date)-f_NombreVictoireSaisonPrecedente(Tennis_table_work,i_name=p2_name,i_date=tourney_date)),by=c(colnames(table_scoreV2))][,c('SurfacePred'):=c(f_surface(Tennis_table_work,i_name=p1_name,i_date=tourney_date,i_round=round_num,i_match_num=match_num,i_surface=surface)-f_surface(Tennis_table_work,i_name=p2_name,i_date=tourney_date,i_round=round_num,i_match_num=match_num,i_surface=surface)),by=c(colnames(table_scoreV2))]

