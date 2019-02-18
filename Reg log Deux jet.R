## Pr?paration des tables train et test


library(lubridate)
#library(anytime)
library(data.table)
load('C:/Users/nbensmina/Desktop/PROJET/table_score.RData')


training_table <- table_score[year(table_score$tourney_date)<2018,]
test_table <- table_score[year(table_score$tourney_date)==2018,]




training_table[,c("tourney_id","tourney_name","surface","tourney_date","tourney_level","match_num","round","round_num","p1_id","p1_name","p2_id","p2_name") := NULL]
test_table[,c("tourney_id","tourney_name","surface","tourney_date","tourney_level","match_num","round","round_num","p1_id","p1_name","p2_id","p2_name") := NULL]

training_table[,cible:=as.factor(coin)]
training_table[,coin := NULL]

f_replaceNA = function(DT) {
  
  for (j in seq_len(ncol(DT)))
    
    set(DT,which(is.na(DT[[j]])),j,0)
  
}

f_replaceNA(training_table)
f_replaceNA(test_table)

#Mod?lisation
library(glmnet)


Xtrain <- as.matrix(training_table[,-64])
Ytrain <- as.matrix(training_table[,64])
Xtest <- as.matrix(test_table[,-1])

ERR <- data.frame(RegLog=0,Ridge1=0,Ridge2=0,Lasso=0,ElasticNet=0)


mod <- glmnet(Xtrain,Ytrain, family='binomial')
names(mod)
mod$lambda
mod$a0

#1) sans param?tre de r?gularisation : R?gression logistique
mod1 <- glmnet(Xtrain,Ytrain, family='binomial',standardize = TRUE,lambda=0, alpha=0) # <------------ Reg log
print(mod1)
names(mod1)

pred1 <- predict(mod1,Xtest, type='class', s=c(0)) # <--------- Pred Reg log
print(table(pred1))
print(sum(test_table$coin != pred1)/nrow(test_table))
ERR$RegLog <- sum(test_table$coin != pred1)/nrow(test_table)
print(ERR)



#2) Ridge
mod2 <- glmnet(Xtrain,Ytrain,family='binomial',alpha=0)
names(mod2)
print(mod2)
plot(mod2, xvar='lambda')

set.seed(1234)
cv_mod2 <- cv.glmnet(Xtrain,Ytrain,family='binomial',type.measure='class',nfolds=10,alpha=0,keep=TRUE)
plot(cv_mod2)
names(cv_mod2)
min(cv_mod2$cvm) #Erreur minimale
cv_mod2$lambda.min # Lambda correspondant
cv_mod2$lambda.1se
pred2 <- predict(cv_mod2,Xtest,s=c(cv_mod2$lambda.min,cv_mod2$lambda.1se),type='class')
print(sum(test_table$coin != pred2[,1])/nrow(test_table))
print(sum(test_table$coin != pred2[,2])/nrow(test_table))
ERR$Ridge1 <- sum(test_table$coin != pred2[,1])/nrow(test_table)
ERR$Ridge2 <- sum(test_table$coin != pred2[,2])/nrow(test_table)
ERR


#3) Elasticnet
mod3 <- glmnet(Xtrain,Ytrain,family='binomial', alpha=0.5)
cv_mod3 <- cv.glmnet(Xtrain,Ytrain,family='binomial',type.measure='class',nfolds=10,alpha=0.5,foldid=cv_mod2$foldid) 
pred3 <- predict(mod3,Xtest,type='class',s=c(cv_mod3$lambda.min)) #<------ Pourquoi Eric r?cup?re le lambda.min de la ridge (dans son exemple ? lui)?
print(sum(test_table$coin != pred3)/nrow(test_table))

ERR$ElasticNet <- sum(test_table$coin != pred3)/nrow(test_table)
ERR

#4)Lasso
mod4 <- glmnet(Xtrain,Ytrain,family='binomial',alpha=1)
cv_mod4 <- cv.glmnet(Xtrain,Ytrain,family='binomial',type.measure='class',nfolds=10,alpha=1,keep=TRUE)
pred4 <- predict(cv_mod4,Xtest,type='class',s=c(cv_mod4$lambda.min))
pred4_bis <- predict(cv_mod4,Xtest,type='class',lambda=cv_mod4$lambda.min) #M?thode Eric : lambda='' au lieu de s=c() /avec cette m?thode l'erreur de prediction est l?g?rement sup
print(sum(test_table$coin != pred4)/nrow(test_table))

ERR$Lasso <- sum(test_table$coin != pred4)/nrow(test_table)

ERR #Le lasso l'emporte (de peu sur l'elasticnet la reg log) : 12,3% d'erreur

table(pred1)
table(pred2[,1])
table(pred2[,2])
table(pred3)
table(pred4)

print(cbind(mod4$lambda,mod4$df))
cv_mod4$lambda.min
mod3$beta[,25]
print(mod4$beta[abs(mod4$beta[,35])>0,35]) #Variables selectionn?es par la 35e valeur de lambda


