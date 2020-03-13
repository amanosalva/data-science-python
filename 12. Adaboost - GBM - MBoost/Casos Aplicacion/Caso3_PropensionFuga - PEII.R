rm(list=ls())
#################################################################################
####### PROGRAMA DE ESPECIALIZACION EN DATA SCIENCE NIVEL II ####################
#################################################################################

############################################################################
# PROFESOR : ANDRÉ CHÁVEZ - JOSÉ CÁRDENAS
############################################################################

## EJERCICIO DE APLICACION : PROPENSION DE FUGA ##

library(plyr)
library(class)
library(partykit)
library(randomForest)
library(class)
library(e1071)

###ABRIENDO EL ARCHIVO DE TRABAJO:

df_file03<-read.table("df_PropensionFuga.txt",header=T,sep=",")

#Flag_Muestra: 1=Training 0=Testing
df_file03$Flag_Muestra<-rbinom(nrow(df_file03),1,0.75)

#Categorizando la variable Target:
df_file03$Fuga<-factor(df_file03$Fuga, levels=c(0,1),labels=c("No Fuga","Fuga"))


#Utilizacion TC:
df_file03$UtilizacionTC <- 100*df_file03$SaldoTotal/df_file03$LineadeCredito

#Recencia vs Frecuencia:
df_file03$RyF <- df_file03$Recency_Global/df_file03$Frecuency_Global

df_training <- df_file03[df_file03$Flag_Muestra==1,]
df_testing <- df_file03[df_file03$Flag_Muestra==0,]

##I. TREE (ALGORITMO CTREE)

n=nrow(df_training)
tree1 <- partykit::ctree( Fuga ~ UtilizacionTC+RyF+
                            BehaviorScoring+MonetaryValue_Global,
                          data = df_training, 
                          control=ctree_control(mincriterion = .95, minsplit = .1*n,
                                                minbucket = .05*n, maxdepth = 5)
)

df_testing$prob_tree <- predict(tree1, df_testing, type="prob")
df_testing$Fuga_tree <- predict(tree1, df_testing, type="response")

#I.3 Matriz de Confusi?n
#Valores Absolutos:
conf_tree<-table(df_testing$Fuga,df_testing$Fuga_tree,
                 dnn =c("Real","Prediccion"))
#Valores Relativos:
conf_tree_porc<-round(100*conf_tree/rowSums(conf_tree),2)

##IV. RANDOM FOREST
#install.packages("randonForest")

rf1 <- randomForest( Fuga ~ UtilizacionTC+RyF+BehaviorScoring+
                       MonetaryValue_Global,
                     data = df_training, ntree=500, mtry = 2, importance = TRUE,
                     do.trace = 10)
#print(rf1)

df_testing$prob_rf <- predict(rf1, df_testing, type="prob")
df_testing$Fuga_rf <- predict(rf1, df_testing, type="response")

#Matriz de confusion:
#Valores Absolutos:
conf_rf<-table(df_testing$Fuga,df_testing$Fuga_rf,
                 dnn =c("Real","Prediccion"))
#Valores Relativos:
conf_rf_porc<-round(100*conf_rf/rowSums(conf_rf),2)

importance(rf1)
varImpPlot(rf1)

## K NEAREST NEIGHBOR (KNN)

#k=sqrt(nrow(df_training))
knn1 <- knn(df_training[,c(3,7,10,11)] , df_testing[,c(3,7,10,11)],
            cl=df_training$Fuga, k=10)

df_testing$Fuga_knn <- knn1

#Matriz de confusion:
#Valores Absolutos:
conf_knn<-table(df_testing$Fuga,df_testing$Fuga_knn,
               dnn =c("Real","Prediccion"))
#Valores Relativos:
conf_knn_porc<-round(100*conf_knn/rowSums(conf_knn),2)

#SVM

svm_1<-svm(Fuga ~ UtilizacionTC+RyF+BehaviorScoring+
             MonetaryValue_Global,
           data=df_training,kernel="linear", cost=1, scale = F)

pred<-predict(svm_1,df_testing)

df_testing$Fuga_svm<-pred

#Matriz de confusion:
#Valores Absolutos:
conf_svm<-table(df_testing$Fuga,df_testing$Fuga_svm,
                dnn =c("Real","Prediccion"))
conf_svm
#Valores Relativos:
conf_svm_porc<-round(100*conf_svm/rowSums(conf_svm),2)



