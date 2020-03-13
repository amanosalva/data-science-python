rm(list=ls())
#################################################################################
####### PROGRAMA DE ESPECIALIZACION EN DATA SCIENCE NIVEL II ####################
#################################################################################

############################################################################
# PROFESOR : ANDRÉ CHÁVEZ - JOSÉ CÁRDENAS
############################################################################

## EJERCICIO DE APLICACION : FRAUDE TRANSACCIONAL ##

library(data.table)
library(class)
library(e1071)
library(partykit)
library(adabag)


#rm(df_file02)
df_file02<-read.table("df_FraudeTransaccional.txt",header=T,
                      sep=",",fill=T)

df_file02$Fraud <- factor(df_file02$Fraud, levels=c(0,1),
                          labels=c("No Fraude","Fraude"))
table(df_file02$Fraud)
#Data Training
df_training<-df_file02[df_file02$Flag_Muestra==1,-2]
#Data Testing
df_testing<-df_file02[df_file02$Flag_Muestra==0,-2]

## I. KNN:

#I.1 Modelo:
#k=sqrt(nrow(df_training))
colnames(df_training)
knn_1 <- knn(train=df_training[,c(3:8)],test=df_testing[,c(3:8)],
             cl=df_training$Fraud,k=100)

#I.2 Predictor en la data
df_testing$Fraud_knn<-knn_1

#I.3 Matriz de Confusi?n
#Valores Absolutos:
conf_knn<-table(df_testing$Fraud,df_testing$Fraud_knn,
                dnn =c("Real","Prediccion"))

#Valores Relativos:
conf_knn_porc<-round(100*conf_knn/rowSums(conf_knn),2)

## II. NAIVE BAYES
#help("naiveBayes")

nb_1 <- naiveBayes(Fraud~Frec_Grupo_dias+Rec_Grupo_Horas+Frec_Cliente_dias+
                     Var_MontoTrx_MontoProm+Frec_Comercio_dias+
                     diff_TiempoTrxs_RecCliente,
                   data=df_training)

##Output Naive Bayes: Probabilidad y Clase:
prob_nb <- predict(nb_1,df_testing,type="raw")
class_nb<- predict(nb_1,df_testing,type="class")

##II.2 Predictor en la data: Probabilidad y Clase:
df_testing$Fraud_nb <- class_nb

#II.3 Matriz de Confusion
#Valores Absolutos:
conf_nb<-table(df_testing$Fraud,df_testing$Fraud_nb,
                  dnn =c("Real","Prediccion"))
#Valores Relativos:
conf_nb_porc<-round(100*conf_nb/rowSums(conf_nb),2)
