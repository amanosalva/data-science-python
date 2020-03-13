rm(list=ls())
#################################################################################
####### PROGRAMA DE ESPECIALIZACION EN DATA SCIENCE NIVEL II ####################
#################################################################################

############################################################################
# PROFESOR : ANDRÉ CHÁVEZ - JOSÉ CÁRDENAS
############################################################################

## EJERCICIO DE APLICACION : PREDICTOR DE INGRESOS ##

library(reshape2)
library(partykit)
library(randomForest)
library(adabag)
library(gbm) #install.packages("gbm")


# Leemos la informacion de clientes #
data<-read.table("data_loan_status_limpia.csv",header=T,
                      sep=",",fill=T)

# Generalmente partimos en train y test, y debemos considerar filtros o criterios e exclusion #
sample=sample.int(nrow(data),round(0.1*nrow(data)))
df_training<-data[-sample,]
df_testing<-data[sample,]

#TECNICA DM1: REGRESION LINEAL
lm_1 <- lm(ApplicantIncome~.,
           data=df_training)
# Prediccion sobre la datatest del modelo entrenado
df_testing$IngresoPred_lm<-predict(lm_1,df_testing,type="response")

#TECNICA DM2: ARBOL DE DECISION
n=nrow(df_training)
tree_1<-partykit::ctree(ApplicantIncome~.,
              data=df_training,
              control=ctree_control(mincriterion = .95,minsplit = .1*n,
              minbucket = .05))

df_testing$IngresoPred_tree<-predict(tree_1,df_testing,type="response")

#TECNICA DM3: RANDOM FORESTS 
rf_1 <- randomForest(ApplicantIncome~.,
                    data=df_training, ntree=50, mtry = 3, importance = TRUE)

df_testing$IngresoPred_rf<-predict(rf_1,df_testing,type="response")


#COMPARACION GRAFICA DE LOS PREDICTORES:

df_testing$X=seq(1:nrow(df_testing))

#rm(df_temp)
df_temp<-melt(df_testing[,c("X","ApplicantIncome",
                            "IngresoPred_tree","IngresoPred_lm",
                            "IngresoPred_rf"
                            )],
              id="X")
ggplot()+
  geom_density(data=df_temp, aes(value,color=variable, alpha = 0.2))+
  scale_x_continuous(limits = c(500, 15000))

#Error Cuadratico Medio de los Predictores (ECM)
ECM1<-sum((df_testing$IngresoPred_tree-df_testing$ApplicantIncome)^2)/nrow(df_testing)
ECM2<-sum((df_testing$IngresoPred_lm-df_testing$IngresoReal)^2)/nrow(df_testing)
ECM3<-sum((df_testing$IngresoPred_rf-df_testing$IngresoReal)^2)/nrow(df_testing)

# Comparamos los predictores, el de menor ECM es el más preciso.
ECM1
ECM2
ECM3



  