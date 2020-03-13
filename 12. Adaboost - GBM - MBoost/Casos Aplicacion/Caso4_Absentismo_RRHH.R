rm(list=ls())
#################################################################################
####### PROGRAMA DE ESPECIALIZACION EN DATA SCIENCE NIVEL II ####################
#################################################################################

############################################################################
# PROFESOR : ANDRÉ CHÁVEZ - JOSÉ CÁRDENAS
############################################################################

## EJERCICIO DE APLICACION : ABSENTISMO DE PERSONAS ##

#----------------------------------------------
# PARTE 1: DESCRIPTIVA
#----------------------------------------------
# Leemos los datos
MFGEmployees <- read.csv("empleados.csv")
str(MFGEmployees)

# Comprobamos aspectos relaciones con la calidad de los datos
summary(MFGEmployees)

# Limpiamos datos, criterios de negocio o políticas de la empresa.
MFGEmployees<-subset(MFGEmployees,MFGEmployees$Age>=18)
MFGEmployees<-subset(MFGEmployees,MFGEmployees$Age<=65)

summary(MFGEmployees)

# Calculamos el absentismo considerando: 52 semanas*40 horas =2080 (Absentismo en horas)
MFGEmployees$AbsenceRate<-MFGEmployees$AbsentHours/2080*100
str(MFGEmployees)

# Análisis exploratorio
counts <- table(MFGEmployees$BusinessUnit)
barplot(counts, main = "N°Empleados X Unidad de Negocio", horiz = TRUE)

counts <- table(MFGEmployees$Gender)
barplot(counts, main = "N°Empleados X Genero", horiz = TRUE)

counts <- table(MFGEmployees$Division)
barplot(counts, main = "N°Empleados X Division", horiz = TRUE)

# ¿Cuánto es la tasa de absentismo? Definimos y estudiamos el target
mean(MFGEmployees$AbsenceRate)
library(ggplot2)
ggplot() + geom_boxplot(aes(y = AbsenceRate, x =1), data = MFGEmployees) + coord_flip()

# Cualquier observación por encima de 3 desviaciones típicas se visualiza con puntos. 
# Por lo tanto, algunos empleados tuvieron un mayor absentismo que el 99% de ellos
library(ggplot2)
library(RcmdrMisc)
ggplot() + geom_boxplot(aes(y = AbsenceRate, x = Gender), data = MFGEmployees) + coord_flip()

# Análisis por género - Generalmente se hace de manera gráfica o usando ANOVA.
AnovaModel.1 <- (lm(AbsenceRate ~ Gender, data=MFGEmployees))
library(car)
Anova(AnovaModel.1)
with(MFGEmployees, (tapply(AbsenceRate, list(Gender), mean, na.rm=TRUE)))

# Análisis por división
ggplot() + geom_boxplot(aes(y = AbsenceRate, x = Division), data = MFGEmployees) + coord_flip()
AnovaModel.2 <- (lm(AbsenceRate ~ Division, data=MFGEmployees))
Anova(AnovaModel.2)
with(MFGEmployees, (tapply(AbsenceRate, list(Division), mean, na.rm=TRUE)))

# Análisis por genéro y división
AnovaModel.3 <- (lm(AbsenceRate ~ Division*Gender, data=MFGEmployees))
Anova(AnovaModel.3)
with(MFGEmployees, (tapply(AbsenceRate, list(Division, Gender), mean, na.rm=TRUE)))


# ¿Varía la tasa de absentismo en función de la edad y los años de servicio en la organización?
library(RcmdrMisc)
scatterplot(AbsenceRate ~ Age, reg.line = FALSE, smooth = FALSE, spread = FALSE,
            boxplots = FALSE, span = 0.5, ellipse = FALSE, levels = c(.5, .9),
            data = MFGEmployees)
cor(MFGEmployees$Age, MFGEmployees$AbsenceRate)

scatterplot(AbsenceRate ~ LengthService, reg.line = FALSE, smooth = FALSE, spread = FALSE,
            boxplots = FALSE, span = 0.5, ellipse = FALSE, levels = c(.5, .9),
            data = MFGEmployees)
cor(MFGEmployees$LengthService, MFGEmployees$AbsenceRate)

scatterplot(LengthService ~ Age, reg.line = FALSE, smooth = FALSE, spread = FALSE,
            boxplots = FALSE, span = 0.5, ellipse = FALSE, levels = c(.5, .9),
            data = MFGEmployees)
cor(MFGEmployees$Age, MFGEmployees$LengthService)


#------------------------------------------------------
# PARTE 2: PREDICTIVA Y CONSTRUCCIÓN DE MODELOS
#------------------------------------------------------
# Antes de nada, limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list = ls())

# Leemos los datos
MFGEmployees <- read.csv("empleados.csv")
MFGEmployees$AbsenceRate<-MFGEmployees$AbsentHours/2080*100
summary(MFGEmployees)
MFGEmployees<-subset(MFGEmployees,MFGEmployees$Age>=18)
MFGEmployees<-subset(MFGEmployees,MFGEmployees$Age<=65)

# ¿Es posible predecir el absentismo? 
# La tasa de absentismo es una variable continua numérica. Necesitamos modelos que permitan trabajar con esas variables.
# Los (1) árboles de regresión y las (2) regresiones lineales son buenos modelos para ello.
library(rattle)
library(magrittr)
building <- TRUE
scoring <- ! building
crv$seed <- 42
MYdataset <- MFGEmployees
str(MYdataset)
MYinput <- c("Gender", "DepartmentName", "StoreLocation", "Division",
             "Age", "LengthService", "BusinessUnit")
MYnumeric <- c("Age", "LengthService")
MYcategoric <- c("Gender", "DepartmentName", "StoreLocation", "Division",
                 "BusinessUnit")
MYtarget <- "AbsenceRate"
MYrisk <- NULL
MYident <- "EmployeeNumber"
MYignore <- c("Surname", "GivenName", "City", "JobTitle", "AbsentHours")
MYweights <- NULL
library(rpart, quietly=TRUE)
set.seed(crv$seed)
MYrpart <- rpart(AbsenceRate ~ .,
                 data=MYdataset[, c(MYinput, MYtarget)],
                 method="anova",
                 parms=list(split="information"),
                 control=rpart.control(minsplit=10,
                                       maxdepth=10,
                                       usesurrogate=0,
                                       maxsurrogate=0))

rpart.plot::rpart.plot(MYrpart,main="Decision Tree MFGEmployees $ AbsenceRate")
# ¿Qué vemos? 
# La edad es un buen factor para determinar la tasa de absentimo. Como vemos, la mayoría de las 
# ramas se producen a partir de la edad. Salvo en un caso, que interviene el género también.

# Modelo de regresión lineal
RegressionCurrentData <- lm(AbsenceRate~Age+LengthService+Gender+DepartmentName, data=MFGEmployees)
summary(RegressionCurrentData)

# Varianza explicada por la edad, los años de servicio, el género y el nombre de departamento
# Las dos variables son significativas explicándolo: Pr(>|t|) de <2e-16
library(ggplot2)
ggplot() + geom_point(aes(x = Age,y = AbsenceRate),data=MFGEmployees) + geom_smooth(aes(x = Age,y = AbsenceRate),data=MFGEmployees)

library(scatterplot3d)
s3d <-scatterplot3d(MFGEmployees$Age,MFGEmployees$LengthService,MFGEmployees$AbsenceRate, pch=16, highlight.type="h", main="Absence Rate By Age And Length of Service")
fit <- lm(MFGEmployees$AbsenceRate ~ MFGEmployees$Age+MFGEmployees$LengthService)
s3d$plane3d(fit)

# Estos dos modelos de predicción tienen alguna debilidad. Especialmente, porque ambos modelos han usado
# datos que ya conocemos para predecir. No sabemos cómo hará predicciones para datos que aún no conocemos.
# Para poder hacer frente a este problema, lo que hacemos es dividir unos datos para entrenamiento y otros 
# para evaluar el modelo
# El paquete de R "caret" nos ayuda en eso.

library(caret)

set.seed(998)
inTraining <- createDataPartition(MFGEmployees$BusinessUnit, p = .75, list = FALSE)
training <- MFGEmployees[inTraining,]
testing <- MFGEmployees[ - inTraining,]
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10)
set.seed(825)
lmFit1 <- train(AbsenceRate ~ Age + LengthService, data = training,
                method = "lm",
                trControl = fitControl)
lmFit1

# El valor rsquared muestra un valor de 0.688. Hemos muestreado y hecho una 10 cross validation
set.seed(825)
rpartFit1 <- train(AbsenceRate ~ Age + LengthService, data = training,
                   method = "rpart",
                   trControl = fitControl,
                   maxdepth = 5)
rpartFit1

# Rsquared de 0.6. Vamos a ver con un pequeño cambio
set.seed(825)
rpartFit2 <- train(AbsenceRate ~ Gender + DepartmentName + StoreLocation + Division + Age + LengthService, data = training,
                     method = "rpart",
                     trControl = fitControl,
                     maxdepth = 5)
rpartFit2

# Tenemos que hacer un "deploy" del modelo
# 1. Utilizar la función predict.
# 2. Publicar el modelo como un servicio HTTP de R para que otros lo puedan usar.

# Dado que hemos entrenado los modelos con los datos históricos (2015),
# vamos a crear los de 2016 para validar los modelos
Absence2016Data<-MFGEmployees #Renombro la data
Absence2016Data$Age<-Absence2016Data$Age+1 # #Creo la edad el año siguiente
Absence2016Data$LengthService<-Absence2016Data$LengthService+1 #Creo el ts del año siguiente
Absence2016Data$AbsenceRate<-0 # Creo una variable target ficticia
Absence2016Data$AbsenceRate<-predict(lmFit1,Absence2016Data)
mean(Absence2016Data$AbsenceRate)
hist(MFGEmployees$AbsenceRate)
mean(MFGEmployees$AbsenceRate)
sd(MFGEmployees$AbsenceRate)
