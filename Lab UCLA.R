setwd("~/Desktop/S. 6 🎳/🧑‍💻Data Mining & Machine Learning/labs/ucla")
#LIBRERIAS 
library(ISLR)
library(rpart)
library(caret)
library(dplyr)
library(ggplot2)
library(ranger)
library(MLmetrics)
library(caTools)
#SET DE DATOS
ucla <- read.csv("datos.csv")

#ESTRUCTURA DE DATOS
str(ucla)

#CONVERTIR CELDAS VACIAS DE "SALARY" A "0"
ucla[is.na(ucla)] <- "0"
any(is.na(ucla))
ucla$salary <- as.numeric(ucla$salary)
ucla$status <- as.factor(ucla$status)

ucla$salary <- NULL

#VARIABLES MAS RELEVANTES
variables_mas_importantes <- ranger(status ~ . , data = ucla, importance = "impurity")

sort(importance(variables_mas_importantes))

#CATOOLS PARA PARTICIONAR LA DATA DE ENTRENAMIENTO Y PRUEBA
muestra <- sample.split(ucla$status, SplitRatio = 0.8)

#SET DE ENTRENAMIENTO Y PRUEBA
entrenamiento <- subset(ucla, muestra == T)
prueba <- subset(ucla, muestra == F)

#MODELO DE ARBOL DE DECISION
arbol <- rpart(status ~ . , data = entrenamiento, method = "class", 
               control = rpart.control(minsplit = 10, minbucket = 5))

#LIBRERIA RPART.PLOT
library(rpart.plot)
prp(arbol)

#PREDICCIONES SOBRE EL SET DE PRUEBA
arbol.predict <- predict(arbol, prueba)
arbol.predict

#FUNCION PARA HACER UN PUNTO DE CORTE
corte <- function(x){
  if(x >= 0.5){
    return("Placed")
  }else{
    return("Not Placed")
  }
}

arbol.predict <- as.data.frame(arbol.predict)
arbol.predict$status <- sapply(arbol.predict$Placed, corte)

confusionMatrix(factor(arbol.predict$status), prueba$status)
