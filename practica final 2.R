library(ISLR2)
library(tidyverse)
library(ggplot2)
library(MASS)
library(msm)
library(msmtools)
library(readr)
library(vcd)
library(PerformanceAnalytics)

data = datos = diabetes <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/diabetes.csv")
head(diabetes)


pregnancies <- diabetes$Pregnancies
glucose <- diabetes$Glucose
bloodPressure <- diabetes$BloodPressure
skinThickness <- diabetes$SkinThickness
insulin <- diabetes$Insulin
bmi <- diabetes$BMI
pedigree <- diabetes$DiabetesPedigreeFunction
age <- diabetes$Age
outcome <- diabetes$Outcome


# Construye un modelo -----------------------------------------------------

modlog <- glm(Outcome ~ pregnancies + glucose + bloodPressure + skinThickness + insulin + bmi + pedigree + age, data = datos, family = "binomial")
summary(modlog)

# Intro ------------------------------------------------------------------

Introducción al Conjunto de Datos de Diabetes
El Conjunto de Datos de Diabetes, originalmente recopilado por el Instituto Nacional de Diabetes y Enfermedades Digestivas y Renales. Este conjunto de datos se compone de varias variables médicas predictivas y una variable objetivo, “Outcome”. Las variables predictoras incluyen el número de embarazos que la paciente ha tenido, su IMC, nivel de insulina, edad y más.
El objetivo de este conjunto de datos es predecir si un paciente tiene diabetes, basándose en ciertas mediciones diagnósticas incluidas en el conjunto de datos. En particular, todos los pacientes aquí son mujeres de al menos 21 años de origen indio Pima.
Contenido del Conjunto de Datos
Este conjunto de datos presenta información específica sobre pacientes, todos ellos mujeres de al menos 21 años y de herencia Pima India. Las variables incluidas son las siguientes:
1.	Pregnancies (Embarazos): Número de veces que la paciente ha estado embarazada.
2.	Glucose (Glucosa): Concentración de glucosa en plasma 2 horas después de un examen de tolerancia a la glucosa oral.
3.	BloodPressure (Presión Arterial): Presión arterial diastólica en milímetros de mercurio (mm Hg).
4.	SkinThickness (Grosor del Pliegue Cutáneo): Grosor del pliegue cutáneo del tríceps en milímetros (mm).
5.	Insulin (Insulina): Nivel de insulina en suero 2 horas después de un examen (en unidades por mililitro - mu U/ml).
6.	BMI (Índice de Masa Corporal): Índice de masa corporal, calculado como el peso en kilogramos dividido por la altura en metros al cuadrado (kg/(m^2)).
7.	DiabetesPedigreeFunction (Función de Pedigrí de Diabetes): Función que representa la probabilidad de tener diabetes según el historial familiar.
8.	Age (Edad): Edad de la paciente en años.
9.	Outcome (Resultado): Variable cualitativa que indica si la paciente tiene diabetes (1) o no (0).


# Gráficos ----------------------------------------------------------------

chart.Correlation(data[ , -9])


ggplot(datos, mapping = aes(x=factor(outcome),y=pregnancies,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=factor(outcome),y=glucose,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=factor(outcome),y=bloodPressure,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=factor(outcome),y=skinThickness,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=factor(outcome),y=insulin,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=factor(outcome),y=bmi,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=factor(outcome),y=pedigree,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

ggplot(datos, mapping = aes(x=factor(outcome),y=age,fill=factor(outcome)))+
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.2)

# Mejor modelo ------------------------------------------------------------


modlog <- glm(outcome ~ pregnancies + glucose + bloodPressure + bmi + pedigree, data = diabetes, family = "binomial")
summary(modlog)


# 5. Construccion de datatrain y datatest ---------------------------------

pregnancies_train <- data_train$Pregnancies
glucose_train <- data_train$Glucose
bloodPressure_train <- data_train$BloodPressure
skinThickness_train <- data_train$SkinThickness
insulin_train <- data_train$Insulin
bmi_train <- data_train$BMI
pedigree_train <- data_train$DiabetesPedigreeFunction
age_train <- data_train$Age
outcome_train <- data_train$Outcome


# https://statisticsglobe.com/r-split-data-into-train-and-test-sets

split_dummy <- sample(c(rep(0, 0.8 * nrow(datos)),  # Create dummy for splitting
                        rep(1, 1 + 0.2 * nrow(datos)))) # + 1 para que tengan el mismo tamaño 154.6
split_dummy  

table(split_dummy)                                 # Table of dummy
# 614 van a train y predecimos 153

data_train <- data[split_dummy == 0, ]             # Create train data
head(data_train)                                   # First rows of train data

data_test <- data[split_dummy == 1, ]              # Create test data
head(data_test)                                    # First rows of test data

cccc = data[!data_train, ]

cccc = data[!pregnancies_train, ]



train = pregnancies_train
per80 = data[!train, ] # conjunto de prueba, #de la base de datos, quita los del entrenamiento
dim(outrain)

outcometrain <- outcome_train[!data_train]


glmtrain <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Volume, data = data, family = "binomial", subset = train)
# subset, restringe a que solo tomemos los entrenamientos
summary(glmtrain)



modlog <- glm(Outcome ~ pregnancies + glucose + bloodPressure + skinThickness + insulin + bmi + pedigree + age, data = datos, family = "binomial", subset = train)
summary(modlog)

glmtrain <- glm(outcome_train ~ pregnancies_train + glucose_train + bloodPressure_train + bmi_train + pedigree_train, data = data_train, family = "binomial", subset = train)
summary(glmtrain)






train = (Year < 2005 ) # conjunto de entrenamiento, se agarra un 80 porciento, para luego veer que tal le fue contra lo demas
Smarket2005 = data[!train, ] # conjunto de prueba, #de la base de datos, quita los del entrenamiento
dim(Smarket2005)

Direction2005 <- Direction[!train]
glmtrain <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Volume, data = data, family = "binomial", subset = train)
# subset, restringe a que solo tomemos los entrenamientos
summary(glmtrain)

glmprobst <- predict(glmtrain, Smarket2005, type = "response")
glmpredt <- rep("Down", 252)
glmpredt[glmprobst > 0.5] = "Up"
table(glmpredt, Direction2005)
mosaic(table(glmpredt, Direction2005),gp=gpar(fill=matrix(c("green4", "red3", "red3", "green4"),2,2)))
# esta tabla fue hecha con el porcentaje de entrenamiento, hasta el año 2005, con 252 obs.

# vamos a comparar para ver cual fue mejor

table(glmpred, Direction)
table(glmpredt, Direction2005)


# 6. Ajuste de modelo logistico con datatrain -----------------------------



glmtrain <- glm(outcome_train ~ pregnancies_train + glucose_train + bloodPressure_train + bmi_train + pedigree_train, data = data_train, family = "binomial", subset = train)
summary(glmtrain)

# 7. Construcción de glmprobs ---------------------------------------------

coef(glmtrain)

#probabilidad de que dado unos valores fijos de las predictoras, a que categoria pertenece el tipo de direccion
glmprobs <- predict(glmtrain, Smarket2005, type = "response")  # Si es mayor a .5 va para arriba y al reves
head(glmprobs)
glmprobs[1:10]

contrasts(factor(outcome_train)) # Como no tenemos r cuadrada no tenemos una metrica que ayude a evaluar el modelo, 
# entonces se hace por una matriz de observacion, se puede transformar a una de mosaico
glmpred <- rep("0",  0.8 * nrow(datos)) # repite el string 1250 veces.
head(glmpred) # esto pero 1250 veces
glmpred[glmprobs > 0.5] = "1" # lo vamos a modificar con el filtro de la pribabilidad, si es mayor a .5 se va a up
glmprobs[1:20]
glmpred[1:20]


# 8. Matriz de confusion --------------------------------------------------

library(vcd)
table(glmpred, Direction2005) # tabla de frecuencias, es el contraste

table <-  table(glmpred, factor(outcome_train))
# Lo que pronostica y lo que en verdad paso
# pred abajo y esta abajo, pred abajo y esta arriba
# pred arriba y esta abajo, pred arriba y esta arriba
table

mosaic(table(glmpred, factor(outcome_train)),gp=gpar(fill=matrix(c("green4", "red3", "red3", "green4"),2,2)))
#verde aciertos y rojo errores


# 9  ----------------------------------------------------------------------


tableper <- table/ (length(outcome_train))*100
tableper

tn <- tableper[1,1]
fn <- tableper[1,2]
fp <- tableper[2,1]
tp <- tableper[2,2]

# Accurency: Proporción de veces que el modelo predice bien las etiquetas en general.
tn + tp

# Precisión: Proporción de veces que el modelo predice bien la clase objetivo.
tp


# La sensibilidad (o recall) representa la tasa de verdaderos positivos (True Positive Rate) ó TP. Es la proporción entre los casos positivos bien clasificados por el modelo, respecto al total de positivos. Para calcularlo en este caso:
tp / ( tp + fp) * 100

# Tasa de falsos positivos: Es similar a la precisión, pero se calcula considerando la proporción de 
# verdaderos positivos sobre todas las instancias predichas como positivas (TP / (TP + FP)) 
tp / ( tp + fp ) * 100


# Tasa de falsos negativos: Mide la proporción de verdaderos negativos sobre todas las 
# instancias predichas como negativas (TN / (TN + FN)).
tn / (tn + fn) * 100
  

