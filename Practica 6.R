library(readr)
library(ggplot2)
library(MASS)
library(ISLR2)
library(PerformanceAnalytics)
library(lmtest)
library(nortest)

diabetes <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/diabetes.csv")
glucose <- diabetes$Glucose
pregnancies <- diabetes$Pregnancies
presion <- diabetes$BloodPressure
piel <- diabetes$SkinThickness
insulina <- diabetes$Insulin
imc <- diabetes$BMI
pedigri <- diabetes$DiabetesPedigreeFunction
edad <- diabetes$Age
resultado <- diabetes$Outcome

modelovacio <- lm(Glucose ~ 1, data=diabetes)
summary(modelovacio)

# Se elimina la variable categorica
modelocompleto <- lm(Glucose ~ . - Outcome , data = diabetes)
summary(modelocompleto)

modeloForward <- step(modelovacio,
                      scope = list(lower=modelovacio,upper=modelocompleto),
                      direction = "forward", trace = 1)
summary(modeloForward)


modeloBackward <- step(modelocompleto,
                      scope = list(lower=modelovacio,upper=modelocompleto),
                      direction = "backward", trace = 1)
 summary(modeloBackward)


modeloStepwise <- step(modelovacio,
                       scope = list(lower=modelovacio,upper=modelocompleto),
                       direction = "both", trace = 1)
summary(modeloStepwise)

stepAIC(modeloStepwise, direction = "both")
AIC(modeloStepwise)

