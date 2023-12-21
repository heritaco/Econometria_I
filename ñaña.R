library(readr)
library(PerformanceAnalytics)

# fastfood ----------------------------------------------------------------

fastfood <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/fastfood.csv")
head(fastfood)
str(fastfood)
fastf <- fastfood[, !(names(fastfood) %in% c('restaurant', 'item', 'salad'))]
head(fastf)
str(fastf)
fastf <- na.omit(fastf) 
chart.Correlation(fastf)

# prediccion de calorias --------------------------------------------------

cal_modelovacio <- lm(calories ~ 1 , data = fastf)

cal_modelocompleto <- lm(calories ~ ., data = fastf)
summary(cal_modelocompleto)

cal_modeloForward <- step(cal_modelovacio,
                      scope = list(lower=cal_modelovacio,upper=cal_modelocompleto),
                      direction = "forward", trace = 1)
summary(cal_modeloForward)



# prediccion del azucar ---------------------------------------------------


sug_modelovacio <- lm(sugar ~ 1 , data = fastf)

sug_modelocompleto <- lm(sugar ~ ., data = fastf)
summary(modelocompleto)

sug_modeloForward <- step(sug_modelovacio,
                      scope = list(lower=sug_modelovacio,upper=sug_modelocompleto),
                      direction = "forward", trace = 1)
summary(sug_modeloForward)



  # prediccion proteina -----------------------------------------------------


pro_modelovacio <- lm(protein ~ 1 , data = fastf)

pro_modelocompleto <- lm(protein ~ ., data = fastf)
summary(pro_modelocompleto)

pro_modeloForward <- step(pro_modelovacio,
                      scope = list(lower=pro_modelovacio,upper=pro_modelocompleto),
                      direction = "forward", trace = 1)
summary(pro_modeloForward)




# carb --------------------------------------------------------------------


carb_modelovacio <- lm(trans_fat ~ 1 , data = fastf)

carb_modelocompleto <- lm(trans_fat ~ ., data = fastf)
summary(carb_modelocompleto)

carb_modeloForward <- step(carb_modelovacio,
                      scope = list(lower=carb_modelovacio,upper=carb_modelocompleto),
                      direction = "forward", trace = 1)
summary(carb_modeloForward)




# carb 2 ------------------------------------------------------------------

carb2_modelovacio <- lm(total_carb ~ 1 , data = fastf)

carb2_modelocompleto <- lm(total_carb ~ . - sodium - trans_fat - protein, data = fastf)
summary(carb_modelocompleto)



modelocompleto <- lm(total_carb ~ calories*total_fat + vit_a + fiber, data = fastf) .85


modelocompleto <- lm(total_carb ~ calories*total_fat + vit_a + fiber, data = fastf) 

modelocompleto <- lm(total_carb ~ calories*total_fat + sugar*calories vit_a + fiber, data = fastf) 
summary(modelocompleto)

ggplot(data = data.frame(y = fastf), aes(sample = y)) +
  stat_qq() +
  geom_abline(intercept = mean(fastf), slope = sd(fastf), color = "red", linetype = "dashed") +
  labs(title = "QQ-Plot para los datos fastf",
       x = "Cuantiles teóricos",
       y = "Cuantiles observados")

kkkfdsa <- lm(total_carb ~ calories*total_fat + calories*sugar + sugar*total_fat + cal_fat * calories + vit_a, data = fastf)
summary(kkkfdsa)



kkkfdsa <- lm(total_carb ~ sugar * fiber + calories * cal_fat * total_fat * sat_fat, data = fastf)
summary(kkkfdsa)

carb2_modeloForward <- step(carb2_modelovacio,
                      scope = list(lower=carb2_modelovacio,upper=carb2_modelocompleto),
                      direction = "forward", trace = 1)
summary(carb2_modeloForward)
AIC(carb2_modeloForward)


para_diagrama <- fastf[, !(names(fastf) %in% c('cal_fat', 'total_fat', 'sat_fat', 'trans_fat', 'sodium', 'trans_fat', 'protein'))]
chart.Correlation(para_diagrama)


# Seleccionar solo las variables 'calorias' y 'colesterol'
para_diagrama <- fastf[c("total_carb", "calories", "cholesterol", "total_fat", "sugar", "fiber", "vit_a", "vit_c", "calcium", "sat_fat")]
chart.Correlation(para_diagrama)



chart.Correlation(fastf)

plot(carb2_modeloForward)


shapiro.test(carb2_modeloForward$residuals)

modelo <- lm(total_carb ~ calories + cholesterol + total_fat + sugar + fiber + vit_a + vit_c + calcium + sat_fat, data = fastf)
AIC(modelo)  
library(MASS)
model <- lm(total_carb ~ .,data= fastf)


modelo$coefficients

stepAIC(model, direction="both")
##     vit_a + vit_c + calcium + sat_fat




# colesterol --------------------------------------------------------------

carb2_modelovacio <- lm(cholesterol ~ 1 , data = fastf)

carb2_modelocompleto <- lm(cholesterol ~ . - calcium - vit_a - calories - sodium, data = fastf)
summary(carb_modelocompleto)

carb2_modeloForward <- step(carb2_modelovacio,
                            scope = list(lower=carb2_modelovacio,upper=carb2_modelocompleto),
                            direction = "forward", trace = 1)
summary(carb2_modeloForward)


para_diagrama <- fastf[, !(names(fastf) %in% c('cal_fat', 'total_fat', 'sat_fat', 'trans_fat', 'sodium', 'trans_fat', 'protein'))]
chart.Correlation(para_diagrama)


# blabla ------------------------------------------------------------------




modelovacio <- lm(sugar ~ 1 , data = fastfood)
modelocompleto <- lm(sugar ~ . - restaurant - item, data = fastfood)
summary(modelocompleto)


modelovacio <- lm(protein ~ 1 , data = fastfood)
modelocompleto <- lm(protein ~ . - restaurant - item, data = fastfood)
summary(modelocompleto)


modelovacio <- lm(calories ~ 1 , data = fastfood)
modelocompleto <- lm(calories ~ . - restaurant - item, data = fastfood)
summary(modelocompleto)

modelovacio <- lm(protein ~ 1 , data = fastfood)
modelocompleto <- lm(protein ~ . - restaurant - item, data = fastfood)
summary(modelocompleto)



library(readr)
fastfood2 <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/ediet/fastfood.csv")
View(fastfood2)

modelovacio <- lm(protein ~ 1 , data = fastfood2)
modelocompleto <- lm(protein ~ . - restaurant - item, data = fastfood2)
summary(modelocompleto)










# Starbucks ---------------------------------------------------------------

starbucks <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/starbucks.csv")
head(starbucks)


modelovacio <- lm(calories ~ 1 , data = starbucks)
modelocompleto <- lm(fat ~ . - item - type, data = starbucks)
summary(modelocompleto)











# email -------------------------------------------------------------------

email <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/email.csv")
head(email)
View(email)

modelovacio <- lm(spam ~ 1 , data = email)
modelocompleto <- lm(spam ~ . , data = email)
summary(modelocompleto)

no





# earthquake --------------------------------------------------------------
on

library(readr)
earthquakes <- read_csv("C:/Users/herie/OneDrive - Fundacion Universidad de las Americas Puebla/Semestre/5 Semestre/Econometria l/Bases de datos/earthquakes.csv")
View(earthquakes)

earthquakes$region <- as.factor(earthquakes$region)
str(earthquakes)

modelovacio <- lm(deaths ~ 1 , data = earthquakes)
modelocompleto <- lm(deaths ~ region, data = earthquakes)
summary(modelocompleto)

# Fin ---------------------------------------------------------------------







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

