#-----------------------------------------
# PD4
#-----------------------------------------
# ECONOMETRIA 2
#-----------------------------------------

#--------------
rm(list=ls())

#--------------

#---------------
# Librer?as
#---------------
library(dplyr)
library(readxl)
library(lmtest)
library(stargazer)
library(AER)
library(mfx)
library(DescTools)
library(margins)

#-----------------------
# Ruta de trabajo
#-----------------------
script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)
base_empleo <- read_excel("employment_08_09.xlsx")

#------------------------
#Pregunta 3.1
#-----------------------

"regresión lineal para la fracción"
eq.empl <- lm(employed ~1 , base_empleo)
summary(eq.empl)

"intervalo al 95% de confianza para el estimador"
confint(eq.empl , level = 0.95)

#------------------------
#Pregunta 3.2
#-----------------------

"creamos la columna age^2"
base_empleo$age2=base_empleo$age^2

"hacemos la regresión lineal"
eq.mpl <- lm(employed ~ age +age2 , base_empleo)
summary(eq.mpl)

  #3.2.1 Ver significancioa de age
  "sí, tiene p-value casi 0"
  
  #3.2.2 ver significancia de age2
  "sí, por su p-value casi 0 y tiene un efecto marginal sobre X no lineal al derivar la regresión"

  # 3.2.3 evaluar el efecto marginal de la edad
  margins_mpl <- margins(eq.mpl,at=list(age=c(20,40,60)))
  margins_mpl
  "efecto marginal lineal, como se sospechaba. Y será 0 para una edad de casi 0"
  
  #3.2.4 calcular la probablididad de las siguientes observaciones
    "creo un dataframe"
  prob <- data.frame(age=c(20, 40, 60),
                      age2=c(20^2, 40^2, 60^2))
  
    "realizo la predicción"
  predict_mpl <- predict(eq.mpl, newdata=prob, type="response")
  predict_mpl

#--------------------------------
#Pregunta 3.3 PROBIT
#---------------------------------
  
eq.probit <- glm(employed ~ age + age2, base_empleo, family = binomial(link="probit"))
summary(eq.probit)
  "no nos da efectos marginales, sino la dirección de la probablidad de acuerdo a cada valor de X: a mayor edad más trabajo y al cuadrado hay menor"

  #3.3.1 Ver significancioa de age
  "sí es significativa"
  
  #3.3.2 Ver significancioa de age2
  "sí es significativa"
  
  #3.3.3 efectos marginales
  margins_probit <-margins(eq.probit,at=list(age=c(20,40,60)))
  margins_probit
    "efectos marginales no constantes. Crece al ser adulto y cae en la vejez."

  #3.3.4 realizamos predicción, usando la misma dataframe
  predict_probit <- predict(eq.probit, newdata=prob, type="response")
  predict_probit
    "positivo, crece y cae la probabilidad con el paso de los años"
#--------------------------------
#Pregunta 3.4 LOGIT
#---------------------------------
  
eq.logit <- glm(employed ~ age + age2, base_empleo, family = binomial(link="logit"))
summary(eq.logit)

  #3.3.1 Ver significancioa de age
  "sí es significativa"

  #3.3.2 Ver significancioa de age2
  "sí es significativa"

  #3.3.3 efectos marginales
  margins_logit <-margins(eq.logit,at=list(age=c(20,40,60)))
  margins_logit
  "efectos marginales no constantes. Crece al ser adulto y cae en la vejez."
  
  #3.3.4 realizamos predicción, usando la misma dataframe
  predict_logit <- predict(eq.logit, newdata=prob, type="response")
  predict_logit
  "positivo, crece y cae con el paso de los años"
  
#------------------------------------------
# 3.5 Comparamos las estimaciones, las predicciones y los efectos
# marginales de los tres modelos
#------------------------------------------

#comparamos estimaciones
stargazer(eq.mpl,eq.probit,eq.logit, type="text")
  "hay parámetros distintos"
  
#comparamos los efectos marginales
margins_logit
margins_probit
margins_mpl
  "logit y probit varían de forma parecido, pero con la lineal es constante"

#comparamos predicciones
predict_logit
predict_probit
predict_mpl
  "los 3 predicen parecido, suben y caen"
  

#------------------------------------------
# 3.6
#------------------------------------------
  
# 3.6.1 Añadimos variables de control
  
base_empleo$age3 <- base_empleo$age * base_empleo$female
  "creamos una variable dummy años x mujer"
  
eq.probit_v2 <- glm(employed ~ age +age2 + educ_lths+ educ_hs + 
                      educ_somecol + educ_aa + educ_bac + earnwke + female + race +
                      ne_states + so_states + ce_states + age3,  base_empleo, family = binomial(link="probit"))
  "agregamos varias dummys de control para verificar si el modelo anterior tenia sesgos de omision de variables, ya que solo eran 2 regresores"

summary(eq.probit_v2)

  "lo mismo con logit"
eq.logit_v2 <- glm(employed ~ age +age2 + educ_lths+ educ_hs + 
                     educ_somecol + educ_aa + educ_bac + earnwke + female + race +
                     ne_states + so_states + ce_states + age3,  base_empleo, family = binomial(link="logit"))
summary(eq.logit_v2)


  "stargazer para comparar todo"
stargazer(eq.probit,eq.logit, eq.probit_v2, eq.logit_v2, type="text")
  "finalmente, no hay mucha variacion entre los modelos logit y probit, no hay cambios en los parámetros, por lo que "
  
# 3.6.2
  "educ_lths que es educacion muy basica, y luego de la crisis estas personas tienen bajas probabilidades de tener trabajo, y es significativo
   con la ganancia semanal es tambien significativo, pero pequeño y explica que con mayor ganancia puedes tener trabajo"

# 3.6.3 Calcular efectos marginales
  "hallamos el efecto marginal por individuo simple con 2 regresores"
mfx.probit <- probitmfx(employed ~ age + age2, base_empleo, atmean=FALSE)
mfx.logit <- logitmfx(employed ~ age + age2, base_empleo, atmean=FALSE)

  "hallamos el efecto marginal por individuo con el modelo de variables de control"
mfx.probit_v2 <- probitmfx(employed ~ age +age2 + educ_lths+ educ_hs + 
                             educ_somecol + educ_aa + educ_bac + earnwke + female + race +
                             ne_states + so_states + ce_states, base_empleo, atmean=FALSE)

mfx.logit_v2 <- logitmfx(employed ~ age +age2 + educ_lths+ educ_hs + 
                           educ_somecol + educ_aa + educ_bac + earnwke + female + race +
                           ne_states + so_states + ce_states, base_empleo, atmean=FALSE)

  "observamos lo que nos da"
mfx.probit
mfx.logit
mfx.probit_v2
mfx.logit_v2


  "usar los criterios de ajuste de bondad para elegir uno de estos modelos usando stargazer"
    