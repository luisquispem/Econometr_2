#-----------------------------------------
# PC2
#-----------------------------------------
# ECONOMETRIA 2
# Luis Quispe Macavilca
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
library(readxl)
library(rstudioapi)
library(sandwich)
library(lmtest)
library(olsrr)
library(skedastic)
library(stargazer)
library(AER)
#-----------------------
# Ruta de trabajo
#-----------------------
script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)
base_alcohol <- read_excel("alcohol.xlsx")

#-----------------------
# 1.1 
#-----------------------
  "fracci?n de la muestra que est? empleada como intercepto de la regresi?n"
eq.empl <- lm(employ ~1 , base_alcohol)
summary(eq.empl)

  "fracci?n de la muestra que abusa del acohol como intercepto de la regresi?n "
eq.abuse <- lm(abuse ~1 , base_alcohol)
summary(eq.abuse)
  "intervalo al 95% de confianza para el estimador de abuse"
confint(eq.abuse , level = 0.95)

#-----------------------
# 1.2
#-----------------------
eq.empleo <- lm(employ ~ abuse , base_alcohol)
summary(eq.empleo)
  "el estimador nos dice que si la persona abusa del alcohol reduce su oportunidad de estar empleado en 2.8 puntos porcentuales"
  "la relaci?n es significativa por tener un p-value peque?o, adem?s que supon?a que una persona 
    que abuse del alcohol puede reducir sus oportunidades de ser empleado"
  
#-----------------------
# 1.3
#-----------------------
eq.probit <- glm(employ ~ abuse, base_alcohol, family = binomial(link="probit"))
summary(eq.probit)
  "encontramos un estimador negativo, lo que nos dice que el efecto marginal para una persona que abusa del alcohol disminuir? sus
  posibilidades de ser empleada. Tambi?n son estimadores significativos, como en el modelo lineal"

mfx.probit <- probitmfx(employ ~ abuse, base_alcohol, atmean=FALSE)
mfx.probit
margins_mpl <- margins(eq.empleo)
margins_mpl
  "observamos que los efectos marginales son similares en modelo lineal y probit (-2.83 puntos porcentuales por abusar del alcohol)"

#-----------------------
# 1.4
#-----------------------
  #para el modelo lineal
  "creo un dataframe"
  prob <- data.frame(abuse=c(0, 0, 1))
  
  "realizo la predicci?n"
  predict_mpl <- predict(eq.empleo, newdata=prob, type="response")
  predict_mpl

  #para el modelo probit
  "creo un dataframe"
  prob <- data.frame(abuse=c(0, 0, 1))
  
  "realizo la predicci?n"
  predict_probit <- predict(eq.probit, newdata=prob, type="response")
  predict_probit
  "obtenemos valores similares. La explicaci?n es porque ambos modelos tienen un efecto marginal similar "
  
#-----------------------
# 1.5
#-----------------------
eq.empleo_2 <- lm(employ ~ abuse  +age+ agesq+ educ+ educsq+ married+ famsize+ white+ northeast+
                    midwest+ south +centcity+ outercity+ qrt1+ qrt2 + qrt3 , base_alcohol)
summary(eq.empleo_2)

stargazer(eq.empleo, eq.empleo_2, type= "text")
  "si cambia porque se reduce en 0.8 puntos porcentuales, y sigue siendo significativa. Se puede explicar que en el primer modelo
  faltaban algunas variables explicativas, y el estimador de abuse era mayor por el sesgo de omisi?n de variables. Al a?adir m?s variables,
  tenemos que el estimador se reduce un poco, pero sigue siendo significativo, adem?s que varias de las nuevas variables son significativas"

#-----------------------
# 1.6
#-----------------------
eq.probit_2 <- glm(employ ~ abuse  +age+ agesq+ educ+ educsq+ married+ famsize+ white+ northeast+
                     midwest+ south +centcity+ outercity+ qrt1+ qrt2 + qrt3 , base_alcohol, family = binomial(link="probit"))
summary(eq.probit_2)

  #efecto marginal del nuevo modelo
  mfx.probit_2 <- probitmfx( employ ~  abuse  +age+ agesq+ educ+ educsq+ married+ famsize+ white+ northeast+
                            midwest+ south +centcity+ outercity+ qrt1+ qrt2 + qrt3 , base_alcohol, atmean=FALSE)
  mfx.probit_2
  "El efecto marginal de abuse en modelo probit aumenta, es significativo al 5%. En el modelo lineal el parametro estimado es menor pero tiene p-vale de casi 0 Ya que a?n es significativo, y tiene un 
  efecto marginal parecido y en la misma direcci?n, podemos decir que son similares."

#-----------------------
# 1.7
#-----------------------

eq.probit_3 <- glm(employ ~ abuse +exhealth + vghealth + goodhealth, base_alcohol, family = binomial(link="probit"))
summary(eq.probit_3)
  "si podr?an a?adirse, ya que estas nuevas variables son significativas, adem?s que la variable abuse sigue siendo significativa al 5%"
  mfx.probit_3 <- probitmfx( employ ~ abuse +exhealth + vghealth + goodhealth, base_alcohol, atmean=FALSE)
  mfx.probit_3
  "al ver los efectos marginales, comprobamos que las nuevas variables reducen el sesgo por omisi?n de variables para nuestra variable explicativa"
  
#-----------------------
# 1.8
#-----------------------
  m1.iv<-ivreg(employ~abuse, ~mothalc + fathalc, base_alcohol)
  summary(m1.iv)
  
  "probamos con errores robustos"
  m1r.iv <- coeftest(m1.iv, vcovHC(m1.iv, type = "HC3"))
  m1r.iv
  "Parece ser end?gena porque ahora la estimaci?n se elev? con los nuevvos instrumentos, que parecen ser correctos 
  porque el p.value aun es significativo al 9%"


  