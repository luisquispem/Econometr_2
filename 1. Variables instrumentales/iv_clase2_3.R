#----------------------------a
#UNIDAD 1: Variables Instrumentales
#----------------------------

#limpieza
rm(list=ls())

#----------------------------B
#Librerías
#----------------------------
library(readxl)
library(rstudioapi)aaaaa
library(sandwich)
library(lmtest)
library(olsrr)
library(skedastic)
library(stargazer)
library(AER)

#----------------------------
#Establecer la ruta de trabajo
#-------------------------------
script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)

#----------------------------
#Importar el excel
#-------------------------------
datos <- read_excel("acemoglu_datos_64.xlsx")

#----------------------------
#ESTIMACIÓN OLS
#-------------------------------

#Estimación OLS básica 
m1<- lm(logpgp95~ avexpr, datos)
summary(m1)
"el modelo más simple, parece estar completo"
  
  #agregamos otra explicativa
  m2<- lm(logpgp95~ avexpr+lat_abst, datos)
  summary(m2)
  "parece que la nueva variable es explicativa"

#Estimación OLS con errores rubustos para el primer modelo
m1r<-coeftest(m1, vcovHC(m1, type="HC1"))
m1r

#----------------------------
#PRUEBAS DE HETEROCEDASTICIDAD
#-------------------------------

#Le aplicamos prueba de heterocedasticidad
ols_test_breusch_pagan(m1)
"dado el resultado, aceptamos la H0"

#aplicamos para el modelo 2, con 2 variables explicativas
ols_test_breusch_pagan(m2, rhs = TRUE)
"haceptamos la H0"

#aplicamos prueba de White al modelo 1
white_lm(m1, interactions = FALSE, statonly = FALSE)
"donde la H0=errores homocedástico, y no rechazamos H0, por lo que no habría necesidad de hacer robustez"

#----------------------------
#ESTIMACIÓN POR VARIABLES INSTRUMENTALES
#-------------------------------

#usamos una nueva función, donde agregamos la variable instrumental
m1.iv<-ivreg(logpgp95~avexpr, ~logem4, datos)
summary(m1.iv)
  
  #agregamos otra variable explicativa. No tiene instrumento, así que ella misma es su instrumento
  m2.iv<-ivreg(logpgp95~avexpr+lat_abst, ~logem4+lat_abst, datos)
  summary(m2.iv)
  
#podemos aplicarle errores robustos
m1r.iv <- coeftest(m1.iv, vcovHC(m1.iv, type = "HC3"))
m1r.iv
  m2r.iv<- coeftest(m2.iv, vcovHC(m2.iv, type = "HC3"))
  m2r.iv
  "con el segundo modelo creció la estimación de la variable de interés, por lo que
  este estimado puede ser muy bueno"
  
#comparamos los resultados de cada la estimación por OLS y por IV
stargazer(m1,m1.iv,type="text",no.space = FALSE, 
          dep.var.labels = c("PBI per capita"), 
          se=list(m1r[, "Std. Error"], m1r.iv[,"Std. Error"]))
"Es normal que aumenten los errores estandar, porque la varianza en I.V. es mayor que OLS
sobre todo en muestras pequeñas. Es el precio a pagar por una mejor estimación causal."
#----------------------------
#PRUEBAS DE DIAGNOSTICO
#-------------------------------
"Weak instruments.  Ho: weak instruments"
"Wu-hausman.        Ho: regressors are exogenous"
"Sargan.            Ho: instruments are exogenous"
#testeamos el modelo m1.iv
summary(m1.iv, diagnostics = TRUE)
  "para Sargan se necesitan sobreidentificación, más instrumentos que regresores explicativos"
  "Los instrumentos no son débiles y los regresores son endógenos"  

#nuevo modelo con 2 instrumentos:
m1.iv2<-ivreg(logpgp95~avexpr, ~logem4+euro1900, datos)
m1r.iv<-coeftest(m1.iv2, vcovHC(m1.iv2, type="HC3"))
m1r.iv
"El estimador es mayor que con OLS"
#lo testeamos
summary(m1.iv2, diagnostics = TRUE)
"los instrumentos no son débiles, el regresor no es exógeno y 
el exceso de instrumentos es válido"

#nuevo modelo con 2 instrumentos y 1 variable de control:
m2.iv2<-ivreg(logpgp95~avexpr+lat_abst, ~logem4+lat_abst+euro1900, datos)
m2r.iv<-coeftest(m2.iv2, vcovHC(m2.iv2, type="HC3"))
m2r.iv
"el coeficiente causal es alto, es mejor que OLS"
#lo testeamos
summary(m2.iv2, diagnostics = TRUE)
"los instrumentos no son débiles, el regresor no es exógeno y 
el exceso de instrumentos es válido"

#nuevo modelo con 2 instrumentos y múltiples variable de control:
m4.iv<-ivreg(logpgp95~ avexpr+lat_abst+asia+africa+malfal94,
             ~ logem4+euro1900+lat_abst+asia+africa+malfal94, datos)
summary(m4.iv)
m4r.iv<-coeftest(m4.iv, vcovHC(m4.iv, type="HC3"))
m4r.iv
"cae el coeficiente, porque las nuevas variables de control retienen la relación de la
variable de interés con la perturbación"
#testeamos
summary(m4.iv, diagnostics=TRUE)
"los instrumentos no son débiles, el regresor no es exógeno y 
el exceso de instrumentos es válido"
