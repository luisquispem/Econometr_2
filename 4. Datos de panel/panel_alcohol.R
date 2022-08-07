#-----------------------------------------
# Datos panel
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
library(AER)
library(stargazer)
library(plm)
#-----------------------
# Ruta de trabajo
#-----------------------
script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)
datos <- read_excel("panel_fatality.xlsx")
"Stock & Watson - Intro. Econometrics"

#---------------
#Crear variables y estad?sticos descriptivos
#------------

"tasa de fatalidades por 10'000 personas"
datos$rfatal <- datos$fatal / datos$pop * 10000

#--------------------------
#PPT actualizado ---------------------------------------------
#-------------------------

#modelo pooled
#-------------
#Estimaci?n por OLS / pooled => X=impuesto alcohol, Y=fatalidades
mod_ols <- lm(rfatal ~ beertax, datos)
summary(mod_ols)
coeftest(mod_ols, vcov = vcovHC, type = "HC1")

mod_pooled <- plm(rfatal ~ beertax, datos, model = "pooling")
summary(mod_pooled)
coeftest(mod_pooled, vcov = vcovHC, type = "HC1")
  "no es lo esperado, quiz? estamos omitiendo variables que nos arrojan una etimaci?n sesgada,
porque tenemos que a m?s impuesto, m?s accidentes, y deber?a ser al reves"


#modelo FE individual
#-----------------------
#Estimaci?n con efectos fijos -> state, year
mod_fe<- plm(rfatal ~ beertax, datos, index=c("state", "year"), model = "within")
summary(mod_fe)
coeftest(mod_fe, vcov = vcovHC, type = "HC1")

#Estimaci?n con efectos fijos manual (dummy), quitandole 1 para evitar trampa de dummy
mod_dummy <- lm(rfatal ~ beertax + state - 1, datos)
summary(mod_dummy)
coeftest(mod_dummy, vcov = vcovHC, type="HC1")

#Estimaci?n FE de efecto individual
mod_fixed <- plm(rfatal ~ beertax, datos, effect = c("individual"), model=c("within"))
summary(mod_fixed)
coeftest(mod_fixed, vcov = vcovHC, type="HC1")
  "el nuevo modelo si nos brinda valores esperados. Adem?s, nos da que beertax es significativo al 1%
por lo que decimos que es mejor modelo que el pooled"

pFtest(mod_fixed, mod_pooled)
  "rechazamos H0:efectos individuales no significativos. Por ello es que debemos estimar por FE, sino tendiramos una
estiamci?n sesgada por OLS/pooled"

#modelo FE, con efecto temporal
#----------------------------------
mod_temporal <- plm(rfatal ~ beertax, datos, effect = c("time"), model=c("within"))
summary(mod_temporal)
coeftest(mod_temporal, vcov = vcovHC, type="HC1")
  "con efectos temporales no tenemos una buena estimaci?n"

#modelo FE, con efecto twoways
#-------------------------------
mod_twoways <- plm(rfatal ~ beertax, datos, effect = c("twoways"), model=c("within"))
summary(mod_twoways)
coeftest(mod_twoways, vcov = vcovHC, type="HC1")
"con efectos twoways, usamos efectos individuales y temporales, y nos brinda un buen estimador, similar al FE individual"
  
#modelo Random Effects individual
#---------------------------------
mod_re <- plm(rfatal ~ beertax, datos, effect = c("individual"), model=c("random"))
summary(mod_re)
coeftest(mod_re, vcov = vcovHC, type="HC1")
  "al calcular por random effects, se estima la matriz OMEGA para lograr la estimaci?n por Minimos Cuadrados Generalizados Factibles,
FGLS, sin embargo, no obtenemos un mejor estimados que los anteriores."

#Test de Hausman
#---------------------------------
phtest(mod_fixed, mod_re)
  "comparamos modelo FEindividual vs modelo RE, rechazando la Hipotesis Nula: no hay correlaci?n entre efectos fijos y ex?gena.
Por lo tanto, el modelo que eligiremos ser? el modelo FE"






  
  
  
  
  
  
  
  
#--------------------------
#PPT antiguo
#-------------------------

#Estimaci?n por OLS

#estimaci?n simple, Y=tasa fatalidades, X=tax al alcohol
mod_ols <- lm(rfatal ~ beertax, datos)
summary(mod_ols)
  "ojo, no es lo esperado. Si sube el tax, deber?a disminuir el consumo del alcohol,
  y por ende, disminuir el numero de fatalidades. ESTIMADOR SESGADO POR OMISION DE VARIABLES FIJAS"

#Estimaci?n con efectos fijos -> state, year
mod_fe<- plm(rfatal ~ beertax, datos, index=c("state", "year"), model = "within")
summary(mod_fe)
  "el nuevo modelo si es lo esperado, mayores tax al alcohol reducen los accidentes fatales"
coeftest(mod_fe, vcov = vcovHC, type = "HC1")  
  "incluimos errores estandar robustos"

"Lo importante es saber que en datos de panel, podemos usar DATOS FIJOS para mejorar las estimaciones"

#Estimaci?n con efectos fijos manual (dummy), quitandole 1 para evitar trampa de dummy
mod_dummy <- lm(rfatal ~ beertax + state - 1, datos)
summary(mod_dummy)
coeftest(mod_dummy, vcov = vcovHC, type="HC1")


