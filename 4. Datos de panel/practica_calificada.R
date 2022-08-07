#-----------------------------------------
# PC3
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
datos <- read_excel("wagepan1.xlsx")

#--------------------------
#Estimaci?n OLS pooled
#-------------------------
"deseamos estimar el salario a base de educacion, experiencia, experiencia^2 y otras variables del individuo"
pooled_ols <- plm(lwage ~ educ+exper+expersq+black+hisp+married+union+d81+d82+d83+d84+
                   d85+d86+d87, datos, model = "pooling")
summary(pooled_ols)
"la regresi?n pooled presenta variables con poca significancia, como las dummys de a?o. 
Adem?s, estamos omitiendo la heterocedasticidad provocada por variables invariables en el tiempo como origen etnico.
Mediante este modelo no obtendremos los mejores estimadores ya que la varianza ser? distinta por individuo"

#--------------------------
#Estimaci?n con efectos fijos -> black, hisp, educ
#--------------------------
mod_fe<- plm(lwage ~ educ+exper+expersq+black+hisp+married+union+d81+d82+d83+d84+
               d85+d86+d87, datos, model = "within")
summary(mod_fe)
coeftest(mod_fe, vcov = vcovHC, type = "HC1")  

"Desaparecen las variables de origen etnico que eran 2, una dummy de a?o y los a?os de educaci?n. De esta forma tenemos
una estimaci?n que obvie los efectos fijos y nos brinden una mejor estimaci?n para hallar el salario. Depender? de la 
experiecnia, experiecnia^2 y otras variables como si esta casado o si pertenece a un sindicato. 
La variable exper toma m?s relevancia gracias a esta estiamcion WITHIN
"

#--------------------------
#Estimaci?n con efectos aleatorios
#--------------------------

mod_re<- plm(lwage ~ educ+exper+expersq+black+hisp+married+union+d81+d82+d83+d84+
               d85+d86+d87, datos, model = "random")
summary(mod_re)
coeftest(mod_re, vcov = vcovHC, type = "HC1")  

"tengo m?s varaibles, sin embargo son poco significativas. el parametro de experiecnia siguesiendo mayor, pero
vuelve a aparecer educacion, que es la segunda mayor. No parece ser el mejor modelo. "
#--------------------------
#Test Hausman
#--------------------------
pFtest(mod_fe, pooled_ols)
"tenemos que el modelo con efectos fijos es mejor, p-value casi 0.  H0: usar OLS  H1: usar FE"

pFtest(mod_fe, mod_re)
"tenemos un estimador insesgado con FE, porque el p-value>0.05. H0: usar FE H1: usar RE"
