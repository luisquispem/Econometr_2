#--------------
#MODELOS DE ELECCION BINARIA
#--------------

#--------------
rm(list=ls())
#--------------
#---------------
# Librerías
#---------------
library(dplyr)
library(readxl)
library(lmtest)
library(stargazer)
library(AER)
library(mfx)
library(DescTools)
#-----------------------
# Ruta de trabajo
#-----------------------
script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)

    #importamos excel y editamos nombres de variables y su formato a numerico (2) y la nombramos la base de datos
hmda <- read_excel("hmda.xlsx")
colnames(hmda)[12] <- "black"
hmda$chist <- as.numeric(hmda$chist)
hmda$mhist <- as.numeric(hmda$mhist)
datos <- hmda
    
#-----------------------
# Manipulación de variables
#-----------------------
    
    #convertimos variables dummy y reemplazamos
datos <- mutate(datos,deny=ifelse(deny=="yes",1,0))
datos <- mutate(datos,black=ifelse(black=="yes",1,0))
datos <- mutate(datos,phist=ifelse(phist=="yes",1,0))
datos <- mutate(datos,selfemp=ifelse(selfemp=="yes",1,0))
datos <- mutate(datos,insurance=ifelse(insurance=="yes",1,0))
datos <- mutate(datos,condomin=ifelse(condomin=="yes",1,0))
datos <- mutate(datos,single=ifelse(single=="yes",1,0))
datos <- mutate(datos,hschool=ifelse(hschool=="yes",1,0))

    #convertimos variable dummys para 3 intervalos
datos <- mutate(datos,lvrat_low=ifelse(lvrat<0.8,1,0))
datos <- mutate(datos,lvrat_med=ifelse(0.8<=lvrat & lvrat<=0.95,1,0))
datos <- mutate(datos,lvrat_high=ifelse(lvrat>0.95,1,0))

    #nos quedamos con determinadas variables y guardamos
datos <- datos[,c(1,12,2:7,10,9,13,14,8,11,15:17)]
datos <- as.data.frame(datos)

#-----------------------
# Estadísticos descriptivos
#-----------------------

    #analizamos algunos estadisticos como media y varianza
"Para todos"
stargazer(datos, type="text")

"Para afroamericanos"
datos_b <- filter(datos,black==1)
datos_b <- datos_b[ ,-2]
stargazer(datos_b, type="text")

"Para blancos"
datos_w <- filter(datos,black==0)
datos_w <- datos_w[,-2]
stargazer(datos_w, type="text")

#-----------------------
# Diferencias de medias
#-----------------------

"comparamos la igualdad de medias de deny, con varianzas iguales a 95% de confianza"
t.test(datos_b$deny, datos_w$deny, alternative = c("two.sided"), mu = 0,
       paired = FALSE, var.equal = TRUE, conf.level = 0.95)
       #nos fijamos en la H0 que dice que las medias son iguales, y el p-value nos dice que rechazamos
        #medias de rechazo por raza no iguales.
"comparamos la igualdad de medias en payment-ingreso ratio"
t.test(datos_b$pirat, datos_w$pirat, alternative = c("two.sided"), mu = 0,
       paired = FALSE, var.equal = TRUE, conf.level = 0.95)
       #nos fijamos y aceptamos la H0, no hay decisión racista en entregar créditos
       #hay distintos motivos para rechazar a los afroamericanos más alla de la raza

#-----------------------
# Modelo de probablidad lineal
#-----------------------
"planteamos mnodelo lineal con 2 regresores"
eq.mpl <- lm(deny ~ black + pirat, datos)
summary(eq.mpl)

#-----------------------
# Modelo probit
#-----------------------
"planteamos modelo probit con 2 regresores"
eq.probit <- glm(deny ~ black + pirat, datos, family = binomial(link="probit"))
summary(eq.probit)
    #estos coeficientes estimados NO SON LOS EFECTOS MARGINALES. son la dirección del efecto

#-----------------------
# Criterios PROBIT - BONDAD DE AJUSTE
#-----------------------

"calcular la proporcion de observaciones correctamnete predichas"
probabilities <- predict(eq.probit, type= "response")   
    #guardado como un vector de predicciones del modelo probit >0.5
abs_prob <- abs(probabilities - datos$deny)
    #calcular dierencia entre las probabilidades y las observaciones
correct <- ifelse(abs_prob<0.5, 1,0)
    #si el valor predicho es correcto, se pone 1, sino 0
porc_correct <- sum(correct)/length(probabilities)
    #ratio de suma de correctos igual a 1 sobre cantidad de predicciones
porc_correct
    #nos enseña el porcentaje correcto

"Pseudo R2, con el ajustado, con criterios y logverosimilitud"
PseudoR2(eq.probit, c("McFadden", "McFaddenAdj", "AIC", "BIC", "logLik", "logLik0"))
    #tenemos que es un buen modelo, con R2 similares, los criterios similares y logLik altos

"Efectos marginales para individuo i"
mfx.probit_APE <- probitmfx(deny ~ black +  pirat, datos, atmean=FALSE)
    mfx.probit_APE
    #ser afroamericano aumenta la prob de rechazo en 16.7 puntos porcentuales, y por 
    #aumento de 10 puntos porcentuales de pirat, aumenta 5 puntos porcentuales ser denegado
"Efectos marginales para la media de la muestra"
mfx.probit <- probitmfx(deny ~ black +  pirat, datos, atmean=TRUE)
    mfx.probit
    #nos ofrece resultados similares
    
#-----------------------
# Modelo logit
#-----------------------

"planteamos modelo logit"
eq.logit <- glm(deny ~ black + pirat, datos, family = binomial(link="logit"))
summary(eq.logit)

"calcular la proporcion de observaciones correctamnete predichas"
probabilities <- predict(eq.logit, type= "response")   
#guardado como un vector de predicciones del modelo probit >0.5
abs_prob <- abs(probabilities - datos$deny)
#calcular dierencia entre las probabilidades y las observaciones
correct <- ifelse(abs_prob<0.5, 1,0)
#si el valor predicho es correcto, se pone 1, sino no
porc_correct <- sum(correct)/length(probabilities)
porc_correct
#nos enseña el porcentaje correcto

"Pseudo R2, con el ajustado, con criterios y logverosimilitud"
PseudoR2(eq.logit, c("McFadden", "McFaddenAdj", "AIC", "BIC", "logLik", "logLik0"))

"Efectos marginales para individuo i"
mfx.logit_APE <- logitmfx(deny ~ black +  pirat, datos, atmean=FALSE)
mfx.logit_APE
"Efectos marginales para la media de la muestra"
mfx.logit <- logitmfx(deny ~ black +  pirat, datos, atmean=TRUE)
mfx.logit
    #analizamos los resultados, que serán similares a probit

#-----------------------
# Resumen de modelos
#-----------------------
stargazer(eq.mpl, eq.probit, eq.logit, type="text")


#-----------------------
#  MODELO FINAL - STOCK & WATSON
#-----------------------
"estimamos por probit el modelo del autor"
eq.probit <- glm(deny ~ black + pirat + hirat + lvrat_med + lvrat_high + 
                        chist + phist + mhist + insurance + single + hschool + selfemp, datos,
                        family = binomial(link="probit"))
summary(eq.probit)
    #a diferencia del primer modelo, black está correlacionado con el error y afecta al hecho que se 
    #rechaze el crédito, porque el parámetro de black CAE MÁS DE LA MITAD, habían muchas variables omitidas en el modelo
    #el cambio brusco en la variable de interés confirma que las demás variables de control son necesarias para explicar causalidad
        #hay variables no significativas, como hirat o mhist, y otras significativas como ser soltero.
    #la variable BLACK es exógena, y significativa, evita la doble causalidad si usamos las var de control correctas.
    #al final, BLACK puede afectar el que se deniege el credito positivamente
"calcular la proporcion de observaciones correctamnete predichas"
probabilities <- predict(eq.probit, type= "response")   
abs_prob <- abs(probabilities - datos$deny)
correct <- ifelse(abs_prob<0.5, 1,0)
porc_correct <- sum(correct)/length(probabilities)
porc_correct

"Pseudo R2, con el ajustado, con criterios y logverosimilitud"
PseudoR2(eq.logit, c("McFadden", "McFaddenAdj", "AIC", "BIC", "logLik", "logLik0"))

"Efectos marginales para individuo i"
mfx.logit_APE <- logitmfx(deny ~ black + pirat + hirat + lvrat_med + lvrat_high + 
                              chist + phist + mhist + insurance + single + hschool + selfemp, datos, atmean=FALSE)
mfx.logit_APE
"Efectos marginales para la media de la muestra"
mfx.logit <- logitmfx(deny ~ black +  pirat, datos, atmean=TRUE)
mfx.logit


