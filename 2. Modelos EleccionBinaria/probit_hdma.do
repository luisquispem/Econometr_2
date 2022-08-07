/*---------------------------------
 Modelos de elección binaria
 Home Mortgage Disclosure Act
 ¿Qué determinantes importan para rechazar hipotecas?
 ¿La raza importa? ¿Estado civil? ¿Educación?
 ----------------------------------
 */

clear all
pwd
import excel "hmda_limpio.xlsx", sheet("Sheet 1") firstrow

*------------------------------------
* Estadísticos descriptivos y otros
*------------------------------------
	//estadística dependiendo de la raza
sum
sum if black ==1
sum if black ==0

*-------------------------------
* Modelo de probabilidad lineal
*-------------------------------
reg deny black pirat hirat lvrat_med lvrat_high chist ///
mhist phist insurance selfemp single hschool

estat hettest 
	//Rechaza H0 -> hay heterocedasticidad
	
reg deny black pirat hirat lvrat_med lvrat_high chist ///
mhist phist insurance selfemp single hschool, vce(r)
	//regresion corregida la heterocedasticidad
	
//el modelo lineal no contempla un output como probabilidad (deberá estar )	

*------------------------
* Modelo probit
*------------------------

probit deny black pirat hirat lvrat_med lvrat_high chist ///
mhist phist insurance selfemp single hschool, vce(r) nolog
	//con errores robustos 
	
*------------------------
* Otros estadísticos
*------------------------
estat ic
	//para el criterio AIC y BIC con criterios similares
	
estat classification
	//el 90.5% de predicciones son correctas
	
ssc install fitstat
fitstat
	
lroc
	//tienen un buen nivel predictivo nuestro modelo, mayor a 0.5

lsens
	//porcentaje de valores correctamente predichos para 0 y 1

*---------------------------
* Efectos marginales
*---------------------------
*Promedio de efectos marginales individuales
margins, dydx(*)
	//al ser black, aumenta la probabilidad de rechazo en 4 puntos porcentuales
	//un incremento de 10 puntos porcentuales de pirat, el rechazo aumenta en 3.5 puntos porcentuales
	//al tener highschool completa, el rechazo disminuye ne 9 puntos porcentuales.
	//y así sigue
	
	
*Efecto marginal evaluado en los valores promedio de los regresores
margins, dydx(*) atmeans

*------------------------
* Modelo logit
*------------------------

logit deny black pirat hirat lvrat_med lvrat_high chist ///
mhist phist insurance selfemp single hschool, vce(r) nolog

*------------------------
* Otros estadísticos
*------------------------
estat ic
estat classification
lroc
lsens