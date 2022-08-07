*************************************************************;
* Stock and Watson, capítulo 10
*************************************************************;

clear all
set more off
set linesize 200

*************************************************************;
* Leer datos; 
pwd 
import excel "panel_fatality.xlsx", sheet("panel") firstrow

************************************************************;
* Estadísticos descriptivos;
sum year		
sum state
************************************************************;
* Transformación de variables

* Tasa de fatalidades por 10,000
gen rfatal = (fatal / pop) * 10000

* Dummies por año;
gen y82=(year==1982)
gen y83=(year==1983)
gen y84=(year==1984)
gen y85=(year==1985)
gen y86=(year==1986)
gen y87=(year==1987)
gen y88=(year==1988)

*Edad mínima para tomar
gen da18=(drinkage<19)         
gen da19=(drinkage>=19)*(drinkage<20)
gen da20=(drinkage>=20)*(drinkage<21)
gen da21=(drinkage>=21)

* Primer castigo: Sentencia de carcel o servicio a la comunidad
gen jail_service = (jail == "yes")*(service == "yes")

* Logartimo del ingreso per cápita
gen lincome = log(income)
gen miles_pd = miles / 1000

*Factor
encode state, gen(statef)
drop state
rename statef state
order state

*-----------------------------------------
* Estimación OLS / pooled
*-----------------------------------------
reg rfatal beertax, r
dis "Adjusted Rsquared = " e(r2_a)

*-----------------------------------------
* Modelo con efectos fijos individuales
*-----------------------------------------

* Estimador FE/LSDV
reg rfatal beertax i.state	// usando dummy"estado", obtenemos la estimación de beertax
areg rfatal beertax, absorb(state)	// estimación sin mostrar dummis

* Estimador FE/W
//previamente defino usando "xtset" y "sort" las clases invariables
*Identificar individuo y tiempo
xtset state year
sort state year

xtreg rfatal beertax, fe 
estimates store modfe

* Estimador FE con SE robustos SUPER NECESARIO PARA CORREGIR HETEROCEDASTICIDAD / AUTOCORRELACIÓN
xtreg rfatal beertax, fe vce(cluster state)
* R2 ajustado modelo LSDV
qui areg rfatal beertax, absorb(state)
dis "Adjusted Rsquared = " e(r2_a)

*---------------------------------------------
* Modelo con efectos aleatorios individuales
*---------------------------------------------

xtreg rfatal beertax, re
estimates store modre

*---------------------------------------------
* Prueba de Hausman
*---------------------------------------------
hausman modfe modre
//mantener el orden
//pvalues<5% entonces el modelo FE es el mejor adecuado 

*----------------------------------------------------
* Modelo con efectos fijos individuales y temporales
*----------------------------------------------------
qui reg rfatal beertax i.state y82 y83 y84 y85 y86 y87
testparm i.state y*
//test para evaluar si las dummy años y estado son significativas o no H0:coef=0
//p-value<5%, rechazo H0, las variables son significativas

xtreg rfatal beertax y82 y83 y84 y85 y86 y87, fe 
test y82 y83 y84 y85 y86 y87
estimates store modfet
//test para evaluar no significancia individuales de efectos temporales.
//pvalue<10%, si son significativas al 10%,  

* Estimación con SE robustos
xtreg rfatal beertax y82 y83 y84 y85 y86 y87, fe vce(cluster state)
test y82 y83 y84 y85 y86 y87
//la prueba anterior, al evaluar con errores robustos, si es significativa al 1%


* R2 ajustado modelo con dummies
 qui areg rfatal beertax y82 y83 y84 y85 y86 y87, absorb(state)
 dis "Adjusted Rsquared = " e(r2_a)
 // tiene mayor R2_adj con efectos indiv y temporales
 
*Prueba de Hausman
hausman modfet modre
//tenemos que el modelo de efectos fijos indiv-tempor es mejor que el modelo random

*----------------------------------------------------
* Modelo con efectos fijos y controles
*----------------------------------------------------

xtreg rfatal beertax drinkage jail_service miles_pd unemp lincome ///
y82 y83 y84 y85 y86 y87, fe vce(cluster state)
test y82 y83 y84 y85 y86 y87
//verificar si los efectos temporales son estadisticamente significativos
test enemp lincperc
//significancia de controles

* R2 ajustado modelo con dummies
 qui areg rfatal beertax drinkage jail_service miles_pd unemp lincome ///
 y82 y83 y84 y85 y86 y87, absorb(state)
 dis "Adjusted Rsquared = " e(r2_a)

 *----------------------------------------------------------------------