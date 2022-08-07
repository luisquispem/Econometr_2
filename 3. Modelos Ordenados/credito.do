/*---------------------------------
 Modelos de elección múltiple
 Credit scoring 
 ¿Qué indicadores determinan el rating crediticio de una empresa?
 ----------------------------------
 */

clear all
cd "C:\Users\Luis PUCP\Desktop\2021-2 pucp\econometria 2\3. Modelos Ordenados"
import excel "credito.xlsx", sheet("Sheet1") firstrow

*Analisis descriptivo
sum RATING INVGRADE BOOKLEV WKA RETA EBIT LOGSALES

*logit BINARIO
logit INVGRADE BOOKLEV EBIT LOGSALES RETA WKA
	//efectos marginales de todos
	margins, dydx(*)
		//Estar apalancado disminuye la probabilidad de ser de inversión, mientras que 
		//a un 1% más de EBIT aumentan las probabilidades en 0.51 puntos porcentuales

/*Probabilidad e obtener grado de inversión si pasa del  p25=0.17 a
a p75=
*/
margins, at(BOOKLEV=0.169804) atmeans
	//un 54% de ser de inversión
margins, at(BOOKLEV=0.387748) atmeans
	//un 31% de ser de inversión, disminuye en (54-31)%
	
	
*logit ORDENADO
ologit RATING BOOKLEV EBIT LOGSALES RETA WKA, nolog

margins, dydx(LOGSALES)
	//un aumento de ventas, disminuye la probabilidad de ser calificado entre 1 y 3
	//y aumenta las probabilidades de ser calificado entre 4 y 7
marginsplot

margins, dydx(BOOKLEV)
	//un aumento del apalancamiento aumenta la probabilidad de ser calificado entre 1 y 3
	//y un aumento del apalancamiento reduce la probabilidad de ser calificado entre 4 y 7
marginsplot

/*Probabilidad e obtener grado de inversión si pasa del  p25=0.17 a
a p75=
*/
margins, at(BOOKLEV=0.169804) atmeans
margins, at(BOOKLEV=0.387748) atmeans

*Predicción solo 1 categoría
// y=1 | booklev = 0.169804 y el resto promedio	
margins, predict(outcome(1)) at(BOOKLEV=0.169804) atmeans

// y=1 | booklev = 0.387748 y el resto promedio	
margins, predict(outcome(1))  at(BOOKLEV=0.387748) atmeans

// y=4 | booklev = 0.169804 y el resto promedio	
margins, predict(outcome(1)) at(BOOKLEV=0.169804) atmeans
