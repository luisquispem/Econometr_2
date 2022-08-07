*************************************************************;
*Econometría 2
*PC4
*Luis Quispe Macavilca 
*20170860

clear all

*************************************************************;
* Leer datos; 
pwd 
use "mus08psi.dta", clear

*ordenamos el panel dinámico
tsset id t		//7 periodos
order id t lwage


************************
*Pregunta1
*Regresión OLS


reg D.lwage LD.lwage L2D.lwage if t>=4,  robust 
// A pesar de tener estimadores significativos, los resultados de la estimación pueden 
// ser anormales. Esperariamos que la diferencia respecto al salario anterior mejore, ya que 
// el salario tienda a crecer en el tiempo, pero también puede que no siempre las diferencias 
// sean cada vez mayores, ya que a determinadas edades los salarios se estancan (madurez adulta)
// mientras que cuando son muestras jóvenes esperamos que las diferencias crezcan.


************************
*Pregunta2
	//Usando Arellano-Bond, para t=4 tenemos 3 instrumentos rezagados y 
	//para t=5 tenemos 4 instrumentos rezagados
	

************************
*Pregunta3
*Usar xtabond2
*ssc install xtabond2

*creamos dummy para t
tab t, gen (periodo)
br periodo*

*regresion
xtabond2 D.lwage LD.lwage L2D.lwage periodo* if t>=4, ///
	gmm(LD.lwage) iv(periodo*) twostep r

		//obtenemos una  estimación con estimadores significativos, 
		//además los estimadores son menores que en el modelo OLS
		//Además tenemos que rechazamos la prueba AR(1) y mantenemos la AR(2), 
		//nuestros errores no tienen correlación de orden 2.
		//Además rechazamos J-test y aceptamos Sargan, por lo que tenemos que las 
		// instrumentos no son bueno
		//Mejor nos quedamos con la estimación OLS antes de Arellano-Bond, no 
		//tiene la forma para estimar por panel dinámico.
		


