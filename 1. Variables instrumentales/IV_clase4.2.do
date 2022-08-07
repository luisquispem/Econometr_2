/*************************************
  Ejemplo IV 
  Acemoglu et al. (2001)
**************************************/
* Econometría 2 - Prof. Erick Lahura
*-------------------------------------
clear all
pwd 
import excel "acemoglu_datos_64", sheet("datos") firstrow

*--------------------------
* Matriz de correlaciones
*--------------------------
cor logpgp95 avexpr logem4 euro1900

*--------------------------
* Gráfico de dispersión
*--------------------------
*Dispersión
twoway (scatter logpgp95 avexpr), title(Instituciones e Ingreso per cápita) subtitle((Correlación = 0.74), size(small))
*Dispersión con línea de regresión
twoway (scatter logpgp95 avexpr) (lfit logpgp95 avexpr) , title(Instituciones e Ingreso per cápita) subtitle((Correlación = 0.74), size(small))

*--------------------------
* Estimación por OLS
*--------------------------
regress logpgp95 avexpr, vce(hc3)	//además usamos errores robustos

*--------------------------
* Estimación por IV/TSLS
*--------------------------
ivregress 2sls logpgp95 (avexpr = logem4), vce(r) first		//usamos la VI=logem4, errores robustos y visualizamos la primera etapa (first)
//si el t^2>10, el instrumento NO es débil!!!!!!!!!!!!!!!!!!!!

*--------------------------
* Pruebas de diagnóstico	
*--------------------------

* (a) Test de exogeneidad
*--------------------------
ivregress 2sls logpgp95 (avexpr = logem4), vce(r) first		
estat endogenous avexpr			//ya que los p-values son pequeños, rechazamos la hipotesis, por lo que el regresor es endógeno.

* (b) Test de relevancia
*-------------------------
ivregress 2sls logpgp95 (avexpr = logem4), vce(r) first
estat firststage				//El estadiístico F>10, nos dice que el instrumento es relevante	
*(Manual)
reg avexpr logem4, vce(r)
test (logem4)

*----------------------------------------
* Estimación por IV/TSLS. Modelo general
*----------------------------------------

*--------------------------
* Estimación por OLS y TSLS
*--------------------------
regress logpgp95 avexpr lat_abst asia africa malfal94, vce(hc3)

ivregress 2sls logpgp95 lat_abst asia africa malfal94 (avexpr = logem4 euro1900) , vce(r) first	//las variables extra son de control

*--------------------------
* Pruebas de diagnóstico
*--------------------------

* (a) Test de exogeneidad
*--------------------------
ivregress 2sls logpgp95 lat_abst asia africa malfal94 (avexpr = logem4 euro1900) , vce(r) first
estat endogenous avexpr		//la variable avexpr es exógena

* (b) Test de relevancia
*-------------------------
ivregress 2sls logpgp95 lat_abst asia africa malfal94 (avexpr = logem4 euro1900) , vce(r) first
estat firststage		//F>10, los instrumentos NO SON DÉBILES

* (c) Test de sobreidentificación
*-------------------------------------
ivregress 2sls logpgp95 lat_abst asia africa malfal94 (avexpr = logem4 euro1900) , vce(r) first
estat overid			//aceptamos que los instrumentos son exógenos, por el pvalue "alto"