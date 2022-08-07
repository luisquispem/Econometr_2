*************************************************************;
*Acemoglu
*************************************************************;

clear all

*************************************************************;
* Leer datos; 
cd "H:\Mi unidad\2021-2 pucp\econometria 2\5. Panel dinámico" 
import excel "datos_acemoglu_clase", sheet("5 Year Panel") firstrow

//Definimos las dimensiones (individuo y tiempo)
tsset code_numeric year_numeric
sort code_numeric year_numeric

//Creamos dummies para cada año (lambda)
tab year, gen (yr)
br yr*
//Creamos dummies para cada código de país (alfa)
tab code, gen(cd)
br cd*

// Sample =1 si los años >= 1960

*--------------------------------------------------------------------------------------------
*T2.C1 REGRESION POOL -> D_t = D_t-1 +y_t-1 +año(lambda) **************************
*---------------
reg fhpolrigaug L.(fhpolrigaug lrgdpch) yr* if sample==1, vce(cluster code)
nlcom _b[L.lrgdpch]/ (1- _b[L.fhpolrigaug])	//prueba no lineal: acumulacion de y 
											//p-value<5%, es significativo
											
twoway (scatter fhpolrigaug lrgdpch)	//no tomamos en cuenta los los efectos fijos, pooled esttá mal ;c
cor fhpolrigaug lrgdpch	//correlación fuerte, no hay aun relacion causal

*--------------------------------------------------------------------------------------------
*T2.C2 REGRESION FE******************************************************************
*---------------
//regresión con dummys (como Acemoglu)
reg fhpolrigaug L.(fhpolrigaug lrgdpch) yr* cd* if sample==1, vce(cluster code)
	//desaparece el efecto causal del ingreso sobre la democracia
nlcom _b[L.lrgdpch]/ (1- _b[L.fhpolrigaug])
	//el efecto acumulado es 0, HO: no hay se acepta


//regresion con herramientas stata
xtreg fhpolrigaug L.(fhpolrigaug lrgdpch) yr* if sample==1, fe vce(cluster code)
nlcom _b[L.lrgdpch]/ (1- _b[L.fhpolrigaug])
	//resultado equivalente
	
*--------------------------------------------------------------------------------------------
*T2.C3 REGRESION ANDERSON-HSIAO ***********************************************
*diferencia en niveles D = diferencia en niveles rezadados de (D + Y) e instrumentos rezago(2) de (D+Y)
*---------------
ivreg D.fhpolrigaug yr* (LD.(fhpolrigaug lrgdpch)= L2.(fhpolrigaug lrgdpch)) ///
if sample==1, cluster(code)
	//son iguales, arriba o abajo
ivregress 2sls D.fhpolrigaug yr* (LD.(fhpolrigaug lrgdpch)= L2.(fhpolrigaug lrgdpch)) ///
if sample==1, cluster(code)
	//tenemos y negativa y no significativa
nlcom _b[LD.lrgdpch]/ (1- _b[LD.fhpolrigaug]) 
	//el efecto acumulado es negativo pero no signficiativo

*--------------------------------------------------------------------------------------------
*T2.C4 REGRESIÓN ARELLANO-BOND*********************************************************
*---------------
* passthru usa los niveles como instrumentos en la 
* ecuación expresada en primeras diferencias
*gmm(variable a instrumentalizar)
xtabond2 fhpolrigaug L.(fhpolrigaug lrgdpch) yr* if sample==1, ///
gmm(L.(fhpolrigaug)) iv( yr*) iv(L2.lrgdpch, passthru) noleveleq robust 
	//regresion con variables instrumentales, varaibles significativas. OK!
	//instrumentos usados: dummys alos; rezago2 de y; IVGMM= todos los rezagos de la endógena desde 2
	//AR(1) rechazo hipotesis nula, hay correlacion de orden 1; pero acepto de orden 2. OK!
	//Sargan y Hansen, no rechazo H0: los instrumentos adicionales no débiles
	
	//resultado: no hay efecto causal del ingreso sobre la democracia, y si lo hay 
	//es un efecto negativo!!
	
nlcom _b[L.lrgdpch]/ (1- _b[L.fhpolrigaug]) 
	//efecto acumulativo negativo y poco significativo
	

*-----------------------------------------------------------------------
* Cuadros 5 y 6
*-----------------------------------------------------------------------

*---------------
*T5.C4 - usando tasa de ahorro como iv - FE 2stages 
*---------------
  ivregress 2sls fhpolrigaug yr* cd* (L.lrgdpch=L.(L.nsave)) if sample==1, ///
  cluster(code)

  ///Alternativa Blatagi (panel data), Within-2SLS
 *xtivreg fhpolrigaug yr* (L.lrgdpch=L.(L.nsave)) if sample==1, ///
 *fe vce(cluster code)

*---------------
*T5.C5
*---------------
  ivregress 2sls fhpolrigaug L.fhpolrigaug yr* cd* (L.lrgdpch=L.(L.nsave)) ///
  if sample==1, cluster(code)
  
  ///Alternativa Blatagi (panel data), Within-2SLS
  xtivreg fhpolrigaug L.fhpolrigaug yr* (L.lrgdpch=L.(L.nsave)) if sample==1, ///
  fe vce(cluster code)

  //son sesgados e inconsistentes porque se realizan usando FE, tanto estático como
  //dinámico, y por ello no se realiza una buena estimación
  
*-----------------------------------------------------
* Estimación GMM con variables instrumentales
	//variable instrumental, 2rezago de la tasa de ahorro 
*-----------------------------------------------------
*---------------
*T5.C6
*---------------
  xtabond2 fhpolrigaug L.(fhpolrigaug lrgdpch) yr* if sample==1, ///
  gmm(L.(fhpolrigaug)) iv( yr*) iv(L.(L.nsave)) noleveleq robust 
  //tenemos una estimación consistente y significativa 

*---------------
*T6.C6
*---------------
  xtabond2 fhpolrigaug L.(fhpolrigaug lrgdpch) yr* if sample==1, ///
  gmm(L.(fhpolrigaug)) iv( yr*) iv(L.(worldincome)) noleveleq robust 
  //tenemos una estimación consistente y significativa 

 //el efecto del ingreso sobre la democracia es negativo y significativo, en las 2 ultimas regresiones. 

*---------------------------------------------------------------------------




