**********************************************
* FUNDAMENTOS DE ECONOMETRÍA: Trabajo Grupal
* PROFESOR: Juan León Jara Almonte
**********************************************

clear all
cd "C:\Manuel\3. Student\Semester 2026-1\1ECO11 Fundamentos de Econometría (722)\Collaborative Task\Materiales" // Introducir la ruta de acceso donde la base de datos está ubicada
use "base_endes.dta", clear

************************************************************
* 1) Transformación y selección de variables independientes
************************************************************
des

* Ingreso: LISTO
sum v191
gen wealth = v191/100000
sum wealth
xtile riqueza_quintil = wealth, nq(5)
tab riqueza_quintil

* Edad de la madre: LISTO
sum v013
tab v013
tab v013, nolabel
recode v013 (1 2 3 = 0 "madres menores de 30 años") (4 5 6 7 = 1 "madres mayores de 30 años"), gen(madre_mayor_30)
tab madre_mayor_30

* Sexo del niño
tab sexo
tab sexo, nolabel
rename sexo mujer
tab mujer, nolabel

* Edad del niño en meses
sum hw1
drop if hw1>36
sum hw1
egen edad_niño = cut(hw1), at(12, 24, 36) label
recode edad_niño (0 = 0 "menor a dos años")(1 = 1 "dos años o más"), gen (edad_mayorigual2)
tab edad_mayorigual2

* Fiebre en las últimas dos semanas
tab fiebre

* Tiempo de lactancia: LISTO
sum lactancia
tab lactancia
tab lactancia, nolabel
drop if lactancia == 0
recode lactancia (1 2 = 1 "lactancia hasta el primer año") (3 4 = 0 "lactancia después del primer año"), gen (tiempo_lactancia)
tab tiempo_lactancia

* Suplemento de hierro durante el embarazo
tab m45
rename m45 hierro_embarazo
tab hierro_embarazo

* Diarrea en las últimas dos semanas
tab diarrea

* Consumo variado de alimentos
tab diver_aliment
tab diver_aliment, nolabel
recode diver_aliment (0 1 2 3 4 = 0 "baja diversidad dietética") (5 6 7 = 1 "alta diversidad dietética"), gen(alta_vardietétitca)
tab alta_vardietétitca

* Anemia de la madre
tab v457
tab v457, nolabel
recode v457 (4 = 0 "no tiene anemia") (1 2 3 = 1 "tiene anemia"), gen (anemia_madre)
tab anemia_madre
	
* Años de escolaridad de la madre
sum v133
tab v133
recode v133 (0 1 2 3 4 5 6 = 0 "ninguno/primaria") (7 8 9 10 11 = 1 "secundaria") (12 13 14 15 16 17 = 2 "superior"), gen (educación_madre)
tab educación_madre [aw=weight_m]

* Anemia en niño
tab hw57
tab hw57, nolabel
recode hw57 (1 2 3 = 1 "anemia") (4 = 0 "no tiene anemia"), gen (anemia_niño)
tab anemia_niño

* Lugar de residencia
tab shregion
tab shregion, nolabel

******************
* 2) Modelo Logit
******************

* Declarando el diseño muestral
svyset v001 [pweight = weight_m], strata (v022)

* Ejecutando el modelo logit
svy: logit anemia_niño ib5.riqueza_quintil ib2.educación_madre ib1.shregion madre_mayor_30 mujer edad_mayorigual2 fiebre tiempo_lactancia hierro_embarazo diarrea alta_vardietétitca anemia_madre 

margins, dydx(*) atmeans 

*** Riqueza: Grupo de referencia es el quintil más alto
*** Educación de la madre: El grupo de referencia es que la madre tiene educación superior.
*** Área de residencia: El grupo de referencia es vivir en Lima Metropolitana

************************************************************
*3) Contrucción de los indicadores de la bondad del modelo
************************************************************

*Ver el F estadístico y su p-value

*Archer y Lemeshow
estat gof

*Matriz de confusión
predict prob_anemia if e(sample)

gen predicho_anemia = (prob_anemia >= 0.5) if prob_anemia != .

svy: tab anemia_niño predicho_anemia, count cell

**Cálculos
*A=2230, B=551, C=1033, D=856.4
***Sensibilidad 
dis 856.4/(1033+856.4)
dis .45326559*100
*45.33%
***Especificidad
dis 2230/(2230+551)
dis .80186983*100
*80.19%
***R^2 de conteo
dis (2230+856.4)/(2230+551+1033+856.4)
dis .66084275*100
*66.08%

*************************************************
* 4) Efectos diferenciado por área de residencia
*************************************************
 svy: logit anemia_niño ib5.riqueza_quintil##ib1.shregion ib2.educación_madre madre_mayor_30 mujer edad_mayorigual2 fiebre tiempo_lactancia hierro_embarazo diarrea alta_vardietétitca anemia_madre
 testparm i.riqueza_quintil#i.shregion
 
 svy: logit anemia_niño ib5.riqueza_quintil ib2.educación_madre##ib1.shregion madre_mayor_30 mujer edad_mayorigual2 fiebre tiempo_lactancia hierro_embarazo diarrea alta_vardietétitca anemia_madre
 testparm i.educación_madre#i.shregion
 
 svy: logit anemia_niño ib5.riqueza_quintil ib2.educación_madre i.alta_vardietétitca##ib1.shregion madre_mayor_30 mujer edad_mayorigual2 fiebre tiempo_lactancia hierro_embarazo diarrea anemia_madre
testparm i.alta_vardietétitca#i.shregion

svy: logit anemia_niño ib5.riqueza_quintil ib2.educación_madre i.anemia_madre##ib1.shregion madre_mayor_30 mujer edad_mayorigual2 fiebre tiempo_lactancia hierro_embarazo diarrea alta_vardietétitca
testparm i.anemia_madre#i.shregion
  
margins shregion, at(riqueza_quintil=(1 2 3 4 5))

margins educación_madre#shregion
  
margins alta_vardietétitca#shregion

margins anemia_madre#shregion
