********************************************************************************

*****************    PRÁCTICA PARA EXAMEN ++++++++++++++++++++++++++++++++++++++

********************************************************************************

*****************************
*             IV            *
*****************************
clear all
 use"/Users/emilianoperezcaullieres/Library/CloudStorage/OneDrive-Personal/DOCUMENTOS/CIDE/7-ECO/VII-MICROENCON/LABS/5. Variables Instrumentales/iv_health_new.dta"

* Variables
gen ln_medexpense = ln(medexpense)
gen ln_income = ln(income)
reg ln_medexpense healthinsu age ln_income illnesses
reg healthinsu healthcupon age ln_income illnesses
predict healthinsu_hat
reg ln_medexpense healthinsu_hat age ln_income illnesses

* Regression without the instrument
reg ln_medexpense healthinsu age ln_income illnesses
test healthinsu age ln_income illnesses
local fvalue = string(r(F), "%9.2f")
local pvalue = string(r(p), "%9.3f")
outreg2 using myregressions, replace excel addstat("F-test", `fvalue', "p-value", `pvalue')

* First Stage
reg healthinsu healthcupon age ln_income illnesses
test healthcupon age ln_income illnesses
local fvalue = string(r(F), "%9.2f")
local pvalue = string(r(p), "%9.3f")
outreg2 using myregressions, append excel addstat("F-test", `fvalue', "p-value", `pvalue')

* Second Stage (using ivreg2)
reg ln_medexpense healthinsu_hat age ln_income illnesses
test healthinsu_hat age ln_income illnesses
local fvalue = string(r(F), "%9.2f")
local pvalue = string(r(p), "%9.3f")
outreg2 using myregressions, append excel addstat("F-test", `fvalue', "p-value", `pvalue')


*****************************
*             RDD           *
***************************** 
clear all
 use "/Users/emilianoperezcaullieres/Library/CloudStorage/OneDrive-Personal/DOCUMENTOS/CIDE/7-ECO/VII-MICROENCON/LABS/6. Regresión discontinua/6a/gov_transfers_new.dta"

* Variables
gen treat = income_centered <= 0
gen income_centered2 = income_centered^2
* Polinomio 1
reg support treat c.income_centered c.income_centered#i.treat
outreg2 using mytable, replace ctitle(regpol1) excel
* Polinomio 2
reg support treat c.income_centered c.income_centered#i.treat c.income_centered2 c.income_centered2#i.treat
outreg2 using mytable, append ctitle(regpol2) excel

* RDROBUST 1 Y 2
rdrobust support income_centered, c(0) p(1)
outreg2 using mytable, append ctitle(rdrobust1) excel
rdrobust support income_centered, c(0) p(2)
outreg2 using mytable, append ctitle(rdrobust2) excel

* Gráficas
rdplot support income_centered, c(0) p(1)  graph_options(leg(off) xtitle("Centered Income") ytitle("Support") ) 
rdplot support income_centered, c(0) p(2)  graph_options(leg(off) xtitle("Centered Income") ytitle("Support") )

*Crear Bins 
egen income_centered_bins = cut(income_centered), at(-0.02(0.001)0.02)
collapse (mean) mean_support=support, by(income_centered_bins)
*Plot
gen before = income_centered_bins < 0
twoway (scatter mean_support income_centered_bins) ///
(lfit mean_support income_centered_bins if before) ///
(lfit mean_support income_centered_bins if !before), ///
legend(off) xline(0)

*****************************
*             PUSSY         *
***************************** 
clear all 
use "/Users/emilianoperezcaullieres/Library/CloudStorage/OneDrive-Personal/DOCUMENTOS/CIDE/7-ECO/VII-MICROENCON/LABS/6. Regresión discontinua/6b/mortgages_new.dta"

gen above = qob_minus_kw > 0

* generar las interacciones 
gen interaction_vet = vet_wwko*qob_minus_kw
gen interaction_above = above*qob_minus_kw

* regresion manual
**IMPORTANTE** SE INSTRUMENTA LA VARIABLE Y LA INTERACCIÓN
ivreg2 home_ownership qob_minus_kw (vet_wwko interaction_vet = above interaction_above) if abs(qob_minus_kw) < 12, robust
outreg2 using fuzzyrd, excel replace ctitle("ivreg2 < 12") stats (coef se) dec(3) nocons nodepvar
rdrobust home_ownership qob_minus_kw, c(0) fuzzy(vet_wwko) h(12 12)
outreg2 using fuzzyrd, excel append ctitle("rdrobust < 12") stats (coef se) dec(3) nocons nodepvar

*****************************
*             PANEL         *
***************************** 
clear all 
use "/Users/emilianoperezcaullieres/Library/CloudStorage/OneDrive-Personal/DOCUMENTOS/CIDE/7-ECO/VII-MICROENCON/LABS/7. Datos Panel/nlswork.dta"

global xregre union grade ttl_exp

* OLS Regression
regress ln_wage $xregre
outreg2 using regresiones, excel replace ctitle(VI) stats (coef se ci) dec(3) nor nodepvar

* PANEL
xtset idcode year

* Random Effects Regression
xtreg ln_wage $xregre, re
outreg2 using regresiones, excel append ctitle(VI) stats (coef se ci) dec(3) nor nodepvar

* Fixed Effects Regression
xtreg ln_wage $xregre, fe
outreg2 using regresiones, excel append ctitle(VI) stats (coef se ci) dec(3) nor nodepvar

*forloop para primeras diferencias
foreach var in ln_wage $xregre { 
	by idcode : gen D_`var'=`var'[_n]-`var'[_n-1]
	by idcode (year), sort: gen diff_`var'=`var'[_n]-`var'[_n-1]
}

global diffvar diff_union diff_grade diff_ttl_exp
xtreg diff_ln_wage $diffvar, fe
outreg2 using regresiones, excel append ctitle(VI) stats (coef se ci) dec(3) nor nodepvar

global iv_xlist grade ttl_exp /* hacemos un nuevo global con las variables explicativas */

ivreg2 ln_wage (union=c_city) $iv_xlist /*estimador de variables instrumentales */
outreg2 using regresiones, excel append ctitle(VI) stats (coef se ci) dec(3) nor nodepvar
xtivreg ln_wage (union=c_city) $iv_xlist, re /*estimador de variables instrumentales con efectos aleatorios*/
outreg2 using regresiones, excel append ctitle(VI EA) stats (coef se ci) dec(3) nor nodepvar
xtivreg ln_wage (union=c_city) $iv_xlist, fe /*estimador de variables instrumentales con efectos fijos*/
outreg2 using regresiones, excel append ctitle(VI EF) stats (coef se ci) dec(3) nor nodepvar
xtivreg ln_wage (union=c_city) $iv_xlist, fd /*estimador de variables instrumentales con primeras diferencias*/
outreg2 using regresiones, excel append ctitle(VI PD) stats (coef se ci) dec(3) nor nodepvar


*****************************
*             PANEL         *
***************************** 
clear all 
use "/Users/emilianoperezcaullieres/Library/CloudStorage/OneDrive-Personal/DOCUMENTOS/CIDE/7-ECO/VII-MICROENCON/LABS/8. Diferencias en diferencias/organ_donations.dta"

*** tratamiento
generate treat = 0
replace treat = 1 if state == "California"

*** tratamiento
generate time = 0
replace time = 1 if quarter_num >= 4

*** did
gen did=time*treat

reg rate treat time did
outreg2 using did, excel replace ctitle(MCO) stats (coef se ci) dec(3) nor nodepvar
reg rate treat time did, vce(cluster state)
outreg2 using did, excel append ctitle(Cluster) stats (coef se ci) dec(3) nor nodepvar

collapse (mean) rate, by(quarter_num treat time)

twoway (line rate quarter_num if treat == 1, sort lcolor(blue)) ///
       (line rate quarter_num if treat == 0, sort lcolor(red)), ///
       legend(label(1 "Treated After") label(2 "Treated Before") ///
              label(3 "Untreated After") label(4 "Untreated Before")) ///
       xline(3.5, lpattern(dash) lcolor(black)) ///
       ytitle("Rate") ///
       xtitle("Quarter Number")
	   

*normalizar
gen n_rate=.
forvalues i=0/1 { /*con este loop transformamos la variable Y a cambios respecto a una base 100, donde 2011Q2=100 */
sum rate if quarter_num==3 & treat==`i'
scalar norm=`r(mean)'
replace n_rate=rate / norm*100 if treat==`i'
}

twoway (line n_rate quarter_num if treat == 1, sort lcolor(blue)) ///
       (line n_rate quarter_num if treat == 0, sort lcolor(red)), ///
       legend(label(1 "Treated After") label(2 "Treated Before") ///
              label(3 "Untreated After") label(4 "Untreated Before")) ///
       xline(3.5, lpattern(dash) lcolor(black)) ///
       ytitle("Rate normalized to 2nd. quarted 2011") ///
       xtitle("Quarter Number")
