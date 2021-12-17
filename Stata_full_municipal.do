clear

use "D:\FM\andlun\df.dta"


*DROP ØKOMUNER PGA FOR LILLE VARIATION - FOLK HAR TENDENS TIL AT FLYTTE VÆK
{/*
tabulate forstekom if counters==1
tabulate sidstekom if counters==1*/
*drop if counters==1 & (forstekom==825|forstekom==741|forstekom==563|forstekom==492|forstekom==411|sidstekom==825|sidstekom==741|sidstekom==563|sidstekom==492|sidstekom==411)

*Fjerner alle der er flyttet som 14-årige
generate alderhjemmefra=hjemmefra_aar-cohort
count if par_indkomst<=0
count if par_indkomst<=0 & (counters==1|counters==0) 
drop if par_indkomst<=0
}

count if (forstekom==825|forstekom==741|forstekom==563|forstekom==492|forstekom==411|sidstekom==825|sidstekom==741|sidstekom==563|sidstekom==492|sidstekom==411)


* 1) Gen percentile (+ deciles rankings)
replace far_uddgruppe=. if far_uddgruppe>80
replace mor_uddgruppe=. if mor_uddgruppe>80

generate par_hojest_udd=max(far_uddgruppe, mor_uddgruppe)
egen par_indkomst=rmean(far_indkomst mor_indkomst)
egen par_perc=xtile(par_indkomst), by(cohort) nq(100)
egen perc=xtile(indkomst), by(cohort) nq(100)


*gen var for fixed effects estimations
capture drop par_dec
egen par_dec=xtile(par_indkomst),by(cohort) nq(10)
*cohort inddeling
capture drop cohortpar
generate cohortpar=recode(cohort,1975,1980,1985)
generate alderflytpar=recode(alderflyt,5,10,15,20,25,30)

save "D:\FM\andlun\df_perc.dta", replace



* 2) Regress by CZ and cohort and find predicted/fitted income ranked conditional on cohort and CZ
use "D:\FM\andlun\df_perc.dta"

capture drop y_hat_perm
capture drop y_hat_flyt0
capture drop y_hat_flyt1

generate y_hat_perm=.
generate y_hat_flyt0=.
generate y_hat_flyt1=.

levelsof forstekom, local(komlist)

foreach lev of local komlist{
	capture noisily regress perc par_perc if forstekom==`lev' & counters==0
	capture noisily predict work
	capture noisily replace y_hat_perm=work if e(sample)
	capture noisily replace y_hat_flyt0=work if forstekom==`lev' & counters==1
	capture noisily replace y_hat_flyt1=work if sidstekom==`lev' & counters==1
	capture noisily drop work
}

/*
levelsof forstekom, local(komlist)
forvalues y=1975(5)1985 {
	foreach lev of local komlist{
		capture noisily regress perc par_perc if cohortpar==`y' & forstekom==`lev' & counters==0
		capture noisily predict work
		capture noisily replace y_hat_perm=work if e(sample)
		capture noisily replace y_hat_flyt0=work if cohortpar==`y' & forstekom==`lev' & counters==1
		capture noisily replace y_hat_flyt1=work if cohortpar==`y' & sidstekom==`lev' & counters==1
		capture noisily drop work
	}
}
*/

*Tager roughly 20 min
*3) Find DELTA for hver flytter
capture drop pred_delta
generate pred_delta=y_hat_flyt1-y_hat_flyt0


save "D:\FM\andlun\df_pred.dta", replace

clear


* _____________________ 4) Opstil regression (baseline semi-parametric) __________________________
clear
use "D:\FM\andlun\df_pred.dta"
count if missing(pred_delta) & counters==1
count if counters==1




* _____________________ 4) Opstil regression (baseline semi-parametric)  (A)lav tjek for nødvendigt niveau af __________________________

******************************************4.A)****************************************
eststo clear
* (1) Naive regressop
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg perc i.alderflyt#c.pred_delta if counters==1
eststo mod1
* (2) Cohort
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg perc i.alderflyt#c.pred_delta i.cohort if counters==1
eststo mod2
* (3) Cohort + Age og move
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg perc i.alderflyt i.cohort i.alderflyt#c.pred_delta if counters==1

eststo mod3
* (4a) Age of Move + Decile + Delta*alderflyt
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg perc i.alderflyt i.cohort c.pred_delta#i.alderflyt c.pred_delta#i.cohort if counters==1,fe
eststo mod4a
*(4b) Age of Move + Decile + Delta*alderflyt
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg perc i.alderflyt i.cohortpar c.pred_delta#i.alderflyt c.pred_delta#i.cohortpar if counters==1,fe
eststo mod4b
*(5) Cohort*Origin * Decile+Age of Move + Delta*alderflyt
capture drop fixgroup
egen fixgroup=group(forstekom par_dec alderflyt)
xtset fixgroup
xtreg perc i.cohortpar c.pred_delta#i.alderflyt c.pred_delta#i.cohortpar if counters==1,fe
eststo mod5
*(6) Parten_dec*Origin + Age of Move + Decile
capture drop fixgroup
egen fixgroup=group(forstekom par_dec cohortpar alderflyt)
xtset fixgroup
xtreg perc c.pred_delta#i.alderflyt c.pred_delta#i.cohortpar if counters==1,fe
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)
eststo mod6
*(6.b) Parten_dec*Origin + Age of Move + Decile
capture drop fixgroup
egen fixgroup=group(forstekom par_dec cohortpar alderflytpar)
xtset fixgroup
xtreg perc c.pred_delta#i.alderflyt c.pred_delta#i.cohortpar if counters==1,fe
eststo mod7

esttab, stats(N r2_w r2_b r2_o) keep(*.alderflyt#c.pred_delta)
esttab using "D:\FM\andlun\fixedeffects_income.tex", stats(N r2_w r2_b r2_o) keep(*.alderflyt#c.pred_delta) label replace booktabs ///
alignment(D{.}{.}{-1})


eststo clear
*Fixed effect regression (baseline semi-parametric)
capture drop fixgroup
egen fixgroup=group(forstekom par_dec cohortpar alderflyt)
xtset fixgroup
xtreg perc c.pred_delta#i.alderflyt c.pred_delta#i.cohortpar if counters==1,fe
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)
eststo model1

*4. Med kontrol for køn
xtreg perc c.pred_delta#i.alderflyt c.pred_delta#i.cohortpar i.KOEN if counters==1,fe
eststo model2
esttab using "D:\FM\andlun\df_pred.csv", se nostar replace substitute(`""' `=' "")

*4. Med kontrol for køn samt uddannelse
xtreg perc c.pred_delta#i.alderflyt c.pred_delta#i.cohortpar i.KOEN i.par_hojest_udd if counters==1,fe
eststo model3
coefplot, vertical keep(*.alderflyt#c.pred_delta)

esttab, keep(*.alderflyt#c.pred_delta)

esttab model_* using "D:\FM\andlun\indkomst coef.csv", stats(N aic bic r2) keep(c.flyt*#c.pred_delta) replace nostar
*________________________________________________________________________________________________

*_________________________ 5) Baseline parametric estimations ___________________________________
clear
use "D:\FM\andlun\df_pred.dta"

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort if counters==1, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)
eststo model1

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN  if counters==1, noconstant
eststo model2

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)
eststo model3

esttab, stats(N aic bic r2)

esttab using "D:\FM\andlun\coeff.tex", stats(N aic bic r2) label replace booktabs keep(*.alderflyt#c.pred_delta) ///
alignment(D{.}{.}{-1})

esttab using "D:\FM\andlun\indkomst coef.csv", stats(N aic bic r2) keep(*.alderflyt#c.pred_delta) replace nostar

*_________________________________________________________________________________________________

eststo clear
*Printer resultater for KBH
forvalues y=1975(5)1985 {
		regress perc par_perc if cohortpar==`y' & forstekom==101 & counters==0, robust
		eststo
}
esttab using "D:\FM\andlun\kbh_coef.tex", label replace booktabs ///
alignment(D{.}{.}{-1})



	
/*___________________________________________________________________________________________ */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Kinky boiz :)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
gen expose_destination=.
replace expose_destination=30-alderflyt


reg perc i.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#par_dec c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1, noconstant

*Ingen flyt
eststo clear
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#c.alderflyt c.pred_delta#i.cohort  i.KOEN i.par_hojest_udd ///
 c.pred_delta ///
if counters==1, noconstant 
esttab, stats(N aic bic r2)
	
	
 *Simpel (1 kink)
est clear

local m=1
forvalues y=5(1)26 {
	capture drop flytfør_X
	capture drop flytefter_X
	gen flytfør_X=0
	replace flytfør_X=1 if alderflyt<=`y' & counters==1
	
	
	eststo model_`m': reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc i.KOEN i.par_hojest_udd ///
	i.flytfør_X#c.pred_delta#c.alderflyt 0.flytfør_X#c.pred_delta  pred_delta /// * Denne linje specificerer kinket (første)
	if counters==1, noconstant
	local m `++m'
}
esttab model_* using "D:\FM\andlun\1kink_ind.csv", stats(N aic bic r2) keep(pred_delta 0.flytfør_X#c.pred_delta *.flytfør_X#c.pred_delta#c.alderflyt) replace nostar



est clear
local m=1
forvalues x=5(1)16{
	forvalues y=20(1)26 {
		capture drop flytfør_Y
		gen flytfør_Y=2
		replace flytfør_Y=1 if alderflyt<=`y' & alderflyt>`x' &counters==1
		replace flytfør_Y=0 if alderflyt<=`x' & counters==1
		
		eststo model_`m': reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
		i.flytfør_Y#c.pred_delta#c.alderflyt i.flytfør_Y#c.pred_delta pred_delta /// * Denne linje specificerer kinket (første)
		if counters==1, noconstant
		local m `++m'
	}
}
esttab model_* using "D:\FM\andlun\kink2_ind.csv", stats(N aic bic r2) keep(pred_delta *.flytfør_Y#c.pred_delta *.flytfør_Y#c.pred_delta#c.alderflyt) replace nostar



/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Spliny dring boiz :)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
gen expose_destination=.
replace expose_destination=30-alderflyt
	
est clear
local m=1
forvalues x=5(1)26{
		capture drop flyt*
		mkspline flyt1 `x' flyt2 = alderflyt		
		eststo model_`m': reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
		c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
		c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
		pred_delta ///
		if counters==1, noconstant
		local m `++m'
}



esttab model_* using "D:\FM\andlun\1knot_ind.csv", stats(N aic bic r2) keep(pred_delta c.flyt*#c.pred_delta) replace nostar


 *Simpel (2 kink)

est clear
local m=1
forvalues x=5(1)16{
	forvalues y=20(1)26 {
		capture drop flyt*
		mkspline flyt1 `x' flyt2 `y' flyt3 = alderflyt
		capture drop flyttidlig
		capture generate flyttidlig=0
		replace flyttidlig=1 if alderflyt<=`x' & counters==1
		
		eststo model_`m': reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
		c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
		c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
		c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
		pred_delta ///
		if counters==1, noconstant
		local m `++m'
	}
}


esttab model_* using "D:\FM\andlun\2knot_ind_kom.csv", stats(N aic bic r2) keep(pred_delta c.flyt*#c.pred_delta) replace nostar


/*______________________________________________________________________________________________ */	
*******XXXXXXXXXXXXX SIMPLE FIGURER TIL DESKRIPTIV STAT OSV XXXXXXXXXXXXXXXXXX *******************
{
set scheme s2color

*Binned scatterplots
binscatter perc par_perc if counters==0, nquantiles(100)
binscatter perc par_perc if counters==0, nquantiles(100) reportreg

binscatter videregaande par_perc if counters==0, line(qfit) nquantiles(100) 


binscatter perc par_perc if counters==0& forstekom==350 & cohortpar==1985, nquantiles(100) reportreg


*Graf for CPH
twoway (lfit perc par_perc if cohortpar==1975 & forstekom==183 & counters==0) ///
	(lfit perc par_perc if cohortpar==1980 & forstekom==183 & counters==0) ///
	(lfit perc par_perc if cohortpar==1985 & forstekom==183 & counters==0) ///
	(lfit perc par_perc if forstekom==183 & counters==0), ///
	legend(label(1 "1975") label(2 "1980") label(3 "1985") label(4 "Pooled"))
	
clear
use "D:\FM\andlun\df_pred_udd.dta"

twoway (qfit videregaande par_perc if cohortpar==1975 & forstekom==101 & counters==0) ///
	(qfit videregaande par_perc if cohortpar==1980 & forstekom==101 & counters==0) ///
	(qfit videregaande par_perc if cohortpar==1985 & forstekom==101 & counters==0) ///
	(qfit videregaande par_perc if forstekom==101 & counters==0), ///
	legend(label(1 "1975") label(2 "1980") label(3 "1985") label(4 "Pooled"))
	
	
clear 
use "D:\FM\andlun\df_pred_udd.dta"


*Summary statistics (Permanent resident vs. 1 gangsflyttere)
eststo clear
estpost tabstat par_indkomst par_perc par_videregaaende perc videregaande if counters==1|counters==0, stats(mean median sd min max n) col(stat) by(counters)

sort par_indkomst

*Summary statistics (1 gangsflyttere i alderflyt)
generate parho_udd=1 if par_hojest_udd>=30
replace parho_udd=0 if inlist(uddgruppe, 10,15,20,25,35)

eststo clear
sort alderflyt
by alderflyt: eststo: quietly estpost summarize par_perc parho_udd KOEN if counters==1, listwise
esttab, cells("mean") label nodepvar

eststo clear
estpost tabstat par_perc par_hojest_udd KOEN pred_delta y_hat_flyt0, stats(mean) col(variables) by(alderflyt)
tabulate alderflyt if counters==1

eststo clear
estpost tabstat indkomst if cohort==1980 & counters==0, stats(mean) col(variables) by(perc)


*Frequencies of age at first move for 1 time movers and Hjemmefra alder
generate alderhjemmefra=hjemmefra_aar-cohort
local call
forval j=10/35{
	local show=`j'+1
	if mod(`j',2) local call `call' `j' "`show'"
	else local call `call' `j' " "
}
graph bar if (cohort==1970|cohort==1985), over(alderhjemmefra, relabel(`call')) by(cohort)

tabulate alderhjemmefra if cohort==1970
tabulate alderhjemmefra if cohort==1985



drop cum1970
drop cum1985

cumul alderhjemmefra if cohort==1970, gen(cum1970) equal
cumul alderhjemmefra if cohort==1985 & alderhjemmefra>15, gen(cum1985_16) equal
cumul alderhjemmefra if cohort==1985 , gen(cum1985) equal

stack cum1970 alderhjemmefra cum1985 alderhjemmefra cum1985_16 alderhjemmefra, into(c temp) wide clear
line cum1970 cum1985 cum1985_16 temp  if temp>=16 & temp<=35, sort xscale(r(16 30))


*Distribution across birth cohorts
generate flytteaar=cohort+alderflyt
tabulate flytteaar if  counters==1
tabulate hjemmefra_aar if counters==1


*Density plot over parent vs. child income rank
hist par_perc if counters==0, discrete color(blue%0) lcolor(black%80) addplot(hist par_perc if counters==1, discrete color(grey%30) lcolor(grey%0))

hist perc if counters==0, discrete color(blue%0) lcolor(black%80) addplot(hist perc if counters==1, discrete color(grey%30) lcolor(grey%0))

**Kommune mobilitetsoversigt
capture noisily statsby n=e(N) _b,  by(forstekom): regress perc par_perc if counters==0
save "D:\FM\andlun\kom_output.dta", replace

**Kommune mobilitetsoversigt
clear

use "D:\FM\andlun\df_pred_udd.dta"



*Mean parent rank for 1 time movers conditional on child's age at move
tabstat par_perc if counters==1, stats(mean N) col(stat) by(alderflyt) 

/*graph bar (count), over(alderflyt)*/
tabstat par_perc if counters==0 & alderhjemmefra<30, stats(mean N) col(stat) by(alderhjemmefra)

*KOM FORDELING
tabulate forstekom if counters==1

* LARGEST CHANGE OVER TIME IN KOM MEAN RANKING - FIND LARGEST DEV AFTER EXPORT TO EXCEL
table forstekom cohortpar if counters==0, c(mean par_perc)

table forstekom cohortpar if counters==0, c(mean videregaande)
}

/*___________________________________________________________________________________________ */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   UDDANNELSES AFSNIT XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
clear
use "D:\FM\andlun\df_pred.dta"
replace uddgruppe=. if uddgruppe>80


generate ufaglaert=0
generate faglaert=0
generate videregaande=0
generate par_videregaaende=0
replace ufaglaert=1 if inlist(uddgruppe, 10,15,20,25,35)
replace faglaert=1 if uddgruppe==30
replace videregaande=1 if inlist(uddgruppe, 40,45,50,55,60,70,75,80)
replace par_videregaaende=1 if inlist(par_hojest_udd, 40,45,50,55,60,70,75,80)

/*
*2.b) Finder kom gns. attendance rate
*Ufaglærte
capture drop ufaglaert_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate ufaglaert_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.

*
levelsof forstekom, local(komlist)
foreach lev of local komlist{
	capture noisily regress ufaglaert c.par_perc##c.par_perc if forstekom==`lev' & counters==0
	capture noisily predict work
	capture noisily replace ufaglaert_frac_perm=work if e(sample)
	capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & counters==1
	capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & counters==1
	capture noisily drop work
}

generate ufaglaert_frac=udd_hat_flyt1-udd_hat_flyt0
generate ufaglaert_frac_0=udd_hat_flyt0


*Faglærte
capture drop faglaert_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate faglaert_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.


levelsof forstekom, local(komlist)
foreach lev of local komlist{
	capture noisily regress faglaert c.par_perc##c.par_perc if forstekom==`lev' & counters==0
	capture noisily predict work
	capture noisily replace faglaert_frac_perm=work if e(sample)
	capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & counters==1
	capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & counters==1
	capture noisily drop work
}
generate faglaert_frac=udd_hat_flyt1-udd_hat_flyt0
generate faglaert_frac_0=udd_hat_flyt0
*/
*Videregåemde
capture drop videregaande_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate videregaande_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.


*
levelsof forstekom, local(komlist)


forvalues y=1975(5)1985 {
	foreach lev of local komlist{
		capture noisily regress videregaande c.par_perc##c.par_perc if forstekom==`lev' & counters==0 & cohortpar==`y'
		capture noisily predict work
		capture noisily replace videregaande_frac_perm=work if e(sample)
		capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & counters==1 & cohortpar==`y'
		capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & counters==1 & cohortpar==`y'
		capture noisily drop work
	}
}

generate videregaande_frac=udd_hat_flyt1-udd_hat_flyt0
generate videregaande_0=udd_hat_flyt0

/*
*Videregåemde
capture drop videregaande_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate videregaande_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.


*
levelsof forstekom, local(komlist)


foreach lev of local komlist{
	capture noisily regress videregaande par_perc if forstekom==`lev' & counters==0 
	capture noisily predict work
	capture noisily replace videregaande_frac_perm=work if e(sample)
	capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & counters==1 
	capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & counters==1
	capture noisily drop work
}
generate videregaande_frac=udd_hat_flyt1-udd_hat_flyt0
generate videregaande_0=udd_hat_flyt0
*/

save "D:\FM\andlun\df_pred_udd.dta", replace



*4) Reg med  uddannelse
eststo clear
clear

use "D:\FM\andlun\df_pred_udd.dta"
generate alderflytpar=recode(alderflyt,5,10,15,20,25,30)
*use "D:\FM\andlun\df_pred_udd_lin.dta"
eststo clear
* (1) Naive regressop
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg videregaande i.alderflyt#c.videregaande_frac if counters==1
eststo mod1
* (2) Cohort
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg videregaande i.alderflyt#c.videregaande_frac i.cohort if counters==1
eststo mod2
* (3) Cohort + Age og move
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg videregaande i.alderflyt i.cohort i.alderflyt#c.videregaande_frac if counters==1
eststo mod3

* (4a) Age of Move + Decile + Delta*alderflyt
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg videregaande i.alderflyt i.cohort c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort i.par_hojest_udd if counters==1,fe
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)
eststo mod4a

*(4b) Age of Move + Decile + Delta*alderflyt
capture drop fixgroup
egen fixgroup=group(forstekom par_dec)
xtset fixgroup
xtreg videregaande i.alderflyt i.cohortpar c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohortpar if counters==1,fe
eststo mod4b
*(5) Cohort*Origin * Decile+Age of Move + Delta*alderflyt
capture drop fixgroup
egen fixgroup=group(forstekom par_dec alderflyt)
xtset fixgroup
xtreg videregaande i.cohortpar c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohortpar if counters==1,fe
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)
eststo mod5
*(6) Parten_dec*Origin + Age of Move + Decile
capture drop fixgroup
egen fixgroup=group(forstekom par_dec cohortpar alderflyt)
xtset fixgroup
xtreg videregaande c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohortpar if counters==1,fe
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)
eststo mod6
*(6.b) Parten_dec*Origin + Age of Move + Decile
capture drop fixgroup
egen fixgroup=group(forstekom par_dec cohortpar alderflytpar)
xtset fixgroup
xtreg videregaande c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohortpar if counters==1,fe
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)
eststo mod7


esttab, stats(N r2_w r2_b r2_o) keep(*.alderflyt#c.videregaande_frac)
esttab using "D:\FM\andlun\fixedeffects_udd.tex", stats(N r2_w r2_b r2_o) keep(*.alderflyt#c.videregaande_frac) label replace booktabs ///
alignment(D{.}{.}{-1})


*5) Parametric fra Chetty til vores egen specification m. controller

clear

use "D:\FM\andlun\df_pred_udd.dta"
*Chetty
eststo clear
reg videregaande ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort  if counters==1, noconstant
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)
eststo mod1
*chetty m. par udd
reg videregaande ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort i.KOEN if counters==1, noconstant
eststo mod2
*Chetty m. par udd + koen
reg videregaande ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort i.KOEN i.par_hojest_udd if counters==1, noconstant
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)
eststo mod3
*Chetty m. par udd + koen + hojeste interact
reg videregaande ibn.cohort i.cohort#c.videregaande_0 i.alderflyt i.alderflyt#i.par_hojest_udd c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort i.KOEN i.par_hojest_udd if counters==1, noconstant
eststo mod4

*Chetty m vores specification + kontroller
reg videregaande ibn.cohort i.cohort#c.videregaande_0 i.alderflyt i.alderflyt#i.par_hojest_udd c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort i.KOEN par_perc if counters==1, noconstant
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)
eststo mod5
esttab using "D:\FM\andlun\parametric_udd.tex", stats(N aic bic r2) label replace booktabs keep(*.alderflyt#c.videregaande_frac) ///
alignment(D{.}{.}{-1})


/*___________________________________________________________________________________________ */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Kinky boiz :)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
gen expose_destination=.
replace expose_destination=30-alderflyt
est clear


reg videregaande ibn.cohort i.cohort#c.videregaande_0 i.alderflyt i.alderflyt#i.par_hojest_udd c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort i.KOEN par_perc if counters==1, noconstant
esttab using "D:\FM\andlun\udd coef.csv", stats(N aic bic r2) keep(*.alderflyt#c.videregaande_frac) replace nostar

est clear
*Ingen flyt
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN par_perc ///
c.videregaande_frac#c.alderflyt c.videregaande_frac ///
if counters==1, noconstant 
esttab, stats(N aic bic r2)
	
	
 *Simpel (1 kink)
est clear

local m=1
forvalues y=5(1)26 {
	capture drop flytfør_X
	gen flytfør_X=0
	replace flytfør_X=1 if alderflyt>`y' & counters==1
	
	
	eststo model_`m': reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN par_perc ///
	i.flytfør_X#c.videregaande_frac#c.alderflyt 1.flytfør_X#c.videregaande_frac  videregaande_frac /// * Denne linje specificerer kinket (første)
	if counters==1, noconstant 
	local m `++m'
}
esttab model_* using "D:\FM\andlun\kink_udd.csv", stats(N aic bic r2) keep(1.flytfør_X#c.videregaande_frac  videregaande_frac *.flytfør_X#c.videregaande_frac#c.alderflyt) replace nostar




est clear
local m=1
forvalues x=5(1)16{
	forvalues y=20(1)26 {
		capture drop flytfør_Y
		gen flytfør_Y=2
		replace flytfør_Y=1 if alderflyt<=`y' & alderflyt>`x' &counters==1
		replace flytfør_Y=0 if alderflyt<=`x' & counters==1
		
		eststo model_`m': reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
		i.flytfør_Y#c.videregaande_frac#c.alderflyt i.flytfør_Y#c.videregaande_frac  videregaande_frac /// * Denne linje specificerer kinket (første)
		if counters==1, noconstant
		local m `++m'
	}
}
esttab model_* using "D:\FM\andlun\kink2_udd.csv", stats(N aic bic r2) keep(videregaande_frac *.flytfør_Y#c.videregaande_frac   *.flytfør_Y#c.videregaande_frac#c.alderflyt) replace nostar


est clear
*Eksempel
capture drop flytfør_Y

gen flytfør_Y=0
replace flytfør_Y=1 if alderflyt<=23 & alderflyt>8 & counters==1
replace flytfør_Y=2 if alderflyt<=8 & counters==1

reg videregaande i.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  ///
i.flytfør_Y#c.videregaande_frac#c.expose_destination i.flytfør_Y#c.videregaande_frac /// * Denne linje specificerer kinket (første)
if counters==1 
esttab, stats(N aic bic r2)

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Spliny dring boiz :)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
gen expose_destination=.
replace expose_destination=30-alderflyt
	
est clear
local m=1
forvalues x=5(1)26{
		capture drop flyt*
		mkspline flyt1 `x' flyt2 = alderflyt		
		eststo model_`m': reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
		c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
		c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
		videregaande_frac ///
		if counters==1, noconstant 
		local m `++m'
}

esttab model_* using "D:\FM\andlun\1knot_udd.csv", stats(N aic bic r2) keep(c.flyt*#c.videregaande_frac videregaande_frac) replace nostar


 *Simpel (2 kink)

est clear
local m=1
forvalues x=5(1)16{
	forvalues y=20(1)26 {
		capture drop flyt*
		mkspline flyt1 `x' flyt2 `y' flyt3 = alderflyt
		
		eststo model_`m': reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
		c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
		c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
		c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
		c.videregaande_frac ///
		if counters==1, noconstant 
		local m `++m'
	}
}

esttab model_* using "D:\FM\andlun\2knot_udd.csv", stats(N aic bic r2) keep(c.flyt*#c.videregaande_frac videregaande_frac) replace nostar



	
/*___________________________________________________________________________________________ */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Sensitivity tests  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
{
	
clear

use "D:\FM\andlun\df_pred_udd.dta"

capture drop flyt*
mkspline flyt1 12 flyt2 26 flyt3 = alderflyt
*BETTER
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc   i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1, noconstant 
eststo
esttab, stats(N aic bic)

est clear

*___________________Moving to worse versus better neighbourhoods ________________________________
{
	
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
*BETTER
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & pred_delta>0, noconstant 
eststo model1


*WORSE
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & pred_delta<0, noconstant 
eststo model2


capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & pred_delta>0, noconstant 
eststo model3

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & pred_delta<0, noconstant 
eststo model4

esttab using "D:\FM\andlun\better_worse_mun.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})
}
eststo clear


*________________________Decil opdelt effekt____________________________________	
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

forval i=1/5 {
	reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
	c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
	c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
	pred_delta ///
	if counters==1 & par_perc>`i'*20-20 & par_perc<=`i'*20, noconstant 
	eststo
}
esttab using "D:\FM\andlun\decilereg_inc.tex", label replace booktabs keep(c.flyt*#c.pred_delta) ///
alignment(D{.}{.}{-1})

eststo clear

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

forval i=1/5 {
	reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
	c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
	c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
	c.videregaande_frac ///
	if counters==1 & par_perc>`i'*20-20 & par_perc<=`i'*20, noconstant
	eststo
}
esttab using "D:\FM\andlun\decilereg_udd.tex", label replace booktabs keep(c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})

eststo clear

*___________________________MEN VS WOMEN_____________________
{
*MEN
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
*BETTER
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & KOEN==1, noconstant 
eststo model1


*women
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & KOEN==2, noconstant 
eststo model2


capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & KOEN==1, noconstant 
eststo model3

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & KOEN==2, noconstant 
eststo model4

esttab using "D:\FM\andlun\koen.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})
}
eststo clear

*________________Den enkelte cohort gruppes effekt__________________
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

levelsof cohortpar, local(ids)
foreach i of local ids {
	reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
	c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
	c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
	pred_delta ///
	if counters==1 & cohortpar==`i', noconstant 
	eststo
}

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

levelsof cohortpar, local(ids)
foreach i of local ids {
	reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
	c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
	c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
	c.videregaande_frac ///
	if counters==1 & cohortpar==`i', noconstant
	eststo
}
esttab using "D:\FM\andlun\cohortpar_reg.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})



*_____________Antal år brugt i parent og child proxy (KUN INDKOMST HER) ________________
eststo clear




forvalues i=4(4)16 {
	clear
	use "D:\FM\andlun\df_perc.dta"
	gen par_nvalues=cond(missing(mor_nvalues,far_nvalues),max(mor_nvalues,far_nvalues),(mor_nvalues+far_nvalues)/2)
	
	capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

	generate y_hat_perm=.
	generate y_hat_flyt0=.
	generate y_hat_flyt1=.

	levelsof forstekom, local(komlist)

	foreach lev of local komlist{
		capture noisily regress perc par_perc if forstekom==`lev' & counters==0 & par_nvalues>=`i'
		capture noisily predict work
		capture noisily replace y_hat_perm=work if e(sample)
		capture noisily replace y_hat_flyt0=work if forstekom==`lev' & counters==1 & par_nvalues>=`i'
		capture noisily replace y_hat_flyt1=work if sidstekom==`lev' & counters==1 & par_nvalues>=`i'
		capture noisily drop work
	}

	capture drop pred_delta
	generate pred_delta=y_hat_flyt1-y_hat_flyt0

	reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
	c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
	c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
	pred_delta ///
	if counters==1 & par_nvalues>=`i', noconstant 
	eststo
}
forvalues i=3(3)9 {
	clear
	use "D:\FM\andlun\df_perc.dta"
	
		gen par_nvalues=cond(missing(mor_nvalues,far_nvalues),max(mor_nvalues,far_nvalues),(mor_nvalues+far_nvalues)/2)
	
	capture drop flyt*
	mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

	
	generate y_hat_perm=.
	generate y_hat_flyt0=.
	generate y_hat_flyt1=.

	levelsof forstekom, local(komlist)

	foreach lev of local komlist{
		capture noisily regress perc par_perc if forstekom==`lev' & counters==0 & nvalues>=`i'
		capture noisily predict work
		capture noisily replace y_hat_perm=work if e(sample)
		capture noisily replace y_hat_flyt0=work if forstekom==`lev' & counters==1 & nvalues>=`i'
		capture noisily replace y_hat_flyt1=work if sidstekom==`lev' & counters==1 & nvalues>=`i'
		capture noisily drop work
	}


	capture drop pred_delta
	generate pred_delta=y_hat_flyt1-y_hat_flyt0

	
	reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
	c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
	c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
	pred_delta ///
	if counters==1 & nvalues>=`i', noconstant 
	eststo
}

esttab using "D:\FM\andlun\inc_nvalues_reesti.tex", label replace booktabs keep(c.flyt*#c.pred_delta) ///
alignment(D{.}{.}{-1})

eststo clear

/*
*_____________Antal år brugt i parent og child proxy (KUN INDKOMST HER - Ikke gen estimeret) ________________
eststo clear

gen par_nvalues=cond(missing(mor_nvalues,far_nvalues),max(mor_nvalues,far_nvalues),(mor_nvalues+far_nvalues)/2)


capture drop flyt*
mkspline flyt1 12 flyt2 24 flyt3 = alderflyt

forvalues i=4(4)16 {
	reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
	c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
	c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
	pred_delta ///
	if counters==1 & par_nvalues>=`i', noconstant 
	eststo
}
forvalues i=3(3)9 {
	reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
	c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
	c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
	pred_delta ///
	if counters==1 & par_nvalues>=`i', noconstant 
	eststo
}

esttab using "D:\FM\andlun\inc_nvalues.tex", label replace booktabs keep(c.flyt*#c.pred_delta) ///
alignment(D{.}{.}{-1})
*/

*_____________Familietype (Bopæl hos begge, mor eller far) ________________
clear
eststo clear
use "D:\FM\andlun\df_pred_udd.dta"

duplicates drop ID_nr, force

merge 1:1 ID_nr using "D:\FM\andlun\df_tilhors.dta"

drop if _merge==2|_merge==1

replace tilhors=2 if tilhors==2|tilhors==3
replace tilhors=3 if tilhors==4|tilhors==5



reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1 & tilhors==2, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(-1(1)30)

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

levelsof tilhors, local(ids)
foreach i of local ids {
	reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
	c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
	c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
	pred_delta ///
	if counters==1 & tilhors==`i', noconstant 
	eststo
}

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

levelsof tilhors, local(ids)
foreach i of local ids {
	reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
	c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
	c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
	c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
	c.videregaande_frac ///
	if counters==1 & tilhors==`i', noconstant
	eststo
}
esttab using "D:\FM\andlun\sensitivity_tilhors.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})

eststo clear



*________________________ NICE DATA CHECK (remove "false data")_______________________
clear
use "D:\FM\andlun\df.dta"

*cohort inddeling
capture drop cohortpar
generate alderhjemmefra=hjemmefra_aar-cohort
generate cohortpar=recode(cohort,1975,1980,1985)
generate alderflytpar=recode(alderflyt,5,10,15,20,25,30)
gen par_nvalues=cond(missing(mor_nvalues,far_nvalues),max(mor_nvalues,far_nvalues),(mor_nvalues+far_nvalues)/2)

generate par_hojest_udd=max(far_uddgruppe, mor_uddgruppe)
egen par_indkomst=rmean(far_indkomst mor_indkomst)

drop if par_indkomst<=0
drop if forstekom==825|forstekom==741|forstekom==563|forstekom==492|forstekom==411|sidstekom==825|sidstekom==741|sidstekom==563|sidstekom==492|sidstekom==411
drop if alderhjemmefra<=14
drop if nvalues<=3
drop if par_nvalues<=5
drop if cohort<=1974


replace far_uddgruppe=. if far_uddgruppe>80
replace mor_uddgruppe=. if mor_uddgruppe>80


egen par_perc=xtile(par_indkomst), by(cohort) nq(100)
egen perc=xtile(indkomst), by(cohort) nq(100)

*gen var for fixed effects estimations
capture drop par_dec
egen par_dec=xtile(par_indkomst),by(cohort) nq(10)


generate y_hat_perm=.
generate y_hat_flyt0=.
generate y_hat_flyt1=.

levelsof forstekom, local(komlist)

foreach lev of local komlist{
	capture noisily regress perc par_perc if forstekom==`lev' & counters==0
	capture noisily predict work
	capture noisily replace y_hat_perm=work if e(sample)
	capture noisily replace y_hat_flyt0=work if forstekom==`lev' & counters==1
	capture noisily replace y_hat_flyt1=work if sidstekom==`lev' & counters==1
	capture noisily drop work
}

capture drop pred_delta
generate pred_delta=y_hat_flyt1-y_hat_flyt0

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1, noconstant 
eststo model1


replace uddgruppe=. if uddgruppe>80


generate ufaglaert=0
generate faglaert=0
generate videregaande=0
generate par_videregaaende=0
replace ufaglaert=1 if inlist(uddgruppe, 10,15,20,25,35)
replace faglaert=1 if uddgruppe==30
replace videregaande=1 if inlist(uddgruppe, 40,45,50,55,60,70,75,80)
replace par_videregaaende=1 if inlist(par_hojest_udd, 40,45,50,55,60,70,75,80)

*Videregåemde
capture drop videregaande_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate videregaande_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.

levelsof forstekom, local(komlist)


forvalues y=1975(5)1985 {
	foreach lev of local komlist{
		capture noisily regress videregaande c.par_perc##c.par_perc if forstekom==`lev' & counters==0 & cohortpar==`y'
		capture noisily predict work
		capture noisily replace videregaande_frac_perm=work if e(sample)
		capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & counters==1 & cohortpar==`y'
		capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & counters==1 & cohortpar==`y'
		capture noisily drop work
	}
}

generate videregaande_frac=udd_hat_flyt1-udd_hat_flyt0
generate videregaande_0=udd_hat_flyt0

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1, noconstant 
eststo model2


esttab using "D:\FM\andlun\sensitivity_nicedta.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})


*_______________________ UDDD SPECIFIKATION____________________________________
eststo clear

clear
use "D:\FM\andlun\df_pred_udd.dta"
replace uddgruppe=. if uddgruppe>80

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1, noconstant 
eststo


clear
use "D:\FM\andlun\df_pred.dta"
replace uddgruppe=. if uddgruppe>80
generate videregaande=0
generate par_videregaaende=0
replace videregaande=1 if inlist(uddgruppe, 40,45,50,55,60,70,75,80)
replace par_videregaaende=1 if inlist(par_hojest_udd, 40,45,50,55,60,70,75,80)


*Videregåemde
capture drop videregaande_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate videregaande_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.


*
levelsof forstekom, local(komlist)
forvalues y=1975(5)1985 {
	foreach lev of local komlist{
		capture noisily regress videregaande c.par_perc if forstekom==`lev' & counters==0 & cohortpar==`y'
		capture noisily predict work
		capture noisily replace videregaande_frac_perm=work if e(sample)
		capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & counters==1 & cohortpar==`y'
		capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & counters==1 & cohortpar==`y'
		capture noisily drop work
	}
}

generate videregaande_frac=udd_hat_flyt1-udd_hat_flyt0
generate videregaande_0=udd_hat_flyt0

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1, noconstant 
eststo

clear
use "D:\FM\andlun\df_pred.dta"
replace uddgruppe=. if uddgruppe>80
generate videregaande=0
generate par_videregaaende=0
replace videregaande=1 if inlist(uddgruppe, 40,45,50,55,60,70,75,80)
replace par_videregaaende=1 if inlist(par_hojest_udd, 40,45,50,55,60,70,75,80)


*Videregåemde
capture drop videregaande_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate videregaande_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.


*
levelsof forstekom, local(komlist)

foreach lev of local komlist{
	capture noisily regress videregaande c.par_perc if forstekom==`lev' & counters==0
	capture noisily predict work
	capture noisily replace videregaande_frac_perm=work if e(sample)
	capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & counters==1 
	capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & counters==1
	capture noisily drop work
}

generate videregaande_frac=udd_hat_flyt1-udd_hat_flyt0
generate videregaande_0=udd_hat_flyt0

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1, noconstant 
eststo


esttab using "D:\FM\andlun\udd_specc_sensi.tex", label replace booktabs keep(c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})



*_______________ Analyse på landsdel ___________________


clear
use "D:\FM\andlun\df_perc.dta"
eststo clear

duplicates drop ID_nr, force

merge 1:1 ID_nr using "D:\FM\andlun\df_amt.dta"
*drop if _merge==2|_merge==1



*cohort inddeling
capture generate par_hojest_udd=max(far_uddgruppe, mor_uddgruppe)
replace far_uddgruppe=. if far_uddgruppe>80
replace mor_uddgruppe=. if mor_uddgruppe>80



generate y_hat_perm=.
generate y_hat_flyt0=.
generate y_hat_flyt1=.

levelsof forsteamt, local(komlist)

foreach lev of local komlist{
	capture noisily regress perc par_perc if forsteamt==`lev' & counters==0
	capture noisily predict work
	capture noisily replace y_hat_perm=work if e(sample)
	capture noisily replace y_hat_flyt0=work if forsteamt==`lev' & counters==1
	capture noisily replace y_hat_flyt1=work if sidsteamt==`lev' & counters==1
	capture noisily drop work
}

capture drop pred_delta
generate pred_delta=y_hat_flyt1-y_hat_flyt0
generate amt_flyt=0
replace amt_flyt=1 if forsteamt!=sidsteamt

correlate y_hat_flyt0 y_hat_flyt1
 
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & amt_flyt==1 & alderflyt!=28, noconstant 
eststo model1




replace uddgruppe=. if uddgruppe>80

generate videregaande=0
generate par_videregaaende=0
replace videregaande=1 if inlist(uddgruppe, 40,45,50,55,60,70,75,80)
replace par_videregaaende=1 if inlist(par_hojest_udd, 40,45,50,55,60,70,75,80)

*Videregåemde
capture drop videregaande_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1

generate videregaande_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.

levelsof forsteamt, local(komlist)


forvalues y=1975(5)1985 {
	foreach lev of local komlist{
		capture noisily regress videregaande c.par_perc##c.par_perc if forsteamt==`lev' & counters==0 & cohortpar==`y'
		capture noisily predict work
		capture noisily replace videregaande_frac_perm=work if e(sample)
		capture noisily replace udd_hat_flyt0=work if forsteamt==`lev' & counters==1 & cohortpar==`y'
		capture noisily replace udd_hat_flyt1=work if sidsteamt==`lev' & counters==1 & cohortpar==`y'
		capture noisily drop work
	}
}

generate videregaande_frac=udd_hat_flyt1-udd_hat_flyt0
generate videregaande_0=udd_hat_flyt0

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & amt_flyt==1, noconstant 
eststo model2


esttab using "D:\FM\andlun\sensitivity_landdele.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})

eststo clear

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1 & amt_flyt==1, noconstant 
eststo
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)


reg videregaande ibn.cohort i.cohort#c.videregaande_0 i.alderflyt i.alderflyt#i.par_hojest_udd c.videregaande_frac#i.alderflyt c.videregaande_frac#i.cohort i.KOEN par_perc if counters==1 & amt_flyt==1, noconstant
coefplot, vertical keep(*.alderflyt#c.videregaande_frac) xlab(2(1)30)

esttab using "D:\FM\andlun\coef_landsdel_flyt.csv", keep(*.alderflyt#c.pred_delta *.alderflyt#c.videregaande_frac) replace nostar

*_______________ Landsdele flyt på kommuner _______________________________
eststo clear
clear
use "D:\FM\andlun\df_pred_udd.dta"

duplicates drop ID_nr, force

merge 1:1 ID_nr using "D:\FM\andlun\df_amt.dta"


drop if _merge==2|_merge==1

capture drop amtsflyt
generate amtsflyt=.
replace amtsflyt=1 if forsteamt!=sidsteamt


reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1 & amtsflyt==1, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)

*indenfor landdel
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & amtsflyt==., noconstant 
eststo model1


*Across landsdel
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & amtsflyt==1, noconstant 
eststo model2


capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
*indenfor landdel
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & amtsflyt==., noconstant  
eststo model3
*Across landsdel
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & amtsflyt==1, noconstant 
eststo model4

esttab using "D:\FM\andlun\kom_landsdelflyt.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})



eststo clear
clear
use "D:\FM\andlun\df_pred_udd.dta"

ineqdeco indkomst if counters==0, by(forstekom)

*___________________________________________________________________________________________

*							TABELLER OG FIGURER
*___________________________________________________________________________________________

**Uddannelsesgrupperinger (tabeller)
tabulate far_uddgruppe counters if counters==1|counters==0, col nofreq
tabulate mor_uddgruppe counters if counters==1|counters==0, col nofreq
tabulate uddgruppe counters if counters==1|counters==0, col nofreq


**Eksempel på "Coefficienter/prediction uddannelse" XXXXXXXXXXX
binscatter videregaande par_perc if counters==0, line(qfit) nquantiles(100) 
binscatter videregaande par_perc if counters==0, line(qfit) nquantiles(100) reportreg
binscatter ufaglaert par_perc if counters==0, line(qfit) nquantiles(100)
binscatter ufaglaert par_perc if counters==0, line(qfit) nquantiles(100) reportreg
binscatter faglaert par_perc if counters==0, line(qfit) nquantiles(100)
binscatter faglaert par_perc if counters==0, line(qfit) nquantiles(100) reportreg

sort videregaande_frac

*Hist delta outcome 
drop if forstekom==411|sidstekom==411
sum pred_delta if counters==1, meanonly
local a=r(mean)
drop prcvideregaande_frac
generate prcvideregaande_frac=videregaande_frac*100
sum videregaande_frac if counters==1, meanonly
local b=r(mean)
hist pred_delta if counters==1, xline(`a', lcolor(dkgreen%100)) xline(`b', lcolor(cranberry%100)) color(blue%0) lcolor(eltblue%100) addplot(hist prcvideregaande_frac if counters==1, color(grey%20) lcolor(grey%0) )
***********DATA kort ID X VALUE******************* - 
{
*Kopires manuelt til EXCEL og laves så i R efterfølgende på hjemme PC
*1 Mean household rank for permanent resident
tabstat perc if counters==0, stats(mean n) col(stat) by(forstekom)

*2 Mean income ranks for children of Permanent Residents (A) Parents at the 25 perc (B) Parents at the 75th percentile
*(A)
tabstat perc if counters==0 & par_perc==25, stats(mean n) col(stat) by(forstekom)
*(B)
tabstat perc if counters==0 & par_perc==75, stats(mean n) col(stat) by(forstekom)

*3 Transition MAP Share of children with >80 own rank with <20 parent rank + Mobility map
capture drop above_80pct
generate above_80pct=0 if counters==0 & par_perc<=20 
replace above_80pct=1 if counters==0 & par_perc<=20 & perc>=80
tabulate forstekom above_80pct  if counters==0, row



*__________________Kort over uddannelse_______________________________
*Andel faglærte
*(A)
tabstat faglaert if counters==0 & par_perc==25, stats(mean n) col(stat) by(forstekom)
*(B)
tabstat faglaert if counters==0 & par_perc==75, stats(mean n) col(stat) by(forstekom)

*Andel videregaande
*(A)
tabstat videregaande if counters==0 & par_perc==25, stats(mean n) col(stat) by(forstekom)
*(B)
tabstat videregaande if counters==0 & par_perc==75, stats(mean n) col(stat) by(forstekom)

*Andel ufaglærte 
*(A)
tabstat ufaglaert if counters==0, stats(mean n)  col(stat) by(forstekom)


*3 Transition MAP Share of children with >80 own rank with <20 parent rank + Mobility map
capture drop kort_til_langudd
generate kort_til_langudd=0 if par_hojest_udd<30 & counters==0
replace kort_til_langudd=1 if counters==0 & par_hojest_udd<30 & videregaande==1
tabulate forstekom kort_til_langudd  if counters==0, row

}


clear
use "D:\FM\andlun\df_pred_udd.dta"


graph bar far_uddgruppe mor_uddgruppe if counters==1, over (mor_uddgruppe)

/* ________________________________________________________________*/

*  XXXXXXXXXXXXXXXXX PLACEBO TESTS XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
/* ________________________________________________________________*/

*Random flyt gemeration m. X %

clear
set seed 400
use "D:\FM\andlun\df_pred_udd.dta"

drop if (forstekom==825|forstekom==741|forstekom==563|forstekom==492|forstekom==411|sidstekom==825|sidstekom==741|sidstekom==563|sidstekom==492|sidstekom==411)

eststo clear

local share=0.4
levelsof forstekom, local(komlist)

capture generate x=runiform()

capture generate rd_moves=.
replace rd_moves=1 if counters==0 & x<=`share' 

local kom_lng : list sizeof local(komlist)

tokenize "`komlist'"

generate alt_kom_nr=floor(runiform(0,`kom_lng'))+1
generate alt_sidstekom=word("`komlist'",alt_kom_nr)
destring alt_sidstekom, replace
replace sidstekom=alt_sidstekom if counters==0 & rd_moves==1 
generate alt_alderflyt=floor(runiform(0,30))+1


*GENLAVE DELTA FOR DE FALSKE FLYTTERE

capture drop y_hat_perm
capture drop y_hat_flyt0
capture drop y_hat_flyt1

generate y_hat_perm=.
generate y_hat_flyt0=.
generate y_hat_flyt1=.

levelsof forstekom, local(komlist)

foreach lev of local komlist{
	capture noisily regress perc par_perc if forstekom==`lev' & counters==0
	capture noisily predict work
	capture noisily replace y_hat_perm=work if e(sample)
	capture noisily replace y_hat_flyt0=work if forstekom==`lev' & rd_moves==1
	capture noisily replace y_hat_flyt1=work if sidstekom==`lev' & rd_moves==1
	capture noisily drop work
}


*Tager roughly 20 min
*3) Find DELTA for hver flytter
capture drop pred_delta
generate pred_delta=y_hat_flyt1-y_hat_flyt0


capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alt_alderflyt
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alt_alderflyt  i.alt_alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if rd_moves==1, noconstant 
eststo model1




capture drop videregaande_frac_perm
capture drop udd_hat_flyt0
capture drop udd_hat_flyt1
capture drop videregaande_0
capture drop videregaande_frac

generate videregaande_frac_perm=.
generate udd_hat_flyt0=.
generate udd_hat_flyt1=.


levelsof forstekom, local(komlist)

forvalues y=1975(5)1985 {
	foreach lev of local komlist{
		capture noisily regress videregaande c.par_perc##c.par_perc if forstekom==`lev' & counters==0 & cohortpar==`y'
		capture noisily predict work
		capture noisily replace videregaande_frac_perm=work if e(sample)
		capture noisily replace udd_hat_flyt0=work if forstekom==`lev' & rd_moves==1 & cohortpar==`y'
		capture noisily replace udd_hat_flyt1=work if sidstekom==`lev' & rd_moves==1 & cohortpar==`y'
		capture noisily drop work
	}
}

generate videregaande_frac=udd_hat_flyt1-udd_hat_flyt0
generate videregaande_0=udd_hat_flyt0



capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alt_alderflyt

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alt_alderflyt  i.alt_alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if rd_moves==1 & counters==0, noconstant 
eststo model3

esttab using "D:\FM\andlun\placebo_rndflyt.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})



eststo clear

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alt_alderflyt i.alt_alderflyt#c.par_perc c.pred_delta#i.alt_alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if rd_moves==1 , noconstant 
eststo



reg videregaande ibn.cohort i.cohort#c.videregaande_0 i.alt_alderflyt i.alt_alderflyt#i.par_hojest_udd c.videregaande_frac#i.alt_alderflyt c.videregaande_frac#i.cohort i.KOEN par_perc if rd_moves==1, noconstant
eststo


esttab using "D:\FM\andlun\rndflyt_coef.csv", stats(N aic bic r2) keep(*.alt_alderflyt#c.pred_delta *.alt_alderflyt#c.videregaande_frac) replace nostar


*********************  !!!! Family fixed EFFECTS!!! ****************************
eststo clear
clear

use "D:\FM\andlun\df_pred_udd.dta"

destring familie_id, replace

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

capture drop fixgroup
egen fixgroup=group(familie_id)
xtset fixgroup
xtreg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1
eststo model1


capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

xtreg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 
eststo model3

esttab using "D:\FM\andlun\placebo_family_fe.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})




*********************  !!!! Time changing observables !!! ****************************
eststo clear
clear

use "D:\FM\andlun\df_pred_udd.dta"

duplicates drop ID_nr, force
drop ind_m0-ind_m2

merge 1:1 ID_nr using "D:\FM\andlun\rank_0.dta"



replace civ_m0="G" if civ_m0=="P"
replace civ_m0="F" if civ_m0=="O"
replace civ_m2="G" if civ_m2=="P"
replace civ_m2="F" if civ_m2=="O"
 
egen maristatus=concat(civ_m0 civ_m2)


tabulate maristatus
replace maristatus="" if maristatus=="E"|maristatus=="EF"|maristatus=="F"|maristatus=="FE"|maristatus=="FU"|maristatus=="G"|maristatus=="GU"|maristatus=="LL"|maristatus=="U"|maristatus=="UE"|maristatus=="UF"
tabulate maristatus if counters==1

encode maristatus, gen(maristatus_num)

generate rankchg=ind_m2-ind_m0

destring familie_id, replace

capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
*Normal/baseline
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model1

*+ Civ control
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd i.maristatus_num#i.alderflyt ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model2

*+ rank control
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd  c.rankchg#i.alderflyt ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model3

*rank chg + rank control
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd i.maristatus_num#i.alderflyt c.rankchg#i.alderflyt ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model4


*Full MITHL


capture drop fixgroup
egen fixgroup=group(familie_id)
xtset fixgroup
xtreg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd i.maristatus_num#i.alderflyt c.rankchg#i.alderflyt ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & maristatus_num!=. & rankchg!=.
eststo model5


esttab using "D:\FM\andlun\ind_varplacebo.tex", label replace booktabs keep(c.flyt*#c.pred_delta) ///
alignment(D{.}{.}{-1})

*_________________Udd ækvivalent____________________
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
eststo clear

*Normal/baseline
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model1

*+ Civ control
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN par_perc i.maristatus_num#i.alderflyt ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model2

*+ rank control
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN  par_perc  c.rankchg#i.alderflyt ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model3

*rank chg + rank control
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN par_perc i.maristatus_num#i.alderflyt c.rankchg#i.alderflyt ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & maristatus_num!=. & rankchg!=., noconstant 
eststo model4


*Full MITHL
capture drop fixgroup
egen fixgroup=group(familie_id)
xtset fixgroup
xtreg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt i.alderflyt#i.par_hojest_udd  i.KOEN par_perc i.maristatus_num#i.alderflyt c.rankchg#i.alderflyt ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1 & maristatus_num!=. & rankchg!=.
eststo model5


esttab using "D:\FM\andlun\udd_varplacebo.tex", label replace booktabs keep(c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})



*________________________ Place bo for flyt før fødsel ________________________________
use "D:\FM\andlun\df.dta"
duplicates drop ID_nr, force
rename counters cunt
save "D:\FM\andlun\df_clean.dta"


clear
eststo clear


use "D:\FM\andlun\df_pre.dta"

*

generate alderhjemmefra=hjemmefra_aar-cohort

* 1) Gen percentile (+ deciles rankings)
replace far_uddgruppe=. if far_uddgruppe>80
replace mor_uddgruppe=. if mor_uddgruppe>80
replace uddgruppe=. if uddgruppe>80

generate videregaande=0
generate par_videregaaende=0
replace videregaande=1 if inlist(uddgruppe, 40,45,50,55,60,70,75,80)



generate par_hojest_udd=max(far_uddgruppe, mor_uddgruppe)
egen par_indkomst=rmean(far_indkomst mor_indkomst)
capture drop cohortpar
generate cohortpar=recode(cohort,1975,1980,1985)
generate alderflytpar=recode(alderflyt,5,10,15,20,25,30)

replace par_videregaaende=1 if inlist(par_hojest_udd, 40,45,50,55,60,70,75,80)

*gen var for fixed effects estimations
capture drop par_dec
capture drop par_perc
capture drop perc
egen par_dec=xtile(par_indkomst),by(cohort) nq(10)
egen par_perc=xtile(par_indkomst), by(cohort) nq(100)
egen perc=xtile(indkomst), by(cohort) nq(100)

duplicates drop ID_nr, force

rename alderflyt aldflyt

merge 1:1 ID_nr using "D:\FM\andlun\df_clean.dta", keepusing(cunt alderflyt)

count if counters==1 & aldflyt>=0 & cunt==counters 
count if cunt==1 
count if counters==1 & aldflyt<=0

replace aldflyt=alderflyt if cunt==1
replace counters=1 if cunt==1

rename alderflyt clean_alderflyt
rename aldflyt alderflyt

* 2) Regress by CZ and cohort and find predicted/fitted income ranked conditional on cohort and CZ

capture drop y_hat_perm
capture drop y_hat_flyt0
capture drop y_hat_flyt1

generate y_hat_perm=.
generate y_hat_flyt0=.
generate y_hat_flyt1=.

/*
replace alderflyt=alderflyt-4
drop if alderflyt<=0
replace counters=0 if counters==1 & alderflyt<=0
*/


levelsof forstekom, local(komlist)
foreach lev of local komlist{
	capture noisily regress perc par_perc if forstekom==`lev' & counters==0
	capture noisily predict work
	capture noisily replace y_hat_perm=work if e(sample)
	capture noisily replace y_hat_flyt0=work if forstekom==`lev' & counters==1
	capture noisily replace y_hat_flyt1=work if sidstekom==`lev' & counters==1
	capture noisily drop work
}
capture drop pred_delta
generate pred_delta=y_hat_flyt1-y_hat_flyt0

*replace counters=1 if counters==0 & alderflyt<=0
replace alderflyt=alderflyt+4

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1, noconstant
eststo 

esttab using "D:\FM\andlun\place_flytfør_onlyperm.csv", stats(N aic bic r2) keep(*.alderflyt#c.pred_delta) replace nostar


/*
capture drop flyt*
mkspline flyt1 12 flyt2 27 flyt3 = alderflyt
*BETTER
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1, noconstant 
eststo model1

esttab using "D:\FM\andlun\udd_varplacebo.tex", label replace booktabs keep(c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})

*/








*_________________Semi param with splines and controls::::
eststo clear
clear
use "D:\FM\andlun\df_pred_udd.dta"
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt, marginal
*Normal/baseline
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt i.alderflyt#c.par_perc ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1, noconstant
eststo 

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt i.alderflyt#c.par_perc i.KOEN ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1, noconstant 
eststo 

reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt i.alderflyt#c.par_perc i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1, noconstant 
eststo 

*UDD
reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd  ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1, noconstant 
eststo

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1, noconstant 
eststo

reg videregaande ibn.cohort i.cohort#c.videregaande_0 c.videregaande_frac#i.cohort i.alderflyt  i.alderflyt#i.par_hojest_udd i.KOEN par_perc ///
c.flyt1#c.videregaande_frac /// * Denne linje specificerer kinket (første)
c.flyt2#c.videregaande_frac /// * Denne linje specificerer kinket (anden)
c.flyt3#c.videregaande_frac /// * Denne linje specificerer kinket (tredje)
c.videregaande_frac ///
if counters==1, noconstant 
eststo


esttab using "D:\FM\andlun\Parametric spline spec_marginal.tex", label replace booktabs keep(c.flyt*#c.pred_delta c.flyt*#c.videregaande_frac) ///
alignment(D{.}{.}{-1})






*_____________SENS TIL FORÆLDRES UDDANNELSES ________________
clear
eststo clear
use "D:\FM\andlun\df_pred_udd.dta"

correlate y_hat_flyt0 y_hat_flyt1

generate par_ufaglaert=0
generate par_faglaert=0
replace par_ufaglaert=1 if inlist(par_hojest_udd, 10,15,20,25,35)
replace par_faglaert=1 if par_hojest_udd==30


capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt


reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & par_videregaaende==1, noconstant 
eststo


reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1 & par_videregaaende==1, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)

esttab using "D:\FM\andlun\par_educlvl.tex", label replace booktabs keep(c.flyt*#c.pred_delta) ///
alignment(D{.}{.}{-1})



*______________________Histogram over deltaerne___________________________
clear
use "D:\FM\andlun\df_perc.dta"
eststo clear

duplicates drop ID_nr, force
merge 1:1 ID_nr using "D:\FM\andlun\df_amt.dta"
drop _merge

*cohort inddeling
capture generate par_hojest_udd=max(far_uddgruppe, mor_uddgruppe)
replace far_uddgruppe=. if far_uddgruppe>80
replace mor_uddgruppe=. if mor_uddgruppe>80



generate y_hat_perm=.
generate y_hat_flyt0=.
generate y_hat_flyt1=.

levelsof forsteamt, local(komlist)

foreach lev of local komlist{
	capture noisily regress perc par_perc if forsteamt==`lev' & counters==0
	capture noisily predict work
	capture noisily replace y_hat_perm=work if e(sample)
	capture noisily replace y_hat_flyt0=work if forsteamt==`lev' & counters==1
	capture noisily replace y_hat_flyt1=work if sidsteamt==`lev' & counters==1
	capture noisily drop work
}

capture drop pred_delta
generate pred_delta_amt=y_hat_flyt1-y_hat_flyt0

merge 1:1 ID_nr using "D:\FM\andlun\df_pred_udd_uniq.dta", keepusing(pred_delta) keep(match)
drop _merge
rename pred_delta pred_delta_kom

merge 1:1 ID_nr using "D:\FM\andlun\df_pred_sogn.dta", keepusing(pred_delta) keep(match)
rename pred_delta pred_delta_sogn


replace pred_delta_sogn=. if abs(pred_delta_sogn)>=20

twoway histogram pred_delta_sogn, color(black%20) lcolor(black%40) yscale(range(-20 20)) ylabel(#9) ytitle("Difference in predicted rank between destination and origin") bgcolor(white) graphregion(color(white)) horizontal name(g1,replace) 

twoway histogram pred_delta_kom, color(black%20) lcolor(black%40) yscale(range(-20 20)) ylabel(#9) ytitle(" ") bgcolor(white) graphregion(color(white)) horizontal name(g2,replace) 

twoway histogram pred_delta_amt, color(black%20) lcolor(black%40) yscale(range(-20 20)) ylabel(#9) ytitle(" ") bgcolor(white) graphregion(color(white)) horizontal name(g3,replace) 

gr combine g1 g2 g3, col(3) graphregion(color(white))



*_____________________ SD REGRESSION __________________
clear
use "D:\FM\andlun\df_pred_udd.dta"


egen poorperc=count(perc) if counters==0 & perc<=20, by(forstekom)

gen poor=0
replace poor=1 if perc<=20

tabstat poor if counters==0, stats(mean n) col(stat) by(forstekom)


egen komstd=sd(perc) if counters==0, by(forstekom)
histogram komstd
bysort sidstekom (counters): replace komstd=komstd[_n-1] if counters>0



reg perc ibn.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#c.par_perc c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1 & komstd>=28, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)


capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
*BETTER
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & komstd>=28, noconstant 










