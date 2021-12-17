clear

use "D:\FM\andlun\df_sogn.dta"


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

save "D:\FM\andlun\df_perc_sogn.dta", replace



* 2) Regress by CZ and cohort and find predicted/fitted income ranked conditional on cohort and CZ
clear
use "D:\FM\andlun\df_perc_sogn.dta"
count if counters==1

*Lav histogram over Størrelse på sogn  - DROP SOGN MED UNDER 50 permanent residents
capture drop sognecount
egen sognecount=count(forstesogn) if counters==0, by(forstesogn)

*hist sognecount if counters==0, frequency width(50)
*XXXXXXXXXXXXXXXXXXXXXXXX DROP SOGN MED UNDER 50 permanent residents XXXXXXXXXXXXXXXXXXXXXXXXXX
levelsof forstesogn if sognecount<=200, local(sognlist)

foreach lev of local sognlist{
	drop if forstesogn==`lev'|sidstesogn==`lev'
}
count if counters==0

*count if counters==1
distinct forstesogn


capture drop y_hat_perm
capture drop y_hat_flyt0
capture drop y_hat_flyt1

generate y_hat_perm=.
generate y_hat_flyt0=.
generate y_hat_flyt1=.

levelsof forstesogn, local(sognlist)

foreach lev of local sognlist{
	capture noisily regress perc par_perc if forstesogn==`lev' & counters==0
	capture noisily predict work
	capture noisily replace y_hat_perm=work if e(sample)
	capture noisily replace y_hat_flyt0=work if forstesogn==`lev' & counters==1
	capture noisily replace y_hat_flyt1=work if sidstesogn==`lev' & counters==1
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


save "D:\FM\andlun\df_pred_sogn.dta", replace

clear


* _____________________ 4) Opstil regression (baseline semi-parametric) __________________________
clear
use "D:\FM\andlun\df_pred_sogn.dta"


	
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt
*BETTER
reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & abs(pred_delta)<3, noconstant 


count if missing(pred_delta) & counters==1
count if counters==1



*________________________________________________________________________________________________

*_________________________ 5) Baseline parametric estimations ___________________________________
clear
use "D:\FM\andlun\df_pred_sogn.dta"

*Chetty måden:
forval c= ``var'_cohort_min'(1)

regress perc ibn.cohort ibn.cohort#c.y_hat_flyt0 ibn.alderflyt ibn.alderflyt#c.par_perc  c.pred_delta#ibn.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1 & abs(pred_delta)>=3, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)
eststo model3


esttab using "D:\FM\andlun\indkomst coef_sogn.csv", stats(N aic bic r2) keep(*.alderflyt#c.pred_delta) replace nostar
	
eststo clear

mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

reg perc ibn.cohort ibn.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort ibn.alderflyt  ibn.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & pred_delta<=0, noconstant
eststo

esttab using "D:\FM\andlun\2splines_sogn.csv", stats(N aic bic r2) keep(c.flyt*#c.pred_delta) replace nostar
	

/*___________________________________________________________________________________________ */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   UDDANNELSES AFSNIT XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
clear
use "D:\FM\andlun\df_pred_sogn.dta"
replace uddgruppe=. if uddgruppe>80


generate ufaglaert=0
generate faglaert=0
generate videregaande=0
generate par_videregaaende=0
replace ufaglaert=1 if inlist(uddgruppe, 10,15,20,25,35)
replace faglaert=1 if uddgruppe==30
replace videregaande=1 if inlist(uddgruppe, 40,45,50,55,60,70,75,80)
replace par_videregaaende=1 if inlist(par_hojest_udd, 40,45,50,55,60,70,75,80)


/*___________________________________________________________________________________________ */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Lange flyt XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
clear
use "D:\FM\andlun\df_pred_sogn.dta"

merge 1:1 ID_nr using "D:\FM\andlun\df_amt.dta"


drop if _merge==2|_merge==1

capture drop amtsflyt
generate amtsflyt=.
replace amtsflyt=1 if forsteamt!=sidsteamt

capture drop komflyt
generate komflyt=.
replace komflyt=1 if forstekom!=sidstekom

eststo clear
reg perc i.cohort i.cohort#c.y_hat_flyt0 i.alderflyt i.alderflyt#par_dec c.pred_delta#i.alderflyt c.pred_delta#i.cohort i.KOEN i.par_hojest_udd if counters==1 & amtsflyt==1, noconstant
coefplot, vertical keep(*.alderflyt#c.pred_delta) xlab(2(1)30)
eststo model

esttab using "D:\FM\andlun\coef_sogn_lang.csv", stats(N aic bic r2) keep(*.alderflyt#c.pred_delta) replace nostar
	
/*___________________________________________________________________________________________ */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Kinky boiz :)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
capture drop flyt*
mkspline flyt1 9 flyt2 24 flyt3 = alderflyt

eststo clear

eststo model_`m': reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
pred_delta ///
if counters==1 & amtsflyt==1, noconstant
eststo model

esttab using "D:\FM\andlun\2kink_sogn_lang.csv", stats(N aic bic r2) keep(c.flyt*#c.pred_delta) replace nostar
	

*___________________________________________________________________________________________

*							TABELLER OG FIGURER
*___________________________________________________________________________________________
*LAV HIST FOR SOGNE
sum pred_delta if counters==1, meanonly
local a=r(mean)
sum pred_delta if counters==1 & amtsflyt==1, meanonly
local b=r(mean)
hist pred_delta if counters==1, xline(`a', lcolor(dkgreen%100)) xline(`b', lcolor(cranberry%100)) color(blue%0) lcolor(eltblue%100) addplot(hist pred_delta if counters==1 & amtsflyt==1, color(grey%20) lcolor(black%10) )


*LAV FIGUR OVER MOVERS VS: TOTAL HER
tabulate cohort if counters==1
tabulate cohort

*Lav histogram over Størrelse på sogn

clear

use "D:\FM\andlun\df_pred_sogn.dta"

gen poor=0
replace poor=1 if perc<=20

tabstat poor if counters==0, stats(mean n) col(stat) by(forstesogn)



capture drop sognecount
egen komstd=sd(perc) if counters==0, by(forstesogn)

histogram komstd

sum komstd if counters==0



capture drop sognecount
egen sognecount=count(forstesogn) if counters==0, by(forstesogn)
sort sognecount

hist sognecount if counters==0, width(50) frequency color(grey%20) lcolor(grey%0) xline(200, lcolor(cranberry%100))
count if  sognecount<=200 & counters==0

clear
/*______________________________________________________________________________________________ */	
*******XXXXXXXXXXXXX SIMPLE FIGURER TIL DESKRIPTIV STAT OSV XXXXXXXXXXXXXXXXXX *******************
{
set scheme s2color



}
***********DATA kort ID X VALUE******************* - 
{
*Kopires manuelt til EXCEL og laves så i R efterfølgende på hjemme PC
*1 Mean household rank for permanent resident
tabstat perc if counters==0, stats(mean n) col(stat) by(forstesogn)

*__________________Kort over uddannelse_______________________________
*Andel videregaande
tabstat videregaande if counters==0, stats(mean n) col(stat) by(forstesogn)

}










/* SKRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALD*/


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
esttab model_* using "D:\FM\andlun\1kink_ind_sogn.csv", stats(N aic bic r2) keep(pred_delta 0.flytfør_X#c.pred_delta *.flytfør_X#c.pred_delta#c.alderflyt) replace nostar



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
esttab model_* using "D:\FM\andlun\kink2_ind_sogn.csv", stats(N aic bic r2) keep(pred_delta *.flytfør_Y#c.pred_delta *.flytfør_Y#c.pred_delta#c.alderflyt) replace nostar



/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Spliny dring boiz :)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
	
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



esttab model_* using "D:\FM\andlun\1knot_ind_sogn.csv", stats(N aic bic r2) keep(pred_delta c.flyt*#c.pred_delta) replace nostar


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


esttab model_* using "D:\FM\andlun\2knot_ind_sogn.csv", stats(N aic bic r2) keep(pred_delta c.flyt*#c.pred_delta) replace nostar



/*Flytninger der er lange*/


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
	if counters==1 & amtsflyt==1, noconstant
	local m `++m'
}
esttab model_* using "D:\FM\andlun\1kink_ind_sogn_lang.csv", stats(N aic bic r2) keep(pred_delta 0.flytfør_X#c.pred_delta *.flytfør_X#c.pred_delta#c.alderflyt) replace nostar



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
		if counters==1 & amtsflyt==1, noconstant
		local m `++m'
	}
}
esttab model_* using "D:\FM\andlun\kink2_ind_sogn_lang.csv", stats(N aic bic r2) keep(pred_delta *.flytfør_Y#c.pred_delta *.flytfør_Y#c.pred_delta#c.alderflyt) replace nostar



/*XXXXXXXXXXXXXXXXXXXXXXXXXXXX   Spliny dring boiz :)  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*___________________________________________________________________________________________ */
	
est clear
local m=1
forvalues x=5(1)26{
		capture drop flyt*
		mkspline flyt1 `x' flyt2 = alderflyt		
		eststo model_`m': reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
		c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
		c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
		pred_delta ///
		if counters==1 & amtsflyt==1, noconstant
		local m `++m'
}



esttab model_* using "D:\FM\andlun\1knot_ind_sogn_lang.csv", stats(N aic bic r2) keep(pred_delta c.flyt*#c.pred_delta) replace nostar


 *Simpel (2 kink)

est clear
local m=1
forvalues x=5(1)16{
	forvalues y=20(1)26 {
		capture drop flyt*
		mkspline flyt1 `x' flyt2 `y' flyt3 = alderflyt
		capture drop flyttidlig
		capture generate flyttidlig=0
		replace flyttidlig=1 if alderflyt<=`x' & counters==1 & amtsflyt==1
		
		eststo model_`m': reg perc ibn.cohort i.cohort#c.y_hat_flyt0 c.pred_delta#i.cohort i.alderflyt  i.alderflyt#c.par_perc  i.KOEN i.par_hojest_udd ///
		c.flyt1#c.pred_delta /// * Denne linje specificerer kinket (første)
		c.flyt2#c.pred_delta /// * Denne linje specificerer kinket (anden)
		c.flyt3#c.pred_delta /// * Denne linje specificerer kinket (tredje)
		pred_delta ///
		if counters==1 & amtsflyt==1, noconstant
		local m `++m'
	}
}


esttab model_* using "D:\FM\andlun\2knot_ind_sogn_lang.csv", stats(N aic bic r2) keep(pred_delta c.flyt*#c.pred_delta) replace nostar


clear
use "D:\FM\andlun\df_pred_sogn.dta"

capture noisily statsby n=e(N) _b,  by(forstesogn): regress perc par_perc if counters==0
sort _b_par_perc
save "D:\FM\andlun\kom_output.dta", replace