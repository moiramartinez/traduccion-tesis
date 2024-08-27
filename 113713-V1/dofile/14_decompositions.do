clear all
cap log close
set linesize 120
set more off, perm
cap program drop _all
log using ${log}/14_decompositions.log, replace
adopath ++${prog}
set seed 1000

* This programme computes the basic decompositions for various subpopulations.
* The sample contains all workers in the dual-connected set.

/*----------- Normalise FEs and prepare group variables -------------*/

cap program drop decomp
program define decomp

use ${data}/worker_dual_`1'_`2', clear

lab def fem 0 "MALE" 1 "FEMALE", modify
lab val fem fem

fastxtile VAq=VA if VA!=., nq(100)
fastxtile REVq=REV if REV!=., nq(100)

* Outsourcing industries
gen food = inlist(w93_3, 553, 554, 555)
gen cleaning = inlist(w93_3, 747)
gen logistics = inlist(w93_3, 602, 631, 632, 634)
gen security = inlist(w93_3, 746)
gen temp = inlist(w93_3, 745)
gen outsrc = food + cleaning + logistics + security + temp


forval g=0/1 {

	di _newline(1)
	di "Normalisation: Estimated optimal cutoff value"
	di "---------------------------------------------------------"
	qui sum psi`1'`g' if VA<=${tau_VA_`1'_`2'}	
	scalar norm=r(mean)
	scalar N=r(N)
	di "`: label (fem)`g''"
	di "Mean firm effect for zero surplus firms (VA/L)                   : " r(mean)
	qui count if VA!=.	
	di "Share of person-years below threshold with non-missing VA/L      : " N/r(N)
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_nVA=psi`1'`g'-norm	

	di _newline(1)
	di "Normalisation: Bottom x% of value added per worker"
	di "---------------------------------------------------------"
	qui sum psi`1'`g' if VAq<=${tau_VAq_`1'}
	scalar norm=r(mean)
	di "`: label (fem)`g''"
	di "Mean firm effect for zero surplus firms (VAq)                   : " r(mean)
	qui count if VA!=.	
	di "Share of person-years below threshold with non-missing VAq      : " N/r(N)
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_nVAq=psi`1'`g'-norm

	di _newline(1)
	di "Normalisation: Bottom x% of revenue per worker"
	di "---------------------------------------------------------"
	qui sum psi`1'`g' if REVq<=${tau_REVq_`1'}
	scalar norm=r(mean)
	di "`: label (fem)`g''"
	di "Mean firm effect for zero surplus firms (REVq)                   : " r(mean)
	qui count if REV!=.	
	di "Share of person-years below threshold with non-missing REVq      : " N/r(N)
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_nREVq=psi`1'`g'-norm

	di _newline(1)
	di "Normalisation: Hospitality Industry"
	di "---------------------------------------------------------"
	qui sum psi`1'`g' if ind==${ind}
	scalar norm=r(mean)
	di "Mean firm effect associated w/ zero-surplus firms in hotelling industry : " r(mean)
	di "Share of person-years in hostpitality industry                          : " r(N)/_N
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_nind=psi`1'`g'-norm

	di _newline(1)
	di "Normalisation: Bottom 10% of firm effect distribution"
	di "---------------------------------------------------------"
	qui sum psi`1'`g', det
	scalar norm=r(p10)
	di "Mean firm effect associated w/ zero-surplus firms in hotelling industry : " r(mean)
	di "Share of person-years in hostpitality industry                          : " r(N)/_N
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_n10p=psi`1'`g'-norm

	di _newline(1)
	di "Normalisation: Outsourcing Industry"
	di "---------------------------------------------------------"
	qui sum psi`1'`g' if outsrc==1
	scalar norm=r(mean)
	di "Mean firm effect associated w/ zero-surplus firms in outsourcing industry : " r(mean)
	di "Share of person-years in outsourcing industry                          : " r(N)/_N
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_noutsource = psi`1'`g'-norm

	di _newline(1)
	di "Normalisation: only firms from VA-sample"
	di "---------------------------------------------------------"
	qui sum psi`1'`g' if VA<=${tau_VA_`1'_`2'}
	scalar norm=r(mean)
	scalar N=r(N)
	di "Mean firm effect for zero surplus firms (VA/L)                   : " r(mean)
	qui count if VA!=.	
	di "Share of person-years below threshold with non-missing VA/L      : " N/r(N)
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_nVA_nomis=psi`1'`g'-norm if VA!=.			// additionally restrict to non-missing VA
}

qui {

	* Additional grouping variables for decomposition

	* Age groups (14) for Figure
	gen ageg14=int((age+1)/3)-6

	* Age groups (3) for Table
	gen ageg3=.
	replace ageg3 = 1 if age<30
	replace ageg3 = 2 if age>=30 & age<45
	replace ageg3 = 3 if age>=45
	 
	* Education groups (3) for Table 
	gen educ3=1 if educ<=1
	replace educ3=2 if educ==2 | educ==3
	replace educ3=3 if educ==4 

}
	di "Distribution of education groups"
	ta educ educ3

qui{
	* Occupation 2-digit
	gen beruf2=int(beruf/10)
	egen mberuf2=mode(beruf2), by(persnr) maxmode
}

forval g=0/1{
	di "Distribution of mean (`: label (fem)`g'') firm effects (make bar chart):"
	table ind if fem==`g', c(mean psi`1'`g' freq)
}

* Firm size categories
gen fsizegr=1 if fsize<=5
replace fsizegr=2 if fsize>5 & fsize<=10
replace fsizegr=3 if fsize>10 & fsize<=20
replace fsizegr=4 if fsize>20 & fsize<=50
replace fsizegr=5 if fsize>50 & fsize<=100
replace fsizegr=6 if fsize>100 & fsize<=250
replace fsizegr=7 if fsize>250 & fsize<=500
replace fsizegr=8 if fsize>500 & fsize<=750
replace fsizegr=9 if fsize>750 & fsize<=1000
replace fsizegr=10 if fsize>1000
lab def fsizegr 1 "fsize<=5" 2 "fsize>5 & fsize<=10" 3 "fsize>10 & fsize<=20" 4 "fsize>20 & fsize<=50" 5 "fsize>50 & fsize<=100" 6 "fsize>100 & fsize<=250" ///
			7 "fsize>250 & fsize<=500" 8 "fsize>500 & fsize<=750" 9 "fsize>750 & fsize<=1000" 10 "fsize>1000"
lab val fsizegr fsizegr

table birthcohort, c(mean fsize freq) format(%9.4gc)


di _newline "For overall decomposition: period `1' `2'"
di 			"============================================"
gen pooled=1

* Define manufacturing industries
gen manuf=.
MANUFACTURING, newvar(manuf) oldvar(w93_3)
tab manuf fem, mis

if "`3'"=="REVq"{
	di "`3'"
	replace fe_`1'0_n`3'=. if VA>`5' & VA!=.
	replace fe_`1'1_n`3'=. if VA>`5' & VA!=.
}

else if ("`3'"=="VA" | "`3'"=="VAq") {
	di "`3'"
	replace fe_`1'0_n`3'=. if VA>`5' & VA!=.
	replace fe_`1'1_n`3'=. if VA>`5' & VA!=.
}

qui {
	ren ln_impwage lw
	qui sum year
	local yrmin=r(min)
	local yrmax=r(max)
	fastxtile tert_m=fe_`1'0_n`3', nq(3)
	fastxtile tert_f=fe_`1'1_n`3', nq(3)
	* skill groups based on wage distribution
	fastxtile tert_w_m=lw if fem==0, nq(3)
	fastxtile tert_w_f=lw if fem==1, nq(3)
	gen tert_w=tert_w_m if fem==0
	replace tert_w=tert_w_f if fem==1
	* skill groups based on person-effect
	fastxtile tert_pe_m=theta`1' if fem==0, nq(3)
	fastxtile tert_pe_f=theta`1' if fem==1, nq(3)
	gen tert_pe=tert_pe_m if fem==0
	replace tert_pe=tert_pe_f if fem==1
	drop tert_w_m tert_w_f tert_pe_m tert_pe_f
	tempvar countall countfirms countworkers
	gen countall=1
	bys betnr `4' fem: gen countfirms=_n==1
	bys persnr `4': gen countworkers=_n==1
	* Collapse data to group-gender cells
	collapse (mean) lw fe_`1'0_n`3' fe_`1'1_n`3' (sum) persyrs=countall firms=countfirms workers=countworkers, by(fem `4')
	sort `4' fem
	bys `4' (fem) : gen lwgap=lw[1]-lw[2]
	bys `4' (fem) : gen feM_males=fe_`1'0_n`3'[1]
	bys `4' (fem) : gen feM_females=fe_`1'0_n`3'[2]
	bys `4' (fem) : gen feF_males=fe_`1'1_n`3'[1]
	bys `4' (fem) : gen feF_females=fe_`1'1_n`3'[2]
	bys `4' (fem) : gen M_persyrs=persyrs[1]
	bys `4' (fem) : gen F_persyrs=persyrs[2]
	bys `4' (fem) : gen M_workers=workers[1]
	bys `4' (fem) : gen F_workers=workers[2]
	bys `4' (fem) : gen M_firms=firms[1]
	bys `4' (fem) : gen F_firms=firms[2]

	gen feffsgap = feM_males-feF_females
	gen sortm=feM_males-feM_females
	gen sortf=feF_males-feF_females
	gen bargm=feM_males-feF_males
	gen bargf=feM_females-feF_females

	bys `4' (fem) : keep if _n==1
	mkmat lwgap feM_males feF_females feffsgap sortm sortf bargm bargf *_persyrs *_workers *_firms, matrix(decomp)
}
* Display output: last three col's contain # of person-years, worker-years, and firms associated with each row.
matlist decomp,		format(%10.4gc)	///
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Decomposition based on `3' over `4', VA-trim: `5' (last 6 cols show # of male(M)/female(F) person-years, workers, and firms): `yrmin' to `yrmax')
end


* Default specification for decomposition

global ind   			"8"
global tau_VA_1_all		"3.25"
global tau_VA_2_all		"3.10"
global tau_VAq_1		"6"
global tau_VAq_2		"6"
global tau_REVq_1		"6"
global tau_REVq_2		"6"

*prepdecomp 1 all
*prepdecomp 2 all


* Loop to run programme for different groups and for different threshold values.

foreach j in 1 2 {					// iterates over period 1 and 2

	decomp `j' all VA pooled 10
	decomp `j' all VA educ3 10
	decomp `j' all VA ageg3 10
	decomp `j' all VA tert_m 10		// by tertile of male firm-effects > robustness to see if bargaining is positive in higher ranges of firm-surplus
	decomp `j' all VA tert_f 10		// by tertile of female firm-effects > robustness to see if bargaining is positive in higher ranges of firm-surplus
	decomp `j' all VA tert_w 10		// by tertile of wage distribution
	decomp `j' all VA tert_pe 10
	decomp `j' all VA manuf 10
	decomp `j' all VA ageg14 10
	*decomp `j' all VA "educ3 ageg14" 10
	decomp `j' all VA ind 10
	decomp `j' all VA moccup 10
	*decomp `j' all VA w93_3 10
	*decomp `j' all VA mberuf2 10
	decomp `j' all VA fsizegr 10
	decomp `j' all VA birthcohort 10
}



foreach j in 1 2 {					// iterates over period 1 and 2

	*decomp `j' all VA pooled 5
	*decomp `j' all VA educ3 5
	*decomp `j' all VA ageg3 5
	*decomp `j' all VA tert_m 5		// by tertile of male firm-effects > robustness to see if bargaining is positive in higher ranges of firm-surplus
	*decomp `j' all VA tert_f 5		// by tertile of female firm-effects > robustness to see if bargaining is positive in higher ranges of firm-surplus
	*decomp `j' all VA tert_w 5		// by tertile of wage distribution
	*decomp `j' all VA manuf 5
	*decomp `j' all VA ageg14 5
	*decomp `j' all VA "educ3 ageg14" 5
	*decomp `j' all VA ind 5
	*decomp `j' all VA moccup 5
	*decomp `j' all VA w93_3 5
	*decomp `j' all VA mberuf2 5
	*decomp `j' all VA fsizegr 5
	*decomp `j' all VA birthcohort 5
}

foreach j in 1 2 {
	decomp `j' all REVq pooled 10
	decomp `j' all ind pooled
	decomp `j' all outsource pooled
}

foreach j in 1 2 {
	*decomp `j' all outsource educ3
	*decomp `j' all outsource ageg3
}




/*----------- Adjusted gender gaps by industry -------------*/
/*
cap program drop adjgap
program define adjgap
use ${data}/tmpdecomp`1'`3', clear
di as res "Build experience variable for regression..."
gen exper=0
replace exper = age - 16 if educ==0 | educ==1
replace exper = age - 19 if educ==2
replace exper = age - 20 if educ==3
replace exper = age - 25 if educ==4
replace exper = 0 if exper < 0
gen exper2 = exper*exper/100
gen exper3 = exper*exper*exper/1000
gen male=1-fem

global X1 "i.educ exper exper2 exper3"
qui levelsof ind, l(industries)
qui reg ln_impwage male $X1 i.year
di "Adjusted GWG (across industries)	: " _b[male]

di _newline "Fitting OLS model for adjusted wages by industry..."
di _newline "Industry                    "	"adj gwg"
foreach j in `industries' {
	qui reg ln_impwage male $X1 i.year if ind==`j'
	di "`j' `: label (ind) `j'' 			: " _b[male]
}
end

adjgap 1 1 all
adjgap 2 1 all
*adjgap 1 5
*adjgap 2 5
*adjgap 1 10
*adjgap 2 10
*/


cap erase ${data}/tmpdecomp1all.dta
cap erase ${data}/tmpdecomp2all.dta

cap log close

*exit








/***************** EVEN FURTHER DECOMPOSITIONS *******************\


* alternatives using different cutoffs for percentiles

foreach q in 6 { //6 10

global tau_VAq_1 		"`q'"
global tau_REVq_1 		"`q'"
global tau_QRq_1 		"`q'"
global tau_VAq_2 		"`q'"
global tau_REVq_2 		"`q'"
global tau_QRq_2 		"`q'"

prepdecomp 1 all
prepdecomp 2 all

foreach j in 1 2 {
*decomp `j' VAq pooled all 10
*decomp `j' VAq educ3 all 10
*decomp `j' VAq ageg3 all 10
*decomp `j' VAq ageg14 all 10
*decomp `j' VAq ind all 10
}
}


decomp 1 VA pooled all 5
decomp 1 VA educ3 all 5
decomp 1 VA ageg3 all 5
decomp 1 VA ageg14 all 5
decomp 1 VA ind all 5
decomp 1 VA moccup all 5

decomp 2 VA pooled all 5
decomp 2 VA educ3 all 5
decomp 2 VA ageg3 all 5
decomp 2 VA ageg14 all 5
decomp 2 VA ind all 5
decomp 2 VA moccup all 5


*/

/*
foreach j in 1 2 {
decomp `j' VA pooled nopadmin
decomp `j' VA educ3 nopadmin
decomp `j' VA ageg3 nopadmin
decomp `j' VA moccup nopadmin
decomp `j' VA ind  nopadmin
}

foreach j in 1 2 {
decomp `j' ind pooled nopadmin
decomp `j' ind educ3 nopadmin
decomp `j' ind ageg3 nopadmin
decomp `j' ind moccup nopadmin
decomp `j' ind ind nopadmin
}
*/

/*
foreach j in 1 2 {
decomp `j' VA pooled 5
*decomp `j' VA educ 5
*decomp `j' VA ageg5 5
decomp `j' VA educ3 5
decomp `j' VA ageg3 5
*decomp `j' VA ageg14 5
decomp `j' VA moccup 5
decomp `j' VA ind 5
}
foreach j in 1 2 {
decomp `j' VA pooled 10
*decomp `j' VA educ 10
*decomp `j' VA ageg5 10
decomp `j' VA educ3 10
decomp `j' VA ageg3 10
*decomp `j' VA ageg14 10
decomp `j' VA moccup 10
decomp `j' VA ind 10
}
*/

