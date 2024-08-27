* Estimate AKMs for industries with presumably low union coverage, run the gridsearch algorithm 
clear all
cap log close
set linesize 120
set more off, perm
log using ${log}/20_akms_lowhigh.log, replace
adopath ++${prog}
set seed 1000


* List of 2-digit industries with low union coverage
* The selection of industries is based on the "Tarifbindung Jahresbericht 2010", Table 2.2.2, where
* I select all industriese with above median shares of union NON-coverage. Then I use a x-walk to
* the 1993 industry classification.


*global low = "1,15,18,19,22,24,25,28,29,30,31,33,34,36,51,52,55,62,63,67,70,71,72,74,85,90,91"
global low = "14,15,17,18,19,20,21,25,28,30,29,36,35,37,45,50,51,52,60,55,22,72,64,67,70,74,73,85,71,63,92,93"
global high = "14,15,17,18,19,20,21,25,28,30,29,36,35,37,45,50,51,52,60,55,22,72,64,67,70,74,73,85,71,63,92,93"


foreach sub in low high { // low high

	capture confirm f ${data}/AKM_`sub'unions.dta
	capture confirm f ${data}/AKM_`sub'unions.dta
	
	if _rc>0 {
	
		forval j=1/2{

			forval g=0/1{

			* Load analysis file
				use persnr betnr year age fem educ ln_impwage w93_3 ind using ${data}/AKM_select_`j', clear
				
			* Restrict to gender
				keep if fem==`g'
				
			* Restrict to industries with low/high union coverage
				gen sic2=int(w93_3/10)
				if "`sub'"=="low" keep if inlist(sic2,${`sub'})
				if "`sub'"=="high" drop if inlist(sic2,${`sub'})
				
			* Find largest connected set in pooled sample
				qui {
					egen id=group(persnr)
					egen firmid=group(betnr)

					di "Run adapted grouping algorithm... Require at least 0.99 of person-years to be allocated (possibly to multiple groups)."
					grouping2 group, i(id) j(firmid) lowbound(0.99)

					bys group : gen gs=_N
					sum gs
					mat coverage=(r(max)\r(N)\ 100*r(max)/r(N))
				}
				matlist coverage, tit(Largest connected set)

			* Restrict to largest mobility group
				sum gs
				replace group=(gs==r(max))
				keep if group==1
				cap drop firmid id gs

			* Make covariates as in AKMs by gender
				replace age=(age-40)/40
				gen age2=age*age
				gen age3=age*age*age

				sum
				bys fem : sum

				char year[omit] 1
				xi i.educ*i.year i.educ|age2 i.educ|age3

			* Estimate full AKM
				a2reg ln_impwage _Ieduc* _Iyear* _IeduXyea* _IeduXage* age2 age3, individual(persnr) unit(betnr) indeffect(theta) uniteffect(psi) xb(xb) resid(r)
				sum theta psi xb r
				bys fem : sum theta psi xb r

			* Variance Decomposition
				mat accum sumdev= ln_impwage theta psi xb r , noc dev
				mat cov = sumdev/(r(N)-1)
				mat corr = corr(sumdev)
				matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore		

			* Save
				keep betnr psi
				ren psi psi`j'`g'
				bys betnr : keep if _n==1
				
				compress
				if (`g'==0 & `j'==1) {
					save ${data}/AKM_`sub'unions, replace
				}
				if (`g'==1 | `j'==2) {
					cap drop _merge
					merge 1:1 betnr using ${data}/AKM_`sub'unions, nogen
					bys betnr: keep if _n==1
					save ${data}/AKM_`sub'unions, replace
				}
			}
		}

	}
}

/*-------------------- FIT SUR MODELS FOR DIFFERENT CUTOFFS (GRIDSEARCH) ---------------------*/

cap program drop gridsearch
program define gridsearch

clear matrix
mat rss=J(`4',3,.)
mat const=J(`4',2,.)
mat beta=J(`4',2,.)
mat tau=J(`4',1,.)

use ${data}/AKM_select_`1' , clear
qui sum year
local yrmin=r(min)
local yrmax=r(max)

* Import AKM effects
merge m:1 betnr using ${data}/AKM_`8'unions, keep(1 3) keepus(psi`1'0 psi`1'1)

* Restrict to dual-connected set [eliminates firms not in estimation set, i.e., in other industries]
keep if (psi`1'0!=. & psi`1'1!=.)

* Restrict sample
cap drop sic2
gen sic2=int(w93_3/10)

if "`8'"=="low" keep if inlist(sic2,${`8'})
if "`8'"=="high" drop if inlist(sic2,${`8'})


di _newline "Collapse data to firm-level"
gen countall=1

collapse (mean) psi`1'0 psi`1'1 `2' (sum) persyrs=countall, by(betnr)
qui count
di "# of establishments                     : " r(N)
qui count if `2'!=.
di "# of establishments w/non-missing `2'   : " r(N)
qui sum persyrs [aw=persyrs]
di "# of person-years in dual-connected set : " r(sum_w)
qui sum persyrs [aw=persyrs] if `2'!=.
di "# of person-years w/ non-missing `2'    : " r(sum_w)
di "Drop firms w/ missing `2' in all periods."
drop if `2'==.

drop if `2'>`7'

di _newline "Start iterating over cutoff vals (# of cutpoints: `4'):"
forval cutpoint =1/`4' {
local cut=`3'+`5'*(`cutpoint'-1)
di "Cutoff # `cutpoint' --> Threshold = `cut'"
qui {
di _newline "Define net surplus based on `2'"
gen NS=max(0, `2'-`cut') if `2'!=.

di _newline "Fit SUR, weighted by total persyrs per firm"
sureg (psi`1'0 NS) (psi`1'1 NS) [aw=persyrs]
*sureg (fe_m NS) (fe_f NS) [aw=persyrs]
mat b=e(b)
mat beta[`cutpoint',1]=b[1,1]
mat beta[`cutpoint',2]=b[1,3]
mat const[`cutpoint',1]=b[1,2]
mat const[`cutpoint',2]=b[1,4]
mat rss[`cutpoint',1]=e(rss_1)
mat rss[`cutpoint',2]=e(rss_2)
mat rss[`cutpoint',3]=e(rss_1)+e(rss_2)
mat tau[`cutpoint',1]=`cut'
cap drop NS
}
}
qui {
mat coln beta=Men Women
matlist beta,	format(%10.4gc)	///
				lines(oneline)	///
				tw(5)			///
				showcoleq(c)	///
				linesize(120)	///
				underscore		///
				tit(Rent-sharing coefficients based on `2': `yrmin' to `yrmax')
mat coln const=Men Women
matlist const,	format(%10.4gc)	///
				lines(oneline)	///
				tw(5)			///
				showcoleq(c)	///
				linesize(120)	///
				underscore		///
				tit(Constant terms (normalisation) based on `2': `yrmin' to `yrmax')
mat coln rss=Men Women Total
matlist rss,	format(%10.4gc)	///
				lines(oneline)	///
				tw(5)			///
				showcoleq(c)	///
				linesize(120)	///
				underscore		///
				tit(System residual sum of squares based on `2': `yrmin' to `yrmax')
mat results=(rss, beta, const, tau)
di _newline "Find tau that minimizes the rss of system"
mata: results=st_matrix("results")
mata: findmin=colmin(results[1..`4',3])
mata: subres=select(results, results[.,3]:==findmin)
mata: st_matrix("subres",subres)
}
matlist subres, format(%10.4gc)	///
				lines(oneline)	///
				tw(5)			///
				showcoleq(c)	///
				linesize(120)	///
				underscore		///
				tit(Tau associated with minimum residual sum of squares based on `2': `yrmin' to `yrmax')

global tau_`2'_`1'_`6'`8'=subres[1,8]

di _newline(2) "Sample-period: `1', productivity measure: `2', stepsize: `5', upper cutoff: `7', `8'"
di "Store optimal value in global: " ${tau_`2'_`1'_`6'`8'}

end

foreach sub in low high { // low

	gridsearch 1 VA 2 30 0.05 all 5 "`sub'"
	gridsearch 2 VA 2 30 0.05 all 5 "`sub'"

}




/*----------- Normalise FEs and prepare group variables -------------*/

cap program drop decomp
program define decomp

use ${data}/AKM_select_`1' , clear
qui sum year
local yrmin=r(min)
local yrmax=r(max)

* Import AKM effects
merge m:1 betnr using ${data}/AKM_`6'unions, keep(1 3) keepus(psi`1'0 psi`1'1)

* Restrict to dual-connected set
keep if (psi`1'0!=. & psi`1'1!=.)

* Restrict sample
cap drop sic2
gen sic2=int(w93_3/10)

if "`6'"=="low" keep if inlist(sic2,${`6'})
if "`6'"=="high" drop if inlist(sic2,${`6'})

forval g=0/1 {

	di _newline(1)
	di "Normalisation: Estimated optimal cutoff value"
	di "---------------------------------------------------------"
	qui sum psi`1'`g' if VA<=${tau_VA_`1'_`2'`6'}	
	scalar norm=r(mean)
	scalar N=r(N)
	di "`: label (fem)`g''"
	di "Mean firm effect for zero surplus firms (VA/L)                   : " r(mean)
	qui count if VA!=.	
	di "Share of person-years below threshold with non-missing VA/L      : " N/r(N)
	qui count if psi`1'`g'<=norm
	di "Share of person-years with psi`1'`g' < " norm	" : " r(N)/_N
	gen fe_`1'`g'_nVA=psi`1'`g'-norm	
}
	
* Education groups (3) for Table 
	gen educ3=1 if educ<=1
	replace educ3=2 if educ==2
	replace educ3=3 if educ==3 | educ==4 
	ta educ educ3

	
	forval g=0/1{
		di "Distribution of mean (`: label (fem)`g'') firm effects (make bar chart):"
		table ind if fem==`g', c(mean psi`1'`g' freq)
	}

	di _newline "For overall decomposition: period `1' `2'"
	di 			"============================================"
	gen pooled=1

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
	drop tert_w_f tert_w_m
	tempvar countall countfirms countworkers
	gen countall=1
	bys betnr `4' fem: gen countfirms=_n==1
	bys persnr `4': gen countworkers=_n==1
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
matlist decomp,		format(%10.4gc)	///
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Decomposition based on `3' over `4' (last 6 cols show # of male(M)/female(F) person-years, workers, and firms): `yrmin' to `yrmax')
end


* Default specification for decomposition

/*
global ind   			"8"
global tau_VA_1_all		"3.2"
global tau_VA_2_all		"3.1"
global tau_VAq_1		"6"
global tau_VAq_2		"6"
global tau_REVq_1		"6"
global tau_REVq_2		"6"
*/
*prepdecomp 1 all
*prepdecomp 2 all

foreach sub in low high { // low

	foreach j in 1 2 {					// iterates over period 1 and 2
	
		decomp `j' all VA pooled 10 "`sub'"
		decomp `j' all VA educ3 10 "`sub'"
		decomp `j' all VA tert_w 10 "`sub'"
		
		*decomp `j' all VA pooled 5 "`sub'"
		*decomp `j' all VA educ3 5 "`sub'"
		*decomp `j' all VA tert_w 5 "`sub'"
	}
}

cap erase ${data}/AKM_highunions.dta
cap erase ${data}/AKM_lowunions.dta
