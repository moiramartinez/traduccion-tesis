clear all
cap log close
set linesize 120
set more off, perm
cap program drop _all
log using ${log}/13_gridsearch.log, replace
adopath ++${prog}
set seed 1000


* This program performs a grid-search for the optimal normalisation of firm effects;
* The loop iterates over discrete values and estimates a SUR model using the implied
* parameters. Summary statistics are displayed in tables. Estimations are 
* performed at the firm-level (1 obs = 1 firm) and weighted by total observed # of
* person-years



* 2SLS regression of female firm effects on male firm effects instrumented using mean log value added

forval j=1/2 {

	use ${data}/worker_dual_`j'_all, clear
	keep if VA!=.
	su VA
	
	di _newline "person-year level"
	ivregress 2sls psi`j'1 (psi`j'0=VA), ro cluster(idnum)
	di _newline "person-year level, VA<=5"
	ivregress 2sls psi`j'1 (psi`j'0=VA) if VA<=5, ro cluster(idnum)
	
	collapse (firstnm) VA psi`j'1 psi`j'0 (count) npy=persnr, by(idnum) fast
	
	di _newline "firm level, unweighted"
	ivregress 2sls psi`j'1 (psi`j'0=VA), ro
	di _newline "firm level, unweighted, VA<=5"
	ivregress 2sls psi`j'1 (psi`j'0=VA) if VA<=5, ro
	di _newline "firm level, npy-weighted"
	ivregress 2sls psi`j'1 (psi`j'0=VA) [aw=npy], ro
	di _newline "firm level, npy-weighted, VA<=5"
	ivregress 2sls psi`j'1 (psi`j'0=VA) [aw=npy] if VA<=5, ro
	
}




/*-------------------- FIT SUR MODELS FOR DIFFERENT CUTOFFS (GRIDSEARCH) ---------------------*/

cap program drop gridsearch
program define gridsearch

clear matrix
* Empty container for parammeter estimates
mat rss=J(`4',3,.)
mat const=J(`4',2,.)
mat beta=J(`4',2,.)
mat tau=J(`4',1,.)

* Load...
use ${data}/worker_dual_`1'_`6' , clear
qui sum year
local yrmin=r(min)
local yrmax=r(max)

* Import union status from LIAB firm data
merge m:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(tarif hrf_quer) nogen
gen union=(tarif==1|tarif==2)

* Skill groups based on wage distribution
fastxtile tert_w_m=ln_impwage if fem==0, nq(3)
fastxtile tert_w_f=ln_impwage if fem==1, nq(3)
gen tert_w=tert_w_m if fem==0
replace tert_w=tert_w_f if fem==1
* Skill groups based on person-effect
fastxtile tert_pe_m=theta`1' if fem==0, nq(3)
fastxtile tert_pe_f=theta`1' if fem==1, nq(3)
gen tert_pe=tert_pe_m if fem==0
replace tert_pe=tert_pe_f if fem==1
drop tert_w_f tert_w_m tert_pe_m tert_pe_f

* Some summary statistics on wages per wage tercile
ta tert_w, mis
table tert_w if fem==0, c(min ln_impwage mean ln_impwage max ln_impwage freq) format(%9.4gc)
table tert_w if fem==1, c(min ln_impwage mean ln_impwage max ln_impwage freq) format(%9.4gc)

* Restrict sample
keep `8'

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
	mat results=(rss, beta, const, tau)
	di _newline "Find tau that minimizes the rss of system"
	mata: results=st_matrix("results")
	mata: findmin=colmin(results[1..`4',3])
	mata: subres=select(results, results[.,3]:==findmin)
	mata: st_matrix("subres",subres)
* Display regression output: 
* cols 1-3: residual sum of squares for men, women, and men+women
* cols 4&5: estimated betas for men and women
* cols 6&7: estimated constants for men and women
* col 8: estimated tau
matlist subres, format(%10.4gc)	///
				lines(oneline)	///
				tw(5)			///
				showcoleq(c)	///
				linesize(120)	///
				underscore		///
				tit(Tau associated with minimum residual sum of squares based on `2': `yrmin' to `yrmax')
global tau_`2'_`1'_`6'=subres[1,8]
di _newline(2) "Sample-period: `1', productivity measure: `2', stepsize: `5', upper cutoff: `7', `8'"

if "`8'"=="if fem!=." di "Store optimal value for baseline decompositions: " ${tau_`2'_`1'_`6'}

end


* For full sample (set threshold arbitrarily high)

/*
gridsearch 1 VA 2 30 0.05 all 10 "if fem!=." 		/*men: pi0 = 0.2648, pi1 = 0.08039 | women: pi0 = 0.2761, pi1 = 0.0835; c=3.05*/
gridsearch 2 VA 2 30 0.05 all 10 "if fem!=."		/*men: pi0 = 1.696, pi1 = 0.1087| women: pi0 = 0.08621, pi1 = 0.1162; c=2.65*/

gridsearch 1 VA 2 30 0.05 all 10 "if educ<=1"		/*men: pi0 = 0.2525, pi1 = 0.07843 | women: pi0 = 0.257, pi1 = 0.09008; c=2.9*/
gridsearch 2 VA 2 30 0.05 all 10 "if educ<=1"		/*men: pi0 = 1.584, pi1 = 0.127 | women: pi0 =-0.04661, pi1 = 0.1432; c=2.1*/

gridsearch 1 VA 2 30 0.05 all 10 "if educ==2"		/*men: pi0 = 0.2676, pi1 = 0.07766 | women: pi0 = 0.2778, pi1 = 0.07907; c=3.05*/
gridsearch 2 VA 2 30 0.05 all 10 "if educ==2"		/*men: pi0 = 1.706, pi1 = 0.1042| women: pi0 = 0.09997, pi1 = 0.1078; c=2.65*/

gridsearch 1 VA 2 30 0.05 all 10 "if educ>2 & educ<."		/*men: pi0 = 0.275, pi1 = 0.09112| women: pi0 = 0.3025, pi1 = 0.08741; c=3.25*/
gridsearch 2 VA 2 30 0.05 all 10 "if educ>2 & educ<."		/*men: pi0 = 1.718, pi1 = 0.09872| women: pi0 = 0.1105, pi1 = 0.1091; c=2.65*/

gridsearch 1 VA 2 30 0.05 all 10 "if tert_w==1"		/*men: pi0 = , pi1 = 0.103 | women: pi0 = , pi1 = 0.102; c= 3.10*/
gridsearch 2 VA 2 30 0.05 all 10 "if tert_w==1"		/*men: pi0 = , pi1 = 0.148 | women: pi0 = , pi1 = 0.154; c= 3.05*/

gridsearch 1 VA 2 30 0.05 all 10 "if tert_w==2"		/*men: pi0 = , pi1 = 0.063 | women: pi0 = 0.057, pi1 = ; c= 3.25*/
gridsearch 2 VA 2 30 0.05 all 10 "if tert_w==2"		/*men: pi0 = , pi1 = 0.081 | women: pi0 = , pi1 = 0.074; c= 2.65*/

gridsearch 1 VA 2 30 0.05 all 10 "if tert_w==3"		/*men: pi0 = , pi1 = 0.074 | women: pi0 = 0.065, pi1 = ; c= 3.45*/
gridsearch 2 VA 2 30 0.05 all 10 "if tert_w==3"		/*men: pi0 = , pi1 = 0.088 | women: pi0 = , pi1 = 0.086; c= 3.45*/

*gridsearch 1 VA 2 30 0.05 all 10 "if tert_pe==3"		/*men: pi0 = , pi1 =  | women: pi0 = , pi1 = ; c= */
*gridsearch 2 VA 2 30 0.05 all 10 "if tert_pe==3"		/*men: pi0 = , pi1 =  | women: pi0 = , pi1 = ; c= */

*gridsearch 1 VA 2 30 0.05 all 10 "if tert_pe==1"		/*men: pi0 = , pi1 =  | women: pi0 = , pi1 = ; c= */
*gridsearch 2 VA 2 30 0.05 all 10 "if tert_pe==1"		/*men: pi0 = , pi1 =  | women: pi0 = , pi1 = ; c= */

*gridsearch 1 VA 2 30 0.05 all 10 "if tert_pe==2"		/*men: pi0 = , pi1 =  | women: pi0 = , pi1 = ; c= */
*gridsearch 2 VA 2 30 0.05 all 10 "if tert_pe==2"		/*men: pi0 = , pi1 =  | women: pi0 = , pi1 = ; c= */

gridsearch 1 VA 2 30 0.05 all 10 "if tarif==1"		/*men: pi0 = 0.2918, pi1 = 0.07312 | women: pi0 = 0.3023, pi1 = 0.07812; c=3.1*/
gridsearch 2 VA 2 30 0.05 all 10 "if tarif==1"		/*men: pi0 = 1.709, pi1 = 0.1043 | women: pi0 = 0.09962, pi1 = 0.1116; c=2.5*/

gridsearch 1 VA 2 30 0.05 all 10 "if tarif==2"		/*men: pi0 = 0.1793, pi1 = 0.07892| women: pi0 = 0.2027, pi1 = 0.07944; c=2.1*/
gridsearch 2 VA 2 30 0.05 all 10 "if tarif==2"		/*men: pi0 = 1.705, pi1 = 0.09896| women: pi0 = 0.09993, pi1 = 0.1087; c=2.55*/

gridsearch 1 VA 2 30 0.05 all 10 "if tarif==3"		/*men: pi0 = 0.1673, pi1 = 0.1354| women: pi0 = 0.1639, pi1 = 0.1337; c=3.3*/
gridsearch 2 VA 2 30 0.05 all 10 "if tarif==3"		/*men: pi0 = 1.652, pi1 = 0.112| women: pi0 = 0.02912, pi1 = 0.1234; c=3.1*/

gridsearch 1 VA 2 30 0.05 all 10 "if union==1 & tarif<4"	/*men: pi0 = 0.2845, pi1 = 0.07378| women: pi0 = 0.2963, pi1 = 0.07824; c=3.05*/
gridsearch 2 VA 2 30 0.05 all 10 "if union==1 & tarif<4"	/*men: pi0 = 1.711, pi1 = 0.1045| women: pi0 = 0.1035, pi1 = 0.1118; c=2.55*/

gridsearch 1 VA 2 30 0.05 all 10 "if union==0 & tarif<4"	/*men: pi0 = 0.1673, pi1 = 0.1354| women: pi0 = 0.1639, pi1 = 0.1337; c=3.30*/
gridsearch 2 VA 2 30 0.05 all 10 "if union==0 & tarif<4"	/*men: pi0 = 1.652, pi1 = 0.112| women: pi0 = 0.02912, pi1 = 0.1234; c=3.10*/
*/

* for subsample with VA<=5

gridsearch 1 VA 2 40 0.05 all 5 "if fem!=."		/*men: pi0 = 0.2599, pi1 = 0.09989 | women: pi0 = 0.2763, pi1 = 0.09712; c=3.20*/
gridsearch 2 VA 2 40 0.05 all 5 "if fem!=."		/*men: pi0 = 1.709, pi1 = 0.1448 | women: pi0 = 0.1088, pi1 = 0.1449; c=3.10*/

gridsearch 1 VA 2 40 0.05 all 5 "if educ<=1"		/*men: pi0 = 0.2409, pi1 = 0.103 | women: pi0 =0.2517, pi1 = 0.1094; c=3.05*/
gridsearch 2 VA 2 40 0.05 all 5 "if educ<=1"	/*men: pi0 =1.603, pi1 = 0.154 | women: pi0 = -.02062, pi1 =0.1706 ; c=2.55*/

gridsearch 1 VA 2 40 0.05 all 5 "if educ==2"	/*men: pi0 = 0.2681, pi1 = 0.09545 | women: pi0 = 0.2827, pi1 = 0.09144; c=3.25*/
gridsearch 2 VA 2 40 0.05 all 5 "if educ==2"	/*men: pi0 = 1.721, pi1 = 0.1427 | women: pi0 = 0.1244, pi1 = 0.1373; c=3.15*/

gridsearch 1 VA 2 40 0.05 all 5 "if educ>2 & educ<."	/*men: pi0 = 0.2568, pi1 = 0.1182 | women: pi0 = 0.2933, pi1 = 0.1034; c=3.30*/
gridsearch 2 VA 2 40 0.05 all 5 "if educ>2 & educ<."	/*men: pi0 = 1.747, pi1 = 0.1362 | women: pi0 = 0.1535, pi1 = 0.1381; c=3.30*/

gridsearch 1 VA 2 40 0.05 all 5 "if tert_w==1"		/*men: pi0 = 0.207, pi1 = 0.1031 | women: pi0 = 0.2186, pi1 = 0.1019; c= 3.1*/
gridsearch 2 VA 2 40 0.05 all 5 "if tert_w==1"		/*men: pi0 = 1.635, pi1 = 0.1481 | women: pi0 = 0.0276, pi1 =0.1535 ; c= 3.05*/

gridsearch 1 VA 2 40 0.05 all 5 "if tert_w==2"		/*men: pi0 = 0.3242, pi1 = 0.0633 | women: pi0 = 0.3456, pi1 = 0.05723; c= 3.25*/
gridsearch 2 VA 2 40 0.05 all 5 "if tert_w==2"		/*men: pi0 = 1.784, pi1 = 0.08105 | women: pi0 = 0.1975, pi1 = 0.07379; c= 2.65*/

gridsearch 1 VA 2 40 0.05 all 5 "if tert_w==3"		/*men: pi0 = 0.3375, pi1 = 0.07413 | women: pi0 = 0.3616, pi1 = 0.06541; c= 3.45*/
gridsearch 2 VA 2 40 0.05 all 5 "if tert_w==3"		/*men: pi0 = 1.859, pi1 = 0.08801 | women: pi0 = 0.2648, pi1 = 0.08551; c= 3.45*/

gridsearch 1 VA 2 40 0.05 all 5 "if tarif==1"		/*men: pi0 = , pi1 =  | women: pi0 = , pi1 = ; c= */
gridsearch 2 VA 2 40 0.05 all 5 "if tarif==1"		/*men: pi0 = 1.688, pi1 = 0.131| women: pi0 = 0.09157, pi1 = 0.1295; c=2.65*/

gridsearch 1 VA 2 40 0.05 all 5 "if tarif==2"		/*men: pi0 = 0.1603, pi1 = 0.1057| women: pi0 = 0.1948, pi1 = 0.09922; c=2.4*/
gridsearch 2 VA 2 40 0.05 all 5 "if tarif==2"		/*men: pi0 = 1.657, pi1 = 0.137| women: pi0 = 0.0502, pi1 = 0.1488; c=2.6*/

gridsearch 1 VA 2 40 0.05 all 5 "if tarif==3"		/*men: pi0 = 0.1591, pi1 = 0.16| women: pi0 = 0.1567, pi1 = 0.1569; c=3.35*/
gridsearch 2 VA 2 40 0.05 all 5 "if tarif==3"		/*men: pi0 = 1.655, pi1 = 0.1371| women: pi0 = 0.03357, pi1 = 0.1484; c=3.3*/

gridsearch 1 VA 2 40 0.05 all 5 "if union==1 & tarif<4"	/*men: pi0 = 0.271, pi1 = 0.09129 | women: pi0 = 0.287, pi1 = 0.09147; c=3.10*/
gridsearch 2 VA 2 40 0.05 all 5 "if union==1 & tarif<4"	/*men: pi0 = 1.684, pi1 = 0.1322 | women: pi0 = 0.08659, pi1 = 0.1322; c=2.65*/

gridsearch 1 VA 2 40 0.05 all 5 "if union==0 & tarif<4"	/*men: pi0 = 0.1591, pi1 = 0.16 | women: pi0 = 0.1567, pi1 = 0.1569; c=3.35*/
gridsearch 2 VA 2 40 0.05 all 5 "if union==0 & tarif<4"	/*men: pi0 = 1.655, pi1 = 0.1371 | women: pi0 = 0.03357, pi1 = 0.1484; c=3.30*/


