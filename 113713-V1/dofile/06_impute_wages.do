* Benjamin Bruns (fdz1213)

* Impute stochastic upper tail of wage distribution using the method of CHK adapted to the
* limitations of the Mover Model.

clear all
set more off, perm
set linesize 120
cap log close

log using ${log}/06_impute_wages.log, replace

use ${data}/export4imputation, clear
sum 

set seed 1000

* Wage
gen ln_impwage=lnrwage

qui sum year
local yrmin=r(min)
local yrmax=r(max)

qui sum educ
local edumin=r(min)
local edumax=r(max)

qui sum agegroup
local agemin=r(min)
local agemax=r(max)


* Loop over years, education groups, age groups (2-5), and gender

forval y=`yrmin'(1)`yrmax' {

	forval educ=`edumin'(1)`edumax' {
	
		forval agec=`agemin'(1)`agemax' {
		
			forval g=0/1 {
			
				di as res "Year : `y'"
				di as res "Bildcat: `educ'"
				di as res "Agegroup: `agec'"
				di as res "Gender: `g'"

				if (`agec'>1 & `agec'<6) {
				
					di "Replace wage in other yrs and share of censored wages in other yrs by sample mean for single-year spells"
					foreach var in lnrwage cens {
						sum `var' if educ==`educ' & agegroup==`agec' & fem==`g' & year==`y'
						replace i`var'=r(mean) if obs1==1 & educ==`educ' & agegroup==`agec' & fem==`g' & year==`y'
					}

					di "Getting SSC-maximum in current year:"
					sum lnrwage if year==`y'
					local wmax=r(max)-0.005
					qui sum lnrwage if year==`y'
					local wmax2=r(max)
					di "Upper bound: `wmax'"

					qui tobit lnrwage age ilnrwage icens largefirm fsize fsize2 obs1 if educ==`educ' & agegroup==`agec' & fem==`g' & year==`y', ul(`wmax')

					di "Fraction of censored calculated from estimation output: " e(N_rc)/e(N)
					qui {
						local sigma =_b[/sigma]
						cap drop xb
						predict xb if e(sample)
						cap drop k
						gen k=normal((`wmax'-xb)/`sigma')
						sum k
						local meank=r(mean)
						cap drop draw
						gen draw=k+uniform()*(1-k)
						replace draw=1-3e-7 if draw>1-3e-7 & draw<.
						replace ln_impwage=xb+`sigma'*invnorm(draw) if e(sample) & lnrwage>=e(ulopt)
						qui sum ln_impwage if e(sample) & lnrwage>=e(ulopt)
						replace ln_impwage=r(mean) if e(sample) & ln_impwage==.
					}
					sum ln_impwage lnrwage if e(sample), d
				}
			}
		}
	}
}
sum ln_impwage lnrwage, d
table year fem, c(mean ln_impwage sd ln_impwage)
table year fem, c(mean lnrwage sd lnrwage)

lab data "allocated wages using orig-spells"
save  ${data}/impwages, replace

* Clear and load masterfile to import imputed wages
clear
use ${data}/masterfile

* Import imputed wages
merge 1:1 persnr year using ${data}/impwages, keepus(ln_impwage)
qui compress
save ${data}/masterfile, replace

* Clean up
cap erase ${data}/export4imputation.dta
cap erase ${data}/impwages.dta

cap log close





