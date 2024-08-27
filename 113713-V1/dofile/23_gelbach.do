clear all
	cap log close
set linesize 120
set more off, perm
cap program drop _all
log using ${log}/23_gelbach.log, replace
adopath ++${prog}
set seed 1000


* Perform Gelbach's exact decomposition of the GWG by estimating a pooled model.
* Also perform decomposition for the dual-connected set.
* OBD not feasible, due to single firm-effect; only identify distributional differences.

cap program drop gelbach
program define gelbach

use persnr betnr ln_impwage educ year age fsize fem pink tenure using ${data}/AKM_select_`1', clear

* Import Xb's and residual by person-year
merge 1:1 persnr year using ${data}/xb_genderpooled_`1', keep(3) nogen keepus(xb`1' r`1')

* Import person effects by person
merge m:1 persnr using ${data}/peffs_genderpooled_`1', keep(3) nogen keepus(theta`1')

* Import person effects by person
merge m:1 betnr using ${data}/feffs_genderpooled_`1', keep(3) nogen keepus(psi`1')

* make covariates as in AKMs by gender
replace age=(age-40)/40
gen age2=age*age
gen age3=age*age*age

sum
bys fem : sum
* # of job spells:
egen jobsp=group(betnr persnr)
sum jobsp

char year[omit] 1
xi i.educ*i.year i.educ|age2 i.educ|age3

ren xb`1' xb
ren r`1' r
ren theta`1' theta
ren psi`1' psi

* estimate unrestricted model
reg ln_impwage fem, cluster(betnr)
di "Conditional GWG : " _b[fem]
scalar gamma_r=_b[fem]

* auxiliary models
reg xb fem, cluster(betnr)
scalar tau_x=_b[fem]

reg theta fem, cluster(betnr)
scalar tau_t=_b[fem]

reg psi fem, cluster(betnr)
scalar tau_p=_b[fem]

di "Check: gamma_r = tau_t + tau_p"
di "gamma_r   =  " gamma_r
di "tau_x     =  " tau_x
di "tau_t     =  " tau_t
di "tau_p     =  " tau_p
di "sum tau_* =  " tau_t+tau_p+tau_x


* Show variance decomposition
mat accum sumdev= ln_impwage theta psi xb r , noc dev
mat cov = sumdev/(r(N)-1)
mat corr = corr(sumdev)
matlist cov, format(%9.4gc) lines(oneline) tw(5)	showcoleq(c) linesize(120) underscore		

* Now focus on those firms/workers in dual-connected set of main analysis
* Import male/female firm effects and drop observations for which either male or female FE is missing
merge m:1 betnr using ${data}/feffs_`1', keep(3) nogen
keep if psi`1'0!=. & psi`1'1!=.
drop psi`1'?

* estimate unrestricted model in dual-connected set
reg ln_impwage fem, cluster(betnr)
di "Conditional GWG : " _b[fem]
scalar gamma_r=_b[fem]

* auxiliary models in dual-connected set
reg xb fem, cluster(betnr)
scalar tau_x=_b[fem]

reg theta fem, cluster(betnr)
scalar tau_t=_b[fem]

reg psi fem, cluster(betnr)
scalar tau_p=_b[fem]

di "Gelbach's exact decomposition for dual-connected set"
di "Check: gamma_r = tau_t + tau_p"
di "gamma_r   =  " gamma_r
di "tau_x     =  " tau_x
di "tau_t     =  " tau_t
di "tau_p     =  " tau_p
di "sum tau_* =  " tau_t+tau_p+tau_x

end

gelbach 1
gelbach 2


cap log close










