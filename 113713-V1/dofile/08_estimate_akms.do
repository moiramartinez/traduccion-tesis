clear all 
cap log close
set linesize 255
set more off, perm
cap program drop _all
log using ${log}/08_estimate_akms.log, replace
adopath ++${prog}


* Program estimates AKMs by gender and period (2x2). Parameters (person-effects, firm-effects, covariate index, and residuals) are saved 
* in separate files for later use. Person-effects are stored in files with one observation per person; firm-effects are stored in files
* with one observation per establishment; covariates and residuals are stored in files with one observation per person-year.

* Output for Table 2
cap program drop akms
program define akms

timer clear 1
timer on 1

use persnr betnr idnum ln_impwage educ year age fsize fem pink tenure using ${data}/AKM_select_`1' if fem==`2', clear

* Find largest connected set in pooled sample (adofile "grouping" is adapted to stop once 98.5% of person-years are allocated to limit search time)
*qui {
	egen id=group(persnr)
	egen firmid=group(betnr)

	di "Run adapted grouping algorithm... Require at least 0.985 of person-years to be allocated."
	grouping2 group, i(id) j(firmid) lowbound(0.985)

	bys group : gen gs=_N
	sum gs
	mat coverage=(r(max)\r(N)\ 100*r(max)/r(N))
*}
matlist coverage, tit(Number of person-year observations in total sample and in largest connected set) format(%11.4gc)

* Flag largest mobility group
sum gs
gen conn_new=(gs==r(max))

di _newline(3) "AKM Models for Largest Connected Sets"
di "============================================"

* Restrict to largest connected set
keep if conn_new==1

* Last firm (omitted in regressions)
if "`1'"=="1" & "`2'"=="0"{
	gen helpbetnr=(betnr==94319202)
	sort helpbetnr
	drop helpbetnr
}
if "`1'"=="1" & "`2'"=="1"{
	gen helpbetnr=(betnr==35897597)
	sort helpbetnr
	drop helpbetnr
}
if "`1'"=="2" & "`2'"=="0"{
	gen helpbetnr=(betnr==26268824)
	sort helpbetnr
	drop helpbetnr
}
if "`1'"=="1" & "`2'"=="1"{
	gen helpbetnr=(betnr==34981929)
	sort helpbetnr
	drop helpbetnr
}
cap drop firmid id gs

* Make covariates
replace age=(age-40)/40
gen age2=age*age
gen age3=age*age*age

sum
bys fem : sum

char year[omit] 1
xi i.educ*i.year i.educ|age2 i.educ|age3

* Estimate AKMs
a2reg ln_impwage _Ieduc* _Iyear* _IeduXyea* _IeduXage* age2 age3, individual(persnr) unit(betnr) indeffect(theta) uniteffect(psi) xb(xb) resid(r)

di _newline "Summarise parameter estimates"
sum theta psi xb r

di _newline "Variance Decomposition (for estimation sample)"
mat accum sumdev= ln_impwage theta psi xb r , noc dev
mat cov = sumdev/(r(N)-1)
mat corr = corr(sumdev)
matlist cov, format(%9.4gc) lines(oneline) tw(5)	showcoleq(c) linesize(120) underscore		

* Estimate associated Match-Effects model (adjusted R2, RMSE)
egen ij=tag(betnr persnr)
egen jobsp=group(betnr persnr)
areg ln_impwage _Ieduc* _Iyear* _IeduXyea* _IeduXage* age2 age3, absorb(jobsp)
predict e, r
gen e_r=r-e
drop jobsp ij

di _newline "Variance Decomposition including match effect (for estimation sample)"
mat accum sumdev= ln_impwage theta psi xb e_r e, noc dev
mat cov = sumdev/(r(N)-1)
mat corr = corr(sumdev)
matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore		

* Clean up
keep persnr betnr year psi theta xb r

compress
ren theta theta`1'
ren psi psi`1'`2'
ren xb xb`1'
ren r r`1'

di _newline "Export coefficients"

di _newline "1. covariates and residuals (unit of observation: person-year)"	
preserve
	keep persnr year xb`1' r`1'
	capture noisily {

		if ("`2'"=="0") {
			save ${data}/xb_`1', replace
		}
		else {
			append using ${data}/xb_`1'
			save ${data}/xb_`1', replace
		}
	}
restore

di _newline "2. person-effects (unit of observation: person)"
preserve
	capture noisily {

		keep persnr theta`1'
		bys persnr : keep if _n==1
		
		if ("`2'"=="0") {
			save ${data}/peffs_`1', replace
		}
		else {
			append using ${data}/peffs_`1'
			save ${data}/peffs_`1', replace
		}
	}
restore

di _newline "3. firm-effects (unit of observation: establishment)"
preserve
	capture noisily {
		
		keep betnr psi`1'`2'
		bys betnr : keep if _n==1

		if ("`2'"=="0") {
			save ${data}/feffs_`1', replace
		}
		else {
			merge 1:1 betnr using ${data}/feffs_`1' 	// 1 = all female firms ; 2 = all male firms ; 3 = dual-connected
			ta _merge
			drop _merge
			save ${data}/feffs_`1', replace
		}
	}
restore	


timer off 1

di "Time use in minutes: " r(t1)/60

end

akms 1 0 
akms 1 1
akms 2 0 
akms 2 1 


* Program estimates AKMs by period, pooling across genders. Parameters (person-effects, firm-effects, covariate index, and residuals) are saved 
* in separate files for later use. Person-effects are stored in files with one observation per person; firm-effects are stored in files
* with one observation per establishment; covariates and residuals are stored in files with one observation per person-year.


* Output for Table D.1

cap program drop akms
program define akms

timer clear 1
timer on 1

use persnr betnr idnum ln_impwage educ year age fsize fem pink tenure using ${data}/AKM_select_`1' if fem!=., clear

* Find largest connected set in pooled sample (adofile "grouping" is adapted to stop once 98.5% of person-years are allocated to limit search time)
*qui {
	egen id=group(persnr)
	egen firmid=group(betnr)

	di "Run adapted grouping algorithm... Require at least 0.985 of person-years to be allocated."
	grouping2 group, i(id) j(firmid) lowbound(0.985)

	bys group : gen gs=_N
	sum gs
	mat coverage=(r(max)\r(N)\ 100*r(max)/r(N))
*}
matlist coverage, tit(Number of person-year observations in total sample and in largest connected set) format(%11.4gc)

* Flag largest mobility group
sum gs
gen conn_new=(gs==r(max))

di _newline(3) "ESTIMATION FOR NEW LARGEST CONNECTED SET"
di "============================================"

* Restrict to largest connected set
keep if conn_new==1

* Last firm (omitted in regressions)
if "`1'"=="1"{
	gen helpbetnr=(betnr==71348004)
	sort helpbetnr
	drop helpbetnr
}
if "`1'"=="2"{
	gen helpbetnr=(betnr==43506297)
	sort helpbetnr
	drop helpbetnr
}

cap drop firmid id gs

* Make covariates
replace age=(age-40)/40
gen age2=age*age
gen age3=age*age*age

sum
bys fem : sum

char year[omit] 1
xi i.educ*i.year i.educ|age2 i.educ|age3

* Estimate AKMs
a2reg ln_impwage _Ieduc* _Iyear* _IeduXyea* _IeduXage* age2 age3, individual(persnr) unit(betnr) indeffect(theta) uniteffect(psi) xb(xb) resid(r)

di _newline "Summarise parameter estimates"
sum theta psi xb r

di _newline "Variance Decomposition (for estimation sample)"
mat accum sumdev= ln_impwage theta psi xb r , noc dev
mat cov = sumdev/(r(N)-1)
mat corr = corr(sumdev)
matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore		

* Estimate associated Match-Effects model (adjusted R2, RMSE)
egen ij=tag(betnr persnr)
egen jobsp=group(betnr persnr)
areg ln_impwage _Ieduc* _Iyear* _IeduXyea* _IeduXage* age2 age3, absorb(jobsp)
predict e, r
gen e_r=r-e
drop jobsp ij

di _newline "Variance Decomposition including match effect (for estimation sample)"
mat accum sumdev= ln_impwage theta psi xb e_r e, noc dev
mat cov = sumdev/(r(N)-1)
mat corr = corr(sumdev)
matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore		

* Clean up
keep persnr betnr year psi theta xb r

compress
ren theta theta`1'
ren psi psi`1'
ren xb xb`1'
ren r r`1'

di _newline "Export coefficients"

di _newline "1. covariates and residuals (unit of observation: person-year)"	
preserve
	keep persnr year xb`1' r`1'
	capture noisily {
		save ${data}/xb_genderpooled_`1', replace
	}
restore

di _newline "2. person-effects (unit of observation: person)"
preserve
	capture noisily {
		keep persnr theta`1'
		bys persnr : keep if _n==1
		save ${data}/peffs_genderpooled_`1', replace
	}
restore

di _newline "3. firm-effects (unit of observation: establishment)"
preserve
	capture noisily {
		keep betnr psi`1'
		bys betnr : keep if _n==1
		save ${data}/feffs_genderpooled_`1', replace
	}
restore	

timer off 1

di "Time use in minutes: " r(t1)/60

end

akms 1 
akms 2 




* Program estimates AKMs by gender, pooling across periods. Parameters (person-effects, firm-effects, covariate index, and residuals) are saved 
* in separate files for later use. Person-effects are stored in files with one observation per person; firm-effects are stored in files
* with one observation per establishment; covariates and residuals are stored in files with one observation per person-year.

* Output for Table D.1

cap program drop akms
program define akms

timer clear 1
timer on 1

* Load analysis file and rebuild panel for 1995-2008
use persnr betnr year age fem educ ln_impwage using ${data}/AKM_select_1 if year<=2000, clear
append using ${data}/AKM_select_2, keep(persnr betnr year age fem educ ln_impwage)

* Restrict to gender
keep if fem==`1'

* Find largest connected set in pooled sample (adofile "grouping" is adapted to stop once 98.5% of person-years are allocated to limit search time)
*qui {
	egen id=group(persnr)
	egen firmid=group(betnr)

	di "Run adapted grouping algorithm... Require at least 0.985 of person-years to be allocated."
	grouping2 group, i(id) j(firmid) lowbound(0.985)

	bys group : gen gs=_N
	sum gs
	mat coverage=(r(max)\r(N)\ 100*r(max)/r(N))
*}
matlist coverage, tit(Number of person-year observations in total sample and in largest connected set) format(%11.4gc)

* Flag largest mobility group
sum gs
gen conn_new=(gs==r(max))

di _newline(3) "ESTIMATION FOR NEW LARGEST CONNECTED SET"
di "============================================"

* Restrict to largest connected set
keep if conn_new==1

* Last firm (omitted in regressions)
if "`1'"=="0"{
	gen helpbetnr=(betnr==62257540)
	sort helpbetnr
	drop helpbetnr
}
if "`1'"=="1"{
	gen helpbetnr=(betnr==29312556)
	sort helpbetnr
	drop helpbetnr
}

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

di _newline "Summarise parameter estimates"
sum theta psi xb r
bys fem : sum theta psi xb r

di _newline "Variance Decomposition (for estimation sample)"
mat accum sumdev= ln_impwage theta psi xb r , noc dev
mat cov = sumdev/(r(N)-1)
mat corr = corr(sumdev)
matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore		

* Estimate associated Match-Effects model (adjusted R2, RMSE)
egen ij=tag(betnr persnr)
egen jobsp=group(betnr persnr)
areg ln_impwage _Ieduc* _Iyear* _IeduXyea* _IeduXage* age2 age3, absorb(jobsp)
predict e, r
gen e_r=r-e
drop jobsp ij

di _newline "Variance Decomposition including match effect (for estimation sample)"
mat accum sumdev= ln_impwage theta psi xb e_r e, noc dev
mat cov = sumdev/(r(N)-1)
mat corr = corr(sumdev)
matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore		

* Clean up
keep persnr betnr year psi theta xb r
ren psi psi`1'

compress

di _newline "Export coefficients"

di _newline "1. covariates and residuals (unit of observation: person-year)"	
preserve
	keep persnr year xb r
	capture noisily {
		if "`1'"=="0"{
			save ${data}/xb_periodpooled, replace
		}
		else {
			append using ${data}/xb_periodpooled
			save ${data}/xb_periodpooled, replace
		}
	}
restore

di _newline "2. person-effects (unit of observation: person)"
preserve
	capture noisily {
		keep persnr theta`1'
		bys persnr : keep if _n==1
		if "`1'"=="0"{
			save ${data}/peffs_periodpooled, replace
		}
		else {
			append using ${data}/peffs_periodpooled
			save ${data}/peffs_periodpooled, replace
		}
	}
restore

di _newline "3. firm-effects (unit of observation: establishment)"
preserve
	capture noisily {
		keep betnr psi`1'
		bys betnr : keep if _n==1
		if "`1'"=="0"{
			save ${data}/feffs_periodpooled, replace
		}
		else {
			merge 1:1 betnr using ${data}/feffs_periodpooled
			ta _merge
			drop _merge
			save ${data}/feffs_periodpooled, replace
		}
	}
restore	

if "`1'"=="1"{

	* Load analysis file and rebuild panel for 1995-2008
	use persnr betnr year age fem educ ln_impwage using ${data}/AKM_select_1 if year<=2000, clear
	append using ${data}/AKM_select_2, keep(persnr betnr year age fem educ ln_impwage)
	
	* Import firm effects
	merge m:1 betnr using ${data}/feffs_periodpooled, nogen
	keep if psi0!=. & psi1!=.
	
	* Correlation of firm effects
	corr psi1 psi0
	reg psi1 psi0, cluster(betnr)
}

timer off 1

di "Time use in minutes: " r(t1)/60

end

akms 0
akms 1


