
clear all 
cap log close
set linesize 120
set more off, perm
cap program drop _all
log using ${log}/17_rentsharing_models.log, replace
adopath ++${prog}

/*
 * Run additional AKMs and decompose rent-sharing coefficients into components attributable
 * to worker, establishment, and covariates.
 * Similar to CCHK-table, complement Gürtzgen, 2009 evidence on rent-sharing in Germany using
 * between-firm variation.
 */
 

/*
 * Estimate pooled AKM models, pooling across GENDERS (not time); yields one firm effect
 * per firm. Store predicted val's in separate files for each period for later use.
 */


cap program drop rentshpooled
program define rentshpooled

use persnr betnr idnum ln_impwage bula educ sic2 w93_3 year age fsize fem pink tenure ind VA using ${data}/AKM_select_`1', clear

* Import AKM effects (gender-pooled)
merge 1:1 persnr year using ${data}/xb_genderpooled_`1', nogen keep(3) keepus(xb`1' r`1')
merge m:1 persnr using ${data}/peffs_genderpooled_`1', nogen keep(3) keepus(theta`1')
merge m:1 betnr using ${data}/feffs_genderpooled_`1', nogen keep(3) keepus(psi`1')


* Restrict sample
keep if `3'

* Generate potential experience
gen exper=0
replace exper = age - 16 if educ==0 | educ==1
replace exper = age - 19 if educ==2
replace exper = age - 20 if educ==3
replace exper = age - 25 if educ==4
replace exper = 0 if exper < 0

* Further covariates
replace age=(age-40)/40
gen age2=age*age
gen age3=age*age*age

gen exper2=exper*exper/100
gen exper3=exper*exper*exper/1000

* Make firm size groups
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

* globals
global x1 "exper exper2 exper3 educ##year"
global addx2 "i.ind i.bula"
global addx3 "i.w93_3 i.bula"
global addx2i "i.ind i.bula i.fsizegr"
global addx3i "i.w93_3 i.bula i.fsizegr"

gen excVA=max(0,VA-${tau_VA_`1'_all})

* Clean up again
keep persnr betnr idnum ind w93_3 sic2 bula exper* age* year educ `2' ln_impwage theta psi xb fsizegr

* Counts of workers and establishments (= number of observations used in subsequent regressions)
sum
codebook persnr betnr idnum ind bula fsizegr w93_3

* empty container for table:
mat beta=J(8,5,.)

local row=1
local rowplus1=2
local col=1		// model 1
foreach v in ln_impwage theta psi xb{
	qui reg `v' `2' ${x1}, cluster(betnr)
	mat beta[`row',`col']=_b[`2']
	mat beta[`rowplus1',`col']=_se[`2']
	local row=`row'+2
	local rowplus1=`row'+1
}
	
local row=1
local rowplus1=2
local col=2		// model 2

foreach v in ln_impwage theta psi xb{
	qui reg `v' `2' ${x1} ${addx2}, cluster(betnr)
	mat beta[`row',`col']=_b[`2']
	mat beta[`rowplus1',`col']=_se[`2']
	local row=`row'+2
	local rowplus1=`row'+1
}


local row=1
local rowplus1=2
local col=3		// model 3

foreach v in ln_impwage theta psi xb{
	qui reg `v' `2' ${x1} ${addx3}, cluster(betnr)
	mat beta[`row',`col']=_b[`2']
	mat beta[`rowplus1',`col']=_se[`2']
	local row=`row'+2
	local rowplus1=`row'+1
}

local row=1
local rowplus1=2
local col=4		// model 2a

foreach v in ln_impwage theta psi xb{
	qui reg `v' `2' ${x1} ${addx2i}, cluster(betnr)
	mat beta[`row',`col']=_b[`2']
	mat beta[`rowplus1',`col']=_se[`2']
	local row=`row'+2
	local rowplus1=`row'+1
}

local row=1
local rowplus1=2
local col=5		// model 3a

foreach v in ln_impwage theta psi xb{
	qui reg `v' `2' ${x1} ${addx3i}, cluster(betnr)
	mat beta[`row',`col']=_b[`2']
	mat beta[`rowplus1',`col']=_se[`2']
	local row=`row'+2
	local rowplus1=`row'+1
}


* Results table:
* Number of workers and number of establishments that are the basis for estimating the following coefficients
codebook persnr idnum 
* Output
matlist beta , tit(Regression coefficients for period `1', normalised firm-effects, `2', `3')

end


global tau_VA_1_all		"3.25"
global tau_VA_2_all		"3.1" 

rentshpooled 1 VA "VA<=5"
rentshpooled 2 VA "VA<=5"

rentshpooled 1 excVA "VA<=5"
rentshpooled 2 excVA "VA<=5"



cap log close

