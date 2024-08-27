clear all
cap log close
set linesize 120
set more off, perm
log using ${log}/12_binning_fe.log, replace
adopath ++${prog}
set seed 1000


* This program computes mean firm-effects binned by measures of firm-productivity.
* It produces several tables with 100 bins (pct) of firm surplus and corresponding
* values of mean fixed effects. Sample contains only firm with firm-surplus in
* the dual-connected set.

di _newline "Data for binned firm effects"
cap program drop binningsurplus
program define binningsurplus

use ${data}/worker_dual_`1'_`4', clear

qui sum year
local yrmin=r(min)
local yrmax=r(max)

* Import union status from LIAB firm data
merge m:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(tarif hrf_quer) nogen
gen union=(tarif==1|tarif==2)
ta tarif union, mis

* Skill groups based on wage distribution
fastxtile tert_w_m=ln_impwage if fem==0, nq(3)
fastxtile tert_w_f=ln_impwage if fem==1, nq(3)
fastxtile tert_w_p=ln_impwage, nq(3)

gen tert_w=tert_w_m if fem==0
replace tert_w=tert_w_f if fem==1

* Skill groups based on person-effect (only testing)
fastxtile tert_pe_m=theta`1' if fem==0, nq(3)
fastxtile tert_pe_f=theta`1' if fem==1, nq(3)
gen tert_pe=tert_pe_m if fem==0
replace tert_pe=tert_pe_f if fem==1
drop tert_w_f tert_w_m tert_pe_m tert_pe_f

* Some summary statistics to check plausibility of wage grouping
table tert_w if fem==0, c(min ln_impwage mean ln_impwage max ln_impwage freq) format(%9.4gc)
table tert_w if fem==1, c(min ln_impwage mean ln_impwage max ln_impwage freq) format(%9.4gc)
table tert_w_p if fem==0, c(min ln_impwage mean ln_impwage max ln_impwage freq) format(%9.4gc)
table tert_w_p if fem==1, c(min ln_impwage mean ln_impwage max ln_impwage freq) format(%9.4gc)

* Restrict sample to subgroup
keep if `5'

* Generate bins
di _newline "`3' bins of `2'"
fastxtile `2'q=`2' if `2'!=., nq(`3')

tempvar countall countfirms countworkers
gen `countall'=1
bys betnr `2'q year: gen `countfirms'=_n==1
bys persnr `2'q: gen `countworkers'=_n==1
gen theta_m=theta`1' if fem==0
gen theta_f=theta`1' if fem==1

if "`5'"=="fem!=."{
	gen exc`2'=max(0,`2'-${tau_`2'_`1'_all}) if `2'!=.
	di _newline(2)  "MALE RENT-SHARING"
	reg psi`1'0 exc`2', cluster(idnum)
	di _newline(2)  "FEMALE RENT-SHARING"
	reg psi`1'1 exc`2', cluster(idnum)
	ivregress 2sls psi`1'1 (psi`1'0=exc`2'), cluster(idnum)
	ivregress 2sls psi`1'1 (psi`1'0=`2'), cluster(idnum)
}	

di "Averaging firm-effects w/n bins of `1', build output table"
preserve
	collapse 	(mean) psi`1'1 psi`1'0 `2' theta_m theta_f fem 	///
				(sum) persyrs=`countall' firmyrs=`countfirms' workers=`countworkers'	///
				(min) min`2'=`2' (max) max`2'=`2', by(`2'q) fast
	mkmat `2'q `2' psi`1'0 psi`1'1 theta_m theta_f fem persyrs firmyrs workers, matrix(bin`2')
restore
/*
preserve
	collapse 	(mean) psi`1'1 psi`1'0 `2' theta_m theta_f fem 	///
				(sum) persyrs=`countall' firms=`countfirms' workers=`countworkers'	///
				(min) min`2'=`2' (max) max`2'=`2' [aw=hrf_quer], by(`2'q) fast
	mkmat `2'q `2' psi`1'0 psi`1'1 theta_m theta_f fem persyrs workers firms, matrix(bin`2'we)
restore
*/
di _newline "Display output, last 3 cols contain # of person-years, # of firm-years (firms per year), # of workers"
di "Note that workers move between firms which may belong to different bins."
di "Sample: `5'"
matlist bin`2', 	format(%9.5gc)	///
						lines(oneline)	///
						tw(5)			///
						showcoleq(c)	///
						linesize(120)	///
						underscore		///
						tit(`3' bins of `2'; `4' : `yrmin' to `yrmax')
/*
matlist bin`2'we, 	format(%9.4gc)	///
						lines(oneline)	///
						tw(5)			///
						showcoleq(c)	///
						linesize(120)	///
						underscore		///
						tit(`3' bins of `2'; `4' : `yrmin' to `yrmax')
*/
end

global nbins=50
global tau_VA_2_all "3.25"
global tau_VA_2_all "3.10"


binningsurplus 1 VA 100 "all" "fem!=."
binningsurplus 2 VA 100 "all" "fem!=."

binningsurplus 1 VA 50 "all" "fem!=."
binningsurplus 2 VA 50 "all" "fem!=."

*binningsurplus 1 VA 50 "all" "educ<=1"
*binningsurplus 2 VA 50 "all" "educ<=1"

*binningsurplus 1 VA 50 "all" "educ==2"
*binningsurplus 2 VA 50 "all" "educ==2"

*binningsurplus 1 VA 50 "all" "educ>2 & educ<."
*binningsurplus 2 VA 50 "all" "educ>2 & educ<."

*binningsurplus 1 VA 50 "all" "tert_w==1"
*binningsurplus 2 VA 50 "all" "tert_w==1"

*binningsurplus 1 VA 50 "all" "tert_w==2"
*binningsurplus 2 VA 50 "all" "tert_w==2"

*binningsurplus 1 VA 50 "all" "tert_w==3"
*binningsurplus 2 VA 50 "all" "tert_w==3"

*binningsurplus 1 VA 50 "all" "tert_w_p==1"
*binningsurplus 2 VA 50 "all" "tert_w_p==1"

*binningsurplus 1 VA 50 "all" "tert_w_p==2"
*binningsurplus 2 VA 50 "all" "tert_w_p==2"

*binningsurplus 1 VA 50 "all" "tert_w_p==3"
*binningsurplus 2 VA 50 "all" "tert_w_p==3"

*binningsurplus 1 VA $nbins "all" "tert_pe==1"
*binningsurplus 2 VA $nbins "all" "tert_pe==1"

*binningsurplus 1 VA $nbins "all" "tert_pe==2"
*binningsurplus 2 VA $nbins "all" "tert_pe==2"

*binningsurplus 1 VA $nbins "all" "tert_pe==3"
*binningsurplus 2 VA $nbins "all" "tert_pe==3"

*binningsurplus 1 VA 50 "all" "tarif==1"
*binningsurplus 2 VA 50 "all" "tarif==1"

*binningsurplus 1 VA 50 "all" "tarif==2"
*binningsurplus 2 VA 50 "all" "tarif==2"

*binningsurplus 1 VA 50 "all" "tarif==3"
*binningsurplus 2 VA 50 "all" "tarif==3"

*binningsurplus 1 VA 100 "all" "union==1 & tarif<4"
*binningsurplus 2 VA 100 "all" "union==1 & tarif<4"

*binningsurplus 1 VA 100 "all" "union==0 & tarif<4"
*binningsurplus 2 VA 100 "all" "union==0 & tarif<4"



*### Pooled gender-specific AKM-effects ###*
di _newline "Data for binned firm effects"
cap program drop binningsurplus
program define binningsurplus

use ${data}/AKM_select_1 if year<=2000, clear
append using ${data}/AKM_select_2

* merge estimated establishment effects
merge m:1 betnr using ${data}/AKM_fe_allyears_0, keep(1 3) keepus(psi0) nogen		// add psi0
merge m:1 betnr using ${data}/AKM_fe_allyears_1, keep(1 3) keepus(psi1) nogen		// add psi1

* restrict to dual-connected set in pooled sample (all years)
keep if psi0!=.& psi1!=.

qui sum year
local yrmin=r(min)
local yrmax=r(max)

di _newline "`3' bins of `2'"
fastxtile `1'q=`1' if `1'!=., nq(`2')

tempvar countall countfirms countworkers
gen `countall'=1
bys betnr `1'q year: gen `countfirms'=_n==1
bys persnr `1'q: gen `countworkers'=_n==1

di "Averaging firm-effects w/n bins of `1', build output table"
collapse 	(mean) psi1 psi0 `1' fem  	///
			(sum) persyrs=`countall' firmyrs=`countfirms' workers=`countworkers'	///
			(min) min`1'=`1' (max) max`1'=`1', by(`1'q)
mkmat `1'q `1' psi0 psi1 fem persyrs firmyrs workers, matrix(bin`1')
di _newline "Display output, last 3 cols contain # of person-years, # of firms, # of workers"
di "Note that workers move between firms which may belong to different bins."
matlist bin`1', 	format(%9.4gc)	///
						lines(oneline)	///
						tw(5)			///
						showcoleq(c)	///
						linesize(120)	///
						underscore		///
						tit(`2' bins of `1': `yrmin' to `yrmax')

end

*binningsurplus VA 100

cap log close


