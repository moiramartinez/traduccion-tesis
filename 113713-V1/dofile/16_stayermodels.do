clear all
cap log close
set linesize 120
set more off, perm
log using ${log}/16_stayermodels.log, replace
adopath ++${prog}
set seed 1000

* This program estimates stayer models based on changes in value added within firms over time.
* The first part builds a panel of stayers/firms, and the second part runs the regressions.
* I fit models for log value added per worker, excess log value added per worker, and for sales per worker,
* and I impose different restrictions on the changes in these measures (trimming, winsorizing, no restrictions).
* I also estimate different specifications and for different subperiods.

* Set format for regression output
set cformat %9.4f, permanently
set pformat %5.4f, permanently


* Combine the two intervals
use idnum betnr year educ lVApw lrevpw shareinputs ind bula fem using ${data}/AKM_select_1 if year<=2000, clear
append using ${data}/AKM_select_2, keep(idnum betnr year educ lVApw lrevpw shareinputs ind bula fem)

* Only LIAB-firms
keep if idnum!=.

gen educ3=educ
recode educ3 (0 1=1) (3 2=2) (4=3)
lab def educ3 1 "low-skilled" 2 "medium-skilled" 3 "high-skilled", modify
lab val educ3 educ3
ta educ3, g(d)

* Merge nb. of full-time workers from EHP; variable az_ges has structural break in 1999 --> not useful to compute per capita values.
cap drop jahr
gen jahr=year
merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis.dta, keepus(az_ges_vz) nogen update keep(3)
drop jahr

ren lVApw lva
ren lrevpw lsales

* Collapse to firm-year level
collapse (firstnm) lva lsales ind az_ges_vz shareinputs (mean) lowsh=d1 medsh=d2 highsh=d3 femsh=fem (count) N=betnr, by(idnum year)

* Calculate average firms size
bys idnum (year) : gen avfsize=(az_ges_vz+az_ges_vz[_n-1])/2
bys idnum (year) : replace avfsize=az_ges_vz if _n==1

* Merge union dummy
merge 1:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(tarif) nogen
misstable sum
replace tarif=2 if missing(tarif)
gen dtarif=tarif<2

* tssetting data to firm-year panel
xtset idnum year


* This part runs an imputation procedure on value added/sales. I dont use that in the later analysis.

/*
* Generate leave out means (exploit longitudinal firm information in imputation)
foreach x in avfsize lva lsales {
	egen `x'_ = mean(`x'), by(idnum)
	bys idnum : gen lom_`x' = (_N*`x'_ - `x')/(_N-1)
	drop `x'_
}
su avfsize lom_avfsize lva lom_lva lsales lom_lsales

* Replace missings by mean value of all other firms in same industry and year
foreach x in avfsize lva lsales {
	egen `x'_ind=mean(`x'), by(ind year)
	bys idnum: replace lom_`x' = `x'_ind if `x'==.
	drop `x'_ind
}

gen ln_avfsize = ln(avfsize)
gen ln_lom_avfsize =ln(lom_avfsize)
gen ln_avfsize2 = ln(avfsize*avfsize)
gen ln_lom_avfsize2 =ln(lom_avfsize*lom_avfsize)
su avfsize ln_avfsize ln_avfsize2 lom_avfsize ln_lom_avfsize ln_lom_avfsize2 lva lom_lva lsales lom_lsales

* Imputation of lva and lsales
qui su year
local ymin=r(min)
local ymax=r(max)

gen lva_imp1a=lva
gen lva_imp1b=lva
gen lva_imp1c=lva
gen lva_imp2a=lva
gen lva_imp2b=lva
gen lva_imp2c=lva
gen lsales_imp1a=lsales
gen lsales_imp1b=lsales
gen lsales_imp1c=lsales
gen lsales_imp2a=lsales
gen lsales_imp2b=lsales
gen lsales_imp2c=lsales


* only firms with at least one year of valid VA and sales
egen lva_ = max(lva), by(idnum)
egen lsales_ = max(lsales), by(idnum)
gen oneplusyear = (lva_!=. & lsales_!=.)
* cross-tab no. of firm-years that have at least one year of non-missing data:
ta oneplusyear
keep if lva_!=. & lsales_!=.
drop lva_ lsales_ oneplusyear


gen lva_impflag1a=0
gen lva_impflag1b=0
gen lva_impflag1c=0
gen lva_impflag2a=0
gen lva_impflag2b=0
gen lva_impflag2c=0
gen lsales_impflag1a=0
gen lsales_impflag1b=0
gen lsales_impflag1c=0
gen lsales_impflag2a=0
gen lsales_impflag2b=0
gen lsales_impflag2c=0


* IMPUTATION:
* 2 versions with 3 alternatives
* version 1: basic controls, no longitudinal info
* version 2: extended controls including longitudinal info
* alternative a: use predicted value based on Xb directly
* alternative b: use predicted value and standard deviation of productivity measure to get a random draw from log-normal dist
* alternative c: as b, but use standard deviation specific for the industry a firm belongs to

foreach v in lva lsales {

	forval y=`ymin'(1)`ymax' {

	* IMPUTATION: BASIC MODEL
	
		* Imputation version 1a: use predicted value from regression
		cap qui {
			reg `v' femsh lowsh medsh ln_avfsize ln_avfsize2 ln_lom_avfsize ln_lom_avfsize2 i.ind if year==`y'
			predict `v'_hat if `v'==., xb
			replace `v'_impflag1a=1 if `v'_imp1a==. & year==`y'
			replace `v'_imp1a=`v'_hat if `v'_imp1a==. & year==`y'
			drop `v'_hat
		}
		
		* Imputation version 1b : add random noise to predicted value, standard deviation from overall sample
		cap qui {
			reg `v' femsh lowsh medsh ln_avfsize ln_avfsize2 ln_lom_avfsize ln_lom_avfsize2 i.ind if year==`y'
			predict `v'_hat if `v'==., xb
			su `v' if year==`y'
			global sd=r(sd)
			bys idnum (year): replace `v'_impflag1b=1 if year==`y' & `v'_imp1b==. & `v'_hat!=.
			bys idnum (year): replace `v'_imp1b=`v'_hat + ${sd}*invnorm(uniform()) if year==`y' & `v'_imp1b==.
			drop `v'_hat
		}
		
		* Imputation version 1c : add random noise to predicted value, standard deviation from industry specific value added
		cap qui {
			reg `v' femsh lowsh medsh ln_avfsize ln_avfsize2 ln_lom_avfsize ln_lom_avfsize2 i.ind if year==`y'
			predict `v'_hat if `v'==., xb
			qui levelsof ind, l(j)
			foreach i of local j {
				su `v' if year==`y' & ind==`i'
				global sd=r(sd)
				bys idnum (year): replace `v'_impflag1c=1 if year==`y' & ind==`i' & `v'_imp1c==. & `v'_hat!=.
				bys idnum (year): replace `v'_imp1c=`v'_hat + ${sd}*invnorm(uniform()) if year==`y' & ind==`i' & `v'_imp1c==.
			}
			drop `v'_hat
		}
		
	* IMPUTATION: INCLUDE CONTROLS FOR LONGITUDINAL INFO
	
		* Imputation version 2a: use predicted value from regression
		cap qui {
			reg `v' femsh lowsh medsh ln_avfsize ln_avfsize2 ln_lom_avfsize ln_lom_avfsize2 lom_`v' lom_lsales i.ind if year==`y'
			predict `v'_hat if `v'==., xb
			replace `v'_impflag2a=1 if `v'_imp2a==. & year==`y'
			replace `v'_imp2a=`v'_hat if `v'_imp2a==. & year==`y'
			drop `v'_hat
		}
		
		* Imputation version 1b : add random noise to predicted value, standard deviation from overall sample
		cap qui {
			reg `v' femsh lowsh medsh ln_avfsize ln_avfsize2 ln_lom_avfsize ln_lom_avfsize2 lom_`v' lom_lsales i.ind if year==`y'
			predict `v'_hat if `v'==., xb
			su `v' if year==`y'
			global sd=r(sd)
			bys idnum (year): replace `v'_impflag2b=1 if year==`y' & `v'_imp2b==. & `v'_hat!=.
			bys idnum (year): replace `v'_imp2b=`v'_hat + ${sd}*invnorm(uniform()) if year==`y' & `v'_imp2b==.
			drop `v'_hat
		}
		
		* Imputation version 1c : add random noise to predicted value, standard deviation from industry specific value added
		cap qui {
			reg `v' femsh lowsh medsh ln_avfsize ln_avfsize2 ln_lom_avfsize ln_lom_avfsize2 lom_`v' lom_lsales i.ind if year==`y'
			predict `v'_hat if `v'==., xb
			qui levelsof ind, l(j)
			foreach i of local j {
				su `v' if year==`y' & ind==`i'
				global sd=r(sd)
				bys idnum (year): replace `v'_impflag2c=1 if year==`y' & ind==`i' & `v'_imp2c==. & `v'_hat!=.
				bys idnum (year): replace `v'_imp2c=`v'_hat + ${sd}*invnorm(uniform()) if year==`y' & ind==`i' & `v'_imp2c==.
			}
			drop `v'_hat
		}

	}
}

* Distribution of observed vs. imputed/allocated values
ta year lva_impflag1a
ta year lva_impflag2a
ta year lsales_impflag1a
ta year lsales_impflag2a
*/

* Value added in excess of 3.15 (results of "gridsearch" for value added when pooling all years)
gen exlva=max(0,lva-3.15) if lva!=.
su lva exlva

* Compute 3-year change and winsorize/trim productivity measures
xtset idnum year
foreach v in lva exlva lsales { /*lva_imp1a lva_imp1b lva_imp1c lva_imp2a lva_imp2b lva_imp2c lsales_imp1a lsales_imp1b lsales_imp1c lsales_imp2a lsales_imp2b lsales_imp2c */

	gen D3`v' = `v'-l3.`v'
	gen D3`v'_win50 = D3`v' if D3`v'>=-.5 & D3`v'<=.5
	replace D3`v'_win50 = -.5 if D3`v'<-.5 & D3`v'!=.
	replace D3`v'_win50 = .5 if D3`v'>.5 & D3`v'!=.
	gen D3`v'_win75 = D3`v' if D3`v'>=-.75 & D3`v'<=.75
	replace D3`v'_win75 = -.75 if D3`v'<-.75 & D3`v'!=.
	replace D3`v'_win75 = .75 if D3`v'>.75 & D3`v'!=.
	gen D3`v'_trim75 = D3`v' if D3`v'>=-.75 & D3`v'<=.75

}


* Summarise

* VALUE ADDED
foreach v in lva exlva { /*lva_imp1a lva_imp1b lva_imp1c lva_imp2a lva_imp2b lva_imp2c */
	
	* Levels:
	di "each firm weighted equally"
	xtsum `v'
	di "weighted by person-years"
	sum `v' [aw=N]
	
	* 3-year changes:
	di "each firm weighted equally"
	xtsum D3`v' D3`v'_win50 D3`v'_win75 D3`v'_trim75
	di "weighted by person-years"
	sum D3`v' D3`v'_win50 D3`v'_win75 D3`v'_trim75 [aw=N]

}
* SALES

foreach v in lsales { /*lsales_imp1a lsales_imp1b lsales_imp1c lsales_imp2a lsales_imp2b lsales_imp2c */
	
	* Levels:
	di "each firm weighted equally"
	xtsum `v' 
	di "weighted by person-years"
	sum `v' [aw=N]

	* 3-year changes:
	di "each firm weighted equally"
	xtsum D3`v' D3`v'_win50 D3`v'_win75 D3`v'_trim75
	di "weighted by person-years"
	sum D3`v' D3`v'_win50 D3`v'_win75 D3`v'_trim75 [aw=N]

}
* Histograms:

* Levels of value added
qui su year
foreach v in lva exlva { /*lva_imp1a lva_imp1b lva_imp1c lva_imp2a lva_imp2b lva_imp2c*/
	cap noisily foreach y in 1995 2008 {
		twoway__histogram_gen `v' if year==`y', bin(30) gen(dens x) dens
		twoway__histogram_gen `v' if year==`y', bin(30) gen(freq x2) freq
		format dens freq x %9.0g
		mkmat dens freq x if x!=., matrix(hist)
		matlist hist, tit(variable = `v'; year = `y')
		cap drop dens x x2 freq
	}
	cap drop dens x x2 freq
}

* Levels of sales
foreach v in lsales { /*lsales_imp1a lsales_imp1b lsales_imp1c lsales_imp2a lsales_imp2b lsales_imp2c */
	cap noisily foreach y in 1995 2008 {
		twoway__histogram_gen `v' if year==`y', bin(30) gen(dens x) dens
		twoway__histogram_gen `v' if year==`y', bin(30) gen(freq x2) freq
		format dens freq x %9.0g
		mkmat dens freq x if x!=., matrix(hist)
		matlist hist, tit(variable = `v'; year = `y')
		cap drop dens x x2 freq
	}
	cap drop dens x x2 freq
}

* Changes of value added
foreach v in lva lva_win50 lva_win75 lva_trim75 {
	cap noisily foreach y in 1998 2008 {
		twoway__histogram_gen D3`v' if year==`y', bin(30) gen(dens x) dens
		twoway__histogram_gen D3`v' if year==`y', bin(30) gen(freq x2) freq
		format dens freq x %9.0g
		mkmat dens freq x if x!=., matrix(hist)
		matlist hist, tit(variable = D3`v'; year = `y')
		cap drop dens x x2 freq
	}
	cap drop dens x x2 freq
}

* Changes of value added
foreach v in exlva exlva_win50 exlva_win75 exlva_trim75 {
	cap noisily foreach y in 1998 2008 {
		twoway__histogram_gen D3`v' if year==`y', bin(30) gen(dens x) dens
		twoway__histogram_gen D3`v' if year==`y', bin(30) gen(freq x2) freq
		format dens freq x %9.0g
		mkmat dens freq x if x!=., matrix(hist)
		matlist hist, tit(variable = D3`v'; year = `y')
		cap drop dens x x2 freq
	}
	cap drop dens x x2 freq
}


* Changes of sales
foreach v in lsales lsales_win50 lsales_win75 lsales_trim75{
	cap noisily foreach y in 1998 2008 {
		twoway__histogram_gen D3`v' if year==`y', bin(30) gen(dens x) dens
		twoway__histogram_gen D3`v' if year==`y', bin(30) gen(freq x2) freq
		format dens freq x %9.0g
		mkmat dens freq x if x!=., matrix(hist)
		matlist hist, tit(variable = D3`v'; year = `y')
		cap drop dens x x2 freq
	}
	cap drop dens x x2 freq
}

save ${data}/firmpanel.dta, replace


* Descriptives of firm productivity

use ${data}/firmpanel, clear

* Import cross-section weights
merge 1:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(hrf_quer) nogen

* Firm size distribution
gen Ngroup=1 if N<=5
replace Ngroup=2 if N>5 & N<=10
replace Ngroup=3 if N>10 & N<=20
replace Ngroup=4 if N>20 & N<=50
replace Ngroup=5 if N>50 & N<=100
replace Ngroup=6 if N>100 & N<=250
replace Ngroup=7 if N>250 & N<=500
replace Ngroup=8 if N>500 & N<=.

gen azgroup=1 if avfsize<=5
replace azgroup=2 if avfsize>5 & avfsize<=10
replace azgroup=3 if avfsize>10 & avfsize<=20
replace azgroup=4 if avfsize>20 & avfsize<=50
replace azgroup=5 if avfsize>50 & avfsize<=100
replace azgroup=6 if avfsize>100 & avfsize<=250
replace azgroup=7 if avfsize>250 & avfsize<=500
replace azgroup=8 if avfsize>500 & avfsize<=.

* Distribution of full-time employees based on admin info and observed in data 
ta Ngroup azgroup, mis


* Make productivity measures constant, 3 weighting schemes:
* type 1: average across all available years without weighting
* type 2: average across all available years weighted by person-years (as baseline specification)
* type 3: average across all available years weighted by number of full time employees based on admin info
 
qui foreach v in lva lsales { /*lva_imp1a lva_imp1b lva_imp1c lva_imp2a lva_imp2b lva_imp2c lsales_imp1a lsales_imp1b lsales_imp1c lsales_imp2a lsales_imp2b lsales_imp2c*/

	egen `v'sum1=sum(`v') if `v'!=., by(idnum)
	egen `v'sum2=sum(`v'*N) if `v'!=., by(idnum)
	egen `v'sum3=sum(`v'*avfsize) if `v'!=., by(idnum)
	
	egen sum1=count(`v') if `v'!=., by(idnum)
	egen sum2=sum(N) if `v'!=., by(idnum)
	egen sum3=sum(avfsize) if `v'!=., by(idnum)
	gen av_`v'1=`v'sum1/sum1
	gen av_`v'2=`v'sum2/sum2
	gen av_`v'3=`v'sum3/sum3
	drop sum? `v'sum?

}


* Build panel of firms with 3-year changes in mean wages of stayers and worker characteristics as of t-3.

* lag
local l=3

cap erase ${data}/stayers_lva.dta
cap erase ${data}/stayers_exlva.dta
cap erase ${data}/stayers_lsales.dta

foreach pm in exlva lva lsales { /*lva_imp1a lva_imp1b lva_imp1c lva_imp2a lva_imp2b lva_imp2c lsales_imp1a lsales_imp1b lsales_imp1c lsales_imp2a lsales_imp2b lsales_imp2c */

	forval y=1998(1)2008{
		
		use ${data}/firmpanel, clear
	qui{
		keep if year<=`y' & year>=`y'-`l'
		di "from " `y'-3 " to " `y' 

		* Only firms with VA in all years
		gen tmpva = 1 if `pm'!=.
		egen vayrs = sum(tmpva), by(idnum)
		keep if vayrs==`l'+1
		drop vayrs
		bys idnum (year): gen `pm'_lyr=`pm'[_N]
		bys idnum (year): gen `pm'_fyr=`pm'[1]
		keep if year==`y'
		keep idnum `pm' D3* avfsize

		* Merge worker data
		merge 1:m idnum using ${data}/masterfile, keep(3) nogen
		ta year

		* Restricting years
		keep if year<=`y' & year>=`y'-`l'

		ta year

		* Only workers observed in all years
		gen m=(fem==0)
		gen f=(fem==1)
		egen Nbm=sum(m) if year==`y'-`l', by(idnum)
		egen Nbf=sum(f) if year==`y'-`l', by(idnum)
		bys persnr: keep if _N==`l'+1
		egen sd=sd(idnum), by(persnr)
		keep if sd==0
		ta year

		* Ensure that firm has male and female stayers
		egen sdg=sd(fem), by(idnum)
		keep if sdg!=0 & sdg!=.

		drop sd sdg

		egen tmpm=sum(m), by(idnum year)
		egen tmpf=sum(f), by(idnum year)
		gen tmpfemsh=tmpf/(tmpf+tmpm)
		su tmpfemsh
		drop tmpm tmpf tmpfemsh

		gen femsh=Nbf/(Nbf+Nbm)

		* Wage change
		xtset persnr year
		ren ln_impwage lw
		gen D3lw=lw - l`l'.lw

		* Initial t-3: age, education, firmsize (fulltime), share females, wage

		foreach v in age educ avfsize femsh lw Nbm Nbf{
			sort persnr year
			by persnr: gen `v'_fyr=`v'[1]
		}

		gen age2 = age_fyr^2

		bys persnr (year) : gen lw_lyr=lw[_N]

		keep if year==`y'
		keep idnum year *_fyr *_lyr age2 D3`pm'* D3lw `pm' persnr fem ind bula
	}
		* Residualise wages
		xtset idnum
		forval g=0/1 {
			xtreg D3lw age_fyr age2 if fem==`g', fe cluster(idnum) 
			predict lwchange_`g', u
		}

		* Save
		if year==1998{
			save ${data}/stayers_`pm', replace
		}
		if year>1998 {
			append using ${data}/stayers_`pm'
			compress
			save ${data}/stayers_`pm', replace
		}

	}
	
* STAYER REGRESSIONS
*********************
	use ${data}/stayers_`pm', clear
	cap erase ${data}/stayers_`pm'.dta
	
	* Summary statistics
	gen aget=age_fyr+3
	qui ta ind, g(I)
	qui ta educ_fyr, g(E)
	codebook idnum 					// get number of firms
	codebook persnr if fem==0		// get number of men
	codebook persnr if fem==1		// get number of women
	tabstat D3lw lw_fyr age_fyr aget E* I* femsh_fyr `pm' if fem==0, c(s) stat(mean n)
	tabstat D3lw lw_fyr age_fyr aget E* I* femsh_fyr `pm' if fem==1, c(s) stat(mean n)
	
	collapse (mean) lwchange_? (firstnm) Nbm_fyr Nbf_fyr `pm' D3* ind bula, by(idnum year) fast
	
	gen Ntot = Nbm_fyr+Nbf_fyr
	ta year


* Regressions for pooled years:
********************************
	
di _newline(3) " Winsorized: +/- 50% "
	
**** No controls
	* men
	reg lwchange_0 D3`pm'_win50 [aweight=Ntot], cluster(idnum)
	predict lwch_m_hat_win50 if e(sample), xb
	
	* women
	reg lwchange_1 D3`pm'_win50 [aweight=Ntot], cluster(idnum)
	predict lwch_f_hat_win50 if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 (lwchange_0=D3`pm'_win50) [aweight=Ntot], first cluster(idnum)

**** Year dummies
	* men
	reg lwchange_0 D3`pm'_win50 i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_m_hat_win50 if e(sample), xb

	* women
	reg lwchange_1 D3`pm'_win50 i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_f_hat_win50 if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_win50) [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies
	* men
	reg lwchange_0 D3`pm'_win50 i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm'_win50 i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind (lwchange_0=D3`pm'_win50) [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies + federal state dummies
	* men
	reg lwchange_0 D3`pm'_win50 i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm'_win50 i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind i.bula (lwchange_0=D3`pm'_win50) [aweight=Ntot], first cluster(idnum)

	fastxtile `pm'q10=D3`pm'_win50 if D3`pm'_win50!=. [aw=Ntot], nq(10)
	table `pm'q10 [aw=Ntot], c(mean D3`pm'_win50 mean lwchange_0 mean lwchange_1 mean lwch_m_hat_win50 mean lwch_f_hat_win50) format(%9.4gc)
	table `pm'q10, c(freq)
	drop `pm'q10
	
	
di _newline(3) " Winsorized: +/- 75% "
		
**** No controls
	
	* men
	reg lwchange_0 D3`pm'_win75 [aweight=Ntot], cluster(idnum)
	predict lwch_m_hat_win75 if e(sample), xb
	
	* women
	reg lwchange_1 D3`pm'_win75 [aweight=Ntot], cluster(idnum)
	predict lwch_f_hat_win75 if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 (lwchange_0=D3`pm'_win75) [aweight=Ntot], first cluster(idnum)

**** Year dummies
	
	* men
	reg lwchange_0 D3`pm'_win75 i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_m_hat_win75 if e(sample), xb

	* women
	reg lwchange_1 D3`pm'_win75 i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_f_hat_win75 if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_win75) [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies
	
	* men
	reg lwchange_0 D3`pm'_win75 i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm'_win75 i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind (lwchange_0=D3`pm'_win75) [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies + federal state dummies
	
	* men
	reg lwchange_0 D3`pm'_win75 i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm'_win75 i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind i.bula (lwchange_0=D3`pm'_win75) [aweight=Ntot], first cluster(idnum)

	fastxtile `pm'q10=D3`pm'_win75 if D3`pm'_win75!=. [aw=Ntot], nq(10)
	table `pm'q10 [aw=Ntot], c(mean D3`pm'_win75 mean lwchange_0 mean lwchange_1 mean lwch_m_hat_win75 mean lwch_f_hat_win75) format(%9.4gc)
	table `pm'q10, c(freq)
	drop `pm'q10
	
	
di _newline(3) " Trimmed: +/- 75% "
	
**** No controls

	* men
	reg lwchange_0 D3`pm'_trim75 [aweight=Ntot], cluster(idnum)
	predict lwch_m_hat_trim75 if e(sample), xb
	
	* women
	reg lwchange_1 D3`pm'_trim75 [aweight=Ntot], cluster(idnum)
	predict lwch_f_hat_trim75 if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 (lwchange_0=D3`pm'_trim75) [aweight=Ntot], first cluster(idnum)

**** Year dummies

	* men
	reg lwchange_0 D3`pm'_trim75 i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_m_hat_trim75 if e(sample), xb

	* women
	reg lwchange_1 D3`pm'_trim75 i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_f_hat_trim75 if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_trim75) [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies
	
	* men
	reg lwchange_0 D3`pm'_trim75 i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm'_trim75 i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind (lwchange_0=D3`pm'_trim75) [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies + federal state dummies
	
	* men
	reg lwchange_0 D3`pm'_trim75 i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm'_trim75 i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind i.bula (lwchange_0=D3`pm'_trim75) [aweight=Ntot], first cluster(idnum)

	fastxtile `pm'q10=D3`pm'_trim75 if D3`pm'_trim75!=. [aw=Ntot], nq(10)
	table `pm'q10 [aw=Ntot], c(mean D3`pm'_trim75 mean lwchange_0 mean lwchange_1 mean lwch_m_hat_trim75 mean lwch_f_hat_trim75) format(%9.4gc)
	table `pm'q10, c(freq)
	drop `pm'q10
	
	
di _newline(3) " Non-winsorized "

**** No controls
	
	* men
	reg lwchange_0 D3`pm' [aweight=Ntot], cluster(idnum)
	predict lwch_m_hat if e(sample), xb
	
	* women
	reg lwchange_1 D3`pm' [aweight=Ntot], cluster(idnum)
	predict lwch_f_hat if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 (lwchange_0=D3`pm') [aweight=Ntot], first cluster(idnum)

**** Year dummies	
	
	* men
	reg lwchange_0 D3`pm' i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_m_hat if e(sample), xb

	* women
	reg lwchange_1 D3`pm' i.year [aweight=Ntot], cluster(idnum)
	predict lwchy_f_hat if e(sample), xb
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm') [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies
	
	* men
	reg lwchange_0 D3`pm' i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm' i.year i.ind [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind (lwchange_0=D3`pm') [aweight=Ntot], first cluster(idnum)

**** Year dummies + industry dummies + federal state dummies
	
	* men
	reg lwchange_0 D3`pm' i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* women
	reg lwchange_1 D3`pm' i.year i.ind i.bula [aweight=Ntot], cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year i.ind i.bula (lwchange_0=D3`pm') [aweight=Ntot], first cluster(idnum)

	fastxtile `pm'q10=D3`pm' if D3`pm'!=. [aw=Ntot], nq(10)
	table `pm'q10 [aw=Ntot], c(mean D3`pm' mean lwchange_0 mean lwchange_1 mean lwch_m_hat mean lwch_f_hat) format(%9.4gc)
	table `pm'q10, c(freq)
	drop `pm'q10

* Regressions for subperiods (only preferred specification with year dummies):
*******************************************************************************

**** Pre-2003

di _newline(3) " Winsorized: +/- 50% "
	
	* men
	reg lwchange_0 D3`pm'_win50 i.year [aweight=Ntot] if year<=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm'_win50 i.year [aweight=Ntot] if year<=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_win50) [aweight=Ntot] if year<=2003, first cluster(idnum)

di _newline(3) " Winsorized: +/- 75% "
	
	* men
	reg lwchange_0 D3`pm'_win75 i.year [aweight=Ntot] if year<=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm'_win75 i.year [aweight=Ntot] if year<=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_win75) [aweight=Ntot] if year<=2003, first cluster(idnum)

di _newline(3) " Trimmed: +/- 75% "
	
	* men
	reg lwchange_0 D3`pm'_trim75 i.year [aweight=Ntot] if year<=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm'_trim75 i.year [aweight=Ntot] if year<=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_trim75) [aweight=Ntot] if year<=2003, first cluster(idnum)

di _newline(3) " Non-winsorized "

	* men
	reg lwchange_0 D3`pm' i.year [aweight=Ntot] if year<=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm' i.year [aweight=Ntot] if year<=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm') [aweight=Ntot] if year<=2003, first cluster(idnum)

**** Post-2003

di _newline(3) " Winsorized: +/- 50% "
	
	* men
	reg lwchange_0 D3`pm'_win50 i.year [aweight=Ntot] if year>=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm'_win50 i.year [aweight=Ntot] if year>=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_win50) [aweight=Ntot] if year>=2003, first cluster(idnum)

di _newline(3) " Winsorized: +/- 75% "
	
	* men
	reg lwchange_0 D3`pm'_win75 i.year [aweight=Ntot] if year>=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm'_win75 i.year [aweight=Ntot] if year>=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_win75) [aweight=Ntot] if year>=2003, first cluster(idnum)

di _newline(3) " Trimmed: +/- 75% "
	
	* men
	reg lwchange_0 D3`pm'_trim75 i.year [aweight=Ntot] if year>=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm'_trim75 i.year [aweight=Ntot] if year>=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm'_trim75) [aweight=Ntot] if year>=2003, first cluster(idnum)

di _newline(3) " Non-winsorized "

	* men
	reg lwchange_0 D3`pm' i.year [aweight=Ntot] if year>=2003, cluster(idnum)

	* women
	reg lwchange_1 D3`pm' i.year [aweight=Ntot] if year>=2003, cluster(idnum)
	
	* ratio
	ivregress 2sls lwchange_1 i.year (lwchange_0=D3`pm') [aweight=Ntot] if year>=2003, first cluster(idnum)

}


