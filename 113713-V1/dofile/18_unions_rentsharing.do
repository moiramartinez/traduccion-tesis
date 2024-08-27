clear all
cap log close
set linesize 120
set more off, perm
log using ${log}/18_unions_rentsharing.log, replace
adopath ++${prog}
set seed 1000


/*------ Analyse relationship b/w firm-level gwg and union status -------*/

global tau_VA_1_all		"3.25"
global tau_VA_2_all		"3.1"
global tau_VAq_1		"6"
global tau_VAq_2		"6"
global tau_REVq_1		"6"
global tau_REVq_2		"6"

cap program drop prepunion
program define prepunion

use ${data}/worker_dual_`1'_`2' `3', clear

lab def fem 0 "Men" 1 "Women", modify
lab val fem fem

di "Merge union status and existence of works council"
merge m:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(tarif betrrat nkeyvars) nogen

cap drop v
bys betnr year : gen v=_n==1


* Tabulate distribution of union coverage:
	
	su year
	local ymin=r(min)
	local ymax=r(max)
	
	* Distribution of tarif: by gender, person-year weighted
	tab tarif fem, col
	di "year `ymin'"
	tab tarif fem if year==`ymin', col
	di "year `ymax'"
	tab tarif fem if year==`ymax', col
	
	* Distribution of betrrat: by gender, person-year weighted
	tab betrrat fem , col

	* Distribution of tarif: by gender, firm-year weighted
	tab tarif fem if v, col
	di "year `ymin'"
	tab tarif fem if year==`ymin' & v, col
	di "year `ymax'"
	tab tarif fem if year==`ymax' & v, col
	
	* Distribution of tarif: by gender, firm-year weighted
	tab betrrat fem if v , col

	* Joint distribution of betrrat and union coverage
	tab tarif betrrat, row
	tab tarif betrrat if v , row

* Distribution of nkeyvars (0 = no indication for rent-sharing, 5 = maximum indication for rent-sharing)
tab nkeyvars tarif
tab nkeyvars tarif, nofreq row
tab nkeyvars betrrat
tab nkeyvars betrrat, nofreq row

cap drop v


* normalise firm-effects

forval g=0/1 {
	qui sum psi`1'`g' if VA<=${tau_VA_`1'_`2'}	
	di  "Normalising constant for psi`1'`g'	using VA/L: " r(mean)
	gen fe_`1'`g'_nVA=psi`1'`g'-r(mean)
}


fastxtile VAq=VA if VA!=., nq(100)
forval g=0/1 {
	qui sum psi`1'`g' if VAq<=${tau_VAq_`1'}	
	di  "Normalising constant for psi`1'`g'	using VAq: " r(mean)
	gen fe_`1'`g'_nVAq=psi`1'`g'-r(mean)
}


fastxtile REVq=REV if REV!=., nq(100)
forval g=0/1 {
	qui sum psi`1'`g' if REVq<=${tau_REVq_`1'}	
	di  "Normalising constant for psi`1'`g'	using binned REV: " r(mean)
	gen fe_`1'`g'_nREVq=psi`1'`g'-r(mean)
}

gen countall=1
egen n=sum(countall), by(idnum year)
egen femsh=mean(fem), by(idnum year)
gen nf=n*femsh
gen nm=n-nf


* compute excess surplus
gen excVA=max(0,VA-${tau_VA_`1'_`2'}) if VA!=.
lab var excVA "log value added in excess of ${tau_VA_`1'_`2'}"

_pctile VA if VA!=. , p(${tau_VAq_`1'})
gen excVAq=max(0,VA-r(r1)) if VA!=.

_pctile REV if REV!=. , p(${tau_REVq_`1'})
gen excREVq=max(0,REV-r(r1)) if REV!=.
sum VA REV excVA excVAq excREV

* Drop establishment-years without information on coverage
drop if tarif==4 | betrrat==2

* clean up
keep idnum year psi* fe_* VA* REV* exc* femsh n nm nf fem ind fsize bula tarif fbirthyr ln_impwage persnr betrrat nkeyvars

* keep one observation per firm-year
bys idnum year : keep if _n==1

* collapse across firm-years to single observation per firm, each firm weighted the same
collapse 	(firstnm) fe_m=psi`1'0 fe_f=psi`1'1 VA* REV* exc*	///
			fe_m_nVA=fe_`1'0_nVA 		///
			fe_f_nVA=fe_`1'1_nVA  		///
			fe_m_nVAq=fe_`1'0_nVAq  		///
			fe_f_nVAq=fe_`1'1_nVAq  	///
			fe_m_nREV=fe_`1'0_nREV 		///
			fe_f_nREV=fe_`1'1_nREV  		///
			fe_m_nREVq=fe_`1'0_nREVq  		///
			fe_f_nREVq=fe_`1'1_nREVq  	///
			 ind fsize	bula tarif fbirthyr	betrrat	nkeyvars	///
			(sum) persyrs=n mpersyrs=nm fpersyrs=nf, by(idnum)


* additional dummies
gen nocollag=tarif==3
gen firmcollag=tarif==2
gen sectorcollag=tarif==1
gen wrkcouncil=betrrat==1
sum nocollag firmcollag sectorcollag wrkcouncil
lab var nocollag "no coll. agreement"
lab var firmcollag "firm-level coll. agreement"
lab var sectorcollag "sector-level coll. agreement"
lab var wrkcouncil "works council"

* additional controls: firm size, sector, federal state, birthcohort
gen avempl=fsize/1000
gen avempl2=avempl*avempl
di _newline "Industry dummies, omit [1]=Agriculture"
xi, prefix(i) i.ind
di _newline "Construct dummies for federal states, omit [1]=Schleswig-Holstein"
xi, prefix(b) i.bula
gen birthcohort=int((fbirthyr-1975)/3)
*tab birthcohort, m
xi, prefix(fb) i.birthcohort

*cap confirm file ${data}/tmpunion`1'.dta
*if _rc!=0{
save ${data}/tmpunion`1'.dta, replace
*}

* Descriptives for birth cohort:
gen union=tarif<=2
ta fbirthyr birthcohort
table birthcohort, c(mean union mean fe_m mean fe_f freq)
table birthcohort, c(mean fe_m_nVA mean fe_f_nVA freq)
table birthcohort, c(mean union sd fe_m sd fe_f freq)

end 

*######## Regression Models fit by union status ########*

cap program drop unionregs
program define unionregs

use ${data}/tmpunion`1' if `2'!=., clear

if "`2'"=="REVq"{
	di "`2'"
	sum fe_m_n`2' if REV>`3' & REV!=.
	replace fe_m_n`2'=. if fe_m_n`2'>r(mean)
	sum fe_f_n`2' if REV>`3' & REV!=.
	replace fe_f_n`2'=. if fe_f_n`2'>r(mean)
}
else if ("`2'"=="VA" | "`2'"=="VAq") {
	di "`2'"
	sum fe_m_n`2' if VA>`3' & VA!=.
	replace fe_m_n`2'=. if fe_m_n`2'>r(mean)
	sum fe_f_n`2' if VA>`3' & VA!=.
	replace fe_f_n`2'=. if fe_f_n`2'>r(mean)
}


* Table XX: union coverage

gen sectorXcouncil=sectorcollag*wrkcouncil
gen firmXcouncil=firmcollag*wrkcouncil
gen fe_gap_n`2'=fe_m_n`2'-fe_f_n`2'


* Weighted by total number of person-years:
*******************************************
qui {
	eststo fe_m_union : reg fe_m_n`2' sectorcollag firmcollag [aw=persyrs], vce(cluster idnum)
	eststo fe_f_union : reg fe_f_n`2' sectorcollag firmcollag [aw=persyrs], vce(cluster idnum)
	eststo fe_gap_union : reg fe_gap_n`2' sectorcollag firmcollag [aw=persyrs], vce(cluster idnum)			
	*eststo fe_m_council : reg fe_m_n`2' wrkcouncil [aw=persyrs], vce(cluster idnum)
	*eststo fe_f_council : reg fe_f_n`2' wrkcouncil [aw=persyrs], vce(cluster idnum)	
	eststo fe_m_union_birth : reg fe_m_n`2' sectorcollag firmcollag fbbirth* [aw=persyrs], vce(cluster idnum)
	eststo fe_f_union_birth : reg fe_f_n`2' sectorcollag firmcollag fbbirth* [aw=persyrs], vce(cluster idnum)
	eststo fe_gap_union_birth : reg fe_gap_n`2' sectorcollag firmcollag fbbirth* [aw=persyrs], vce(cluster idnum)
	*eststo fe_m_union_raw : reg psi`1'0 sectorcollag firmcollag [aw=persyrs], vce(cluster idnum)
	*eststo fe_f_union_raw : reg psi`1'1 sectorcollag firmcollag [aw=persyrs], vce(cluster idnum)	
	*eststo fe_gap_union_raw : reg psi_gap sectorcollag firmcollag [aw=persyrs], vce(cluster idnum)
	*eststo fe_m_council_raw : reg psi`1'0 wrkcouncil [aw=persyrs], vce(cluster idnum)
	*eststo fe_f_council_raw : reg psi`1'1 wrkcouncil [aw=persyrs], vce(cluster idnum)		
}

esttab fe_m_union fe_f_union fe_gap_union fe_m_union_birth fe_f_union_birth fe_gap_union_birth, ///
	keep(sectorcollag firmcollag ) 		///
	cells(b(star fmt(3))	///
	se(par fmt(3)))			///
	stats(N r2)			///
	collabels(none)			///
	mlabels(none) 			///
	varwidth(35) 			///
	coeflabels(sectorcollag "Sector-level barg." firmcollag "Firm-level barg.") tit(GENDER-POOLED EMPLOYMENT WEIGHTING)
/*

* Weighted by male person-years:
*******************************************
qui {
	eststo fe_m_union : reg fe_m_n`2' sectorcollag firmcollag [aw=mpersyrs], vce(cluster idnum)
	eststo fe_f_union : reg fe_f_n`2' sectorcollag firmcollag [aw=mpersyrs], vce(cluster idnum)
	eststo fe_gap_union : reg fe_gap_n`2' sectorcollag firmcollag [aw=mpersyrs], vce(cluster idnum)			
	*eststo fe_m_council : reg fe_m_n`2' wrkcouncil [aw=mpersyrs], vce(cluster idnum)
	*eststo fe_f_council : reg fe_f_n`2' wrkcouncil [aw=mpersyrs], vce(cluster idnum)	
	eststo fe_m_union_birth : reg fe_m_n`2' sectorcollag firmcollag fbbirth* [aw=mpersyrs], vce(cluster idnum)
	eststo fe_f_union_birth : reg fe_f_n`2' sectorcollag firmcollag fbbirth* [aw=mpersyrs], vce(cluster idnum)
	eststo fe_gap_union_birth : reg fe_gap_n`2' sectorcollag firmcollag fbbirth* [aw=mpersyrs], vce(cluster idnum)
	*eststo fe_m_union_raw : reg psi`1'0 sectorcollag firmcollag [aw=mpersyrs], vce(cluster idnum)
	*eststo fe_f_union_raw : reg psi`1'1 sectorcollag firmcollag [aw=mpersyrs], vce(cluster idnum)	
	*eststo fe_gap_union_raw : reg psi_gap sectorcollag firmcollag [aw=mpersyrs], vce(cluster idnum)
	*eststo fe_m_council_raw : reg psi`1'0 wrkcouncil [aw=mpersyrs], vce(cluster idnum)
	*eststo fe_f_council_raw : reg psi`1'1 wrkcouncil [aw=mpersyrs], vce(cluster idnum)		
}

esttab fe_m_union fe_f_union fe_gap_union fe_m_union_birth fe_f_union_birth fe_gap_union_birth, ///
	keep(sectorcollag firmcollag ) 		///
	cells(b(star fmt(3))	///
	se(par fmt(3)))			///
	stats(N r2)			///
	collabels(none)			///
	mlabels(none) 			///
	varwidth(35) 			///
	coeflabels(sectorcollag "Sector-level barg." firmcollag "Firm-level barg.") tit(MALE EMPLOYMENT WEIGHTING)
	
qui {

* Weighted by female person-years:
*******************************************
	eststo fe_m_union : reg fe_m_n`2' sectorcollag firmcollag [aw=fpersyrs], vce(cluster idnum)
	eststo fe_f_union : reg fe_f_n`2' sectorcollag firmcollag [aw=fpersyrs], vce(cluster idnum)
	eststo fe_gap_union : reg fe_gap_n`2' sectorcollag firmcollag [aw=fpersyrs], vce(cluster idnum)			
	*eststo fe_m_council : reg fe_m_n`2' wrkcouncil [aw=fpersyrs], vce(cluster idnum)
	*eststo fe_f_council : reg fe_f_n`2' wrkcouncil [aw=fpersyrs], vce(cluster idnum)	
	eststo fe_m_union_birth : reg fe_m_n`2' sectorcollag firmcollag fbbirth* [aw=fpersyrs], vce(cluster idnum)
	eststo fe_f_union_birth : reg fe_f_n`2' sectorcollag firmcollag fbbirth* [aw=fpersyrs], vce(cluster idnum)
	eststo fe_gap_union_birth : reg fe_gap_n`2' sectorcollag firmcollag fbbirth* [aw=fpersyrs], vce(cluster idnum)
	*eststo fe_m_union_raw : reg psi`1'0 sectorcollag firmcollag [aw=fpersyrs], vce(cluster idnum)
	*eststo fe_f_union_raw : reg psi`1'1 sectorcollag firmcollag [aw=fpersyrs], vce(cluster idnum)	
	*eststo fe_gap_union_raw : reg psi_gap sectorcollag firmcollag [aw=fpersyrs], vce(cluster idnum)
	*eststo fe_m_council_raw : reg psi`1'0 wrkcouncil [aw=fpersyrs], vce(cluster idnum)
	*eststo fe_f_council_raw : reg psi`1'1 wrkcouncil [aw=fpersyrs], vce(cluster idnum)		
}
*/
end


* for all workers

prepunion 1 all
prepunion 2 all

unionregs 1 VA 10
unionregs 2 VA 10

*unionregs 1 REVq 10
*unionregs 2 REVq 10

unionregs 1 VA 5
unionregs 2 VA 5

*unionregs 1 REVq 6
*unionregs 2 REVq 6






/*############ Decompositions ################*/

cap program drop decompunion
program define decompunion


use ${data}/worker_dual_`1'_all, clear

lab def fem 0 "Men" 1 "Women", modify
lab val fem fem

di "Merge union status and existence of works council"
merge m:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(tarif betrrat nkeyvars) nogen

* normalise firm-effects

forval g=0/1 {
	qui sum psi`1'`g' if VA<=${tau_VA_`1'_`2'}	
	di  "Normalising constant for psi`1'`g'	using VA/L: " r(mean)
	gen fe_`1'`g'_nVA=psi`1'`g'-r(mean)
}


fastxtile VAq=VA if VA!=., nq(100)
forval g=0/1 {
	qui sum psi`1'`g' if VAq<=${tau_VAq_`1'}	
	di  "Normalising constant for psi`1'`g'	using VAq: " r(mean)
	gen fe_`1'`g'_nVAq=psi`1'`g'-r(mean)
}


fastxtile REVq=REV if REV!=., nq(100)
forval g=0/1 {
	qui sum psi`1'`g' if REVq<=${tau_REVq_`1'}	
	di  "Normalising constant for psi`1'`g'	using binned REV: " r(mean)
	gen fe_`1'`g'_nREVq=psi`1'`g'-r(mean)
}

gen countall=1
egen n=sum(countall), by(idnum year)
egen femsh=mean(fem), by(idnum year)
gen nf=n*femsh
gen nm=n-nf


* compute excess surplus
gen excVA=max(0,VA-${tau_VA_`1'_`2'}) if VA!=.
lab var excVA "log value added in excess of ${tau_VA_`1'_`2'}"

_pctile VA if VA!=. , p(${tau_VAq_`1'})
gen excVAq=max(0,VA-r(r1)) if VA!=.

_pctile REV if REV!=. , p(${tau_REVq_`1'})
gen excREVq=max(0,REV-r(r1)) if REV!=.
sum VA REV excVA excVAq excREV

* Drop establishment-years without information on coverage
drop if tarif==4 | betrrat==2


if "`2'"=="REVq"{
di "`2'"
sum fe_`1'0_n`2' if REV>`3' & REV!=.
replace fe_`1'0_n`2'=. if fe_`1'0_n`2'>r(mean)
sum fe_`1'1_n`2' if REV>`3' & REV!=.
replace fe_`1'1_n`2'=. if fe_`1'1_n`2'>r(mean)
}
else if ("`2'"=="VA" | "`2'"=="VAq") {
di "`2'"
sum fe_`1'0_n`2' if VA>`3' & VA!=.
replace fe_`1'0_n`2'=. if fe_`1'0_n`2'>r(mean)
sum fe_`1'1_n`2' if VA>`3' & VA!=.
replace fe_`1'1_n`2'=. if fe_`1'1_n`2'>r(mean)
}



di _newline(2) "Tabulate normalised mean firm effects by status of union coverage: "
table tarif fem , c(mean fe_`1'0_n`2' mean fe_`1'1_n`2') format(%9.4gc)
table betrrat fem , c(mean fe_`1'0_n`2' mean fe_`1'1_n`2') format(%9.4gc)
table nkeyvars fem , c(mean fe_`1'0_n`2' mean fe_`1'1_n`2') format(%9.4gc)

ren ln_impwage lw
qui sum year
local yrmin=r(min)
local yrmax=r(max)


foreach v in tarif betrrat nkeyvars {
preserve
gen dall=1
bys idnum `v' fem: gen dfirms=_n==1
bys persnr `v': gen dworker=_n==1
collapse (mean) lw fe_`1'0_n`2' fe_`1'1_n`2' exc`2' (sum) persyrs=dall workers=dworker firms=dfirms , by(fem `v')
mkmat `v' fem lw fe_`1'0_n`2' fe_`1'1_n`2' exc`2' persyrs workers firms, matrix(decomp)
matlist decomp,		format(%9.4gc)	///	
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Decomposition based on `2' over `v' (last 3 cols show # of person-years, workers, and firms): `yrmin' to `yrmax')
restore
}
end

decompunion 1 VA  10
decompunion 2 VA  10

*decompunion 1 REV 10
*decompunion 2 REV 10

*decompunion 1 VA  5
*decompunion 2 VA  5

*decompunion 1 REV 5
*decompunion 2 REV 5




* Table A.3

cap program drop bysurplus
program define bysurplus

	use ${data}/worker_dual_`1'_all, clear

	* renormalise firm effects
	forval g=0/1 {
		qui sum psi`1'`g' if VA<=${tau_VA_`1'_all}	
		di  "Normalising constant for psi`1'`g'	using VA/L: " r(mean)
		gen fe_`1'`g'_nVA=psi`1'`g'-r(mean)
	}

	* compute excess surplus
	gen excVA=max(0,VA-${tau_VA_`1'_all}) if VA!=.
	lab var excVA "log value added in excess of ${tau_VA_`1'_all}"

	gen low`2'=(`2'<=${tau_`2'_`1'_all})
	gen high`2'=(`2'>${tau_`2'_`1'_all} & `2'<.)
	gen no`2'=`2'==.

	gen group=1*low`2'+2*high`2'+3*no`2'

	xi, prefix(i) i.ind		// omits smallest value
	qui sum ind
	local start=r(min)+1
	local end =r(max)
	forval j=`start'/`end' {
	lab var iind_`j' "`j' `: label (ind) `j''"
	}
	cap drop femshare
	egen femshare=mean(fem), by(betnr)
	bys betnr year: gen dfirmyrs=_n==1
	di _newline "Collapse data to firm-level, persyrs counts # of person-years used to compute firm-level statistics"
	collapse (mean) fsize iind_* group excVA VA REV QR femshare (count) persyrs=persnr (sum) firmyrs=dfirmyrs, by(betnr)
	di _newline "Collapse data to group-level, persyrs counts # of person-years, firmyrs counts # of firm-years, firms counts # of firms"
	gen meancoverage=persyrs/(fsize*firmyrs)
	collapse (mean) fsize iind_* excVA VA REV QR femshare meancoverage (sum) persyrs firmyrs (count) firms=betnr, by(group)
	lab def g 1 "Low `2'/L" 2 "High `2'/L" 3 "Missing `2'/L", modify
	lab val group g
	decode group, gen(group_lb)
	* note that QR might be non-missing even though VA is missing  since it is constructed from vapw
	* and trimmed only with respect to QR and not va (wrt. cutoffs)
	mkmat fsize iind_* femshare excVA VA REV QR meancoverage persyrs firmyrs firms , matrix(by`2') rownames(group_lb)
	mat by`2'=(by`2')'

	matlist by`2',		format(%12.4gc)	///
						lines(oneline)	///
						tw(15)			///
						showcoleq(c)	///
						linesize(120)	///
						underscore		///
						tit(Firm-level descriptives by surplus measure (`2'/L) : `r(min)' to `r(max)')

end

bysurplus 1 VA
bysurplus 2 VA

*bysurplus 1 REV
*bysurplus 2 REV

*bysurplus 1 QR
*bysurplus 2 QR

cap erase ${data}/tmpunion1.dta
cap erase ${data}/tmpunion2.dta
cap erase ${data}/for_decompunion1.dta
cap erase ${data}/for_decompunion2.dta

*/
cap log close






