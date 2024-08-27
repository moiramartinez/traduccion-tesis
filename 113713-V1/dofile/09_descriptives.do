clear all
set linesize 120
cap log close
set more off, perm
log using ${log}/09_descriptives.log, replace


* This program generates summary statistics and the dual-connected worker sample for the decomposition analysis.
* Unit of observation is the person-year. No aggregation is performed unless stated otherwise.

* Prepare variables for descriptives, merge AKM parameters, generate summary statistics.

cap program drop summarise
program define summarise

use ${data}/AKM_select_`1'.dta,clear
qui sum year
local yrmin=r(min)
local yrmax=r(max)
*gen runi=uniform()
*keep if runi>.95

* Age group dummies
gen agec1=age<25
gen agec2=(age>=25 & age<35)
gen agec3=(age>=35 & age<45)
gen agec4=(age>=45 & age<55)
gen agec5=age>=55

* Make age categorical
gen ageg5=agec1+2*agec2+3*agec3+4*agec4+5*agec5
lab def ageg5 1 "Age < 25" 2 "25 <= Age < 35" 3 "35 <= Age < 45" 4 "45 <= Age < 55" 5 "55 <= Age < 60"
lab val ageg5 ageg5

* Education dummies
tab educ , g(d)

* Approximate years of schooling (taken from CHK) *
gen school=.
replace school=10.5 if educ==0 	// missing, 0
replace school=11   if educ==1	// dropouts/primary
replace school=13	if educ==2	// apprentice
replace school=15	if educ==3	// some college
replace school=18	if educ==4	// university

* Recode isco
recode isco (1 2 =1) (8 9 =8)
tab isco, g(occ)

* Modal occupation (7, ISCO-88, combined)
egen moccup=mode(isco), by(persnr) maxmode
di _newline "Checking off-diagonal elements"
tab isco moccup, mis

* Female employment share, indicators for allmale/allfemale firms
egen femshare=mean(fem), by(betnr)
gen allmale=(femshare==0)
gen allfemale=(femshare==1)

* Import estimated person effects (from AKMs, see matlab code)
merge m:1 persnr using ${data}/peffs_`1'.dta, keep(1 3) keepus(theta`1') nogenerate
gen connected=theta`1'!=.

* Import estimated firm effects (from AKMs, see matlab code); 2 effects per firm, 1 for each gender; make indicator for dual-connected set
merge m:1 betnr using ${data}/feffs_`1'.dta, keep(1 3) keepus(psi`1'0 psi`1'1) nogenerate
gen dualconnect=(psi`1'0!=. & psi`1'1!=.)

* Correlation between male and female firm effects
corr psi`1'1 psi`1'0
corr psi`1'1 psi`1'0 if dualconnect==1
reg psi`1'1 psi`1'0 if dualconnect==1, cluster(betnr)

di _newline(2) "OBSERVATIONS FOR DIFFERENT ANALYSIS SAMPLES"
di _newline "Persons, Establishments (all IDs), EP-Establishments (IDs associated with IAB-BP)"
di _newline(2) "Overall worker sample"
bys persnr: gen p=_n==1
bys betnr: gen f=_n==1
bys idnum: gen f2=_n==1
total(p f f2), cformat(%9.0gc)
drop p f f2

di _newline(2) "Largest connected set (for AKMs)"
preserve
keep if connected==1
bys persnr: gen p=_n==1
bys betnr: gen f=_n==1
bys idnum: gen f2=_n==1
total(p f f2), cformat(%9.0gc)
restore

di _newline(2) "Dual-connected set (for decompositions)"
preserve
keep if dualconnect==1
bys persnr: gen p=_n==1
bys betnr: gen f=_n==1
bys idnum: gen f2=_n==1
total(p f f2), cformat(%9.0gc)
restore

di _newline(2) "IAB Establishment panel (for normalisation)"
preserve
keep if idnum!=.
bys persnr: gen p=_n==1
bys betnr: gen f=_n==1
bys idnum: gen f2=_n==1
total(p f f2), cformat(%9.0gc)
restore

* Firm size distribution
preserve

	cap drop jahr
	gen jahr=year
	merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis.dta, keepus(az_ges_vz) nogen update keep(3)
	drop jahr
	di _newline "Collapse to firm-year level"
	collapse (firstnm) az_ges_vz dualconnect idnum (count) worker=fem, by(betnr year) fast

	bys betnr (year) : gen avfsize=(az_ges_vz+az_ges_vz[_n-1])/2
	bys betnr (year) : replace avfsize=az_ges_vz if _n==1

	* define firm size groups based on admin data
	gen dec_ft=1 if avfsize<=5
	replace dec_ft=2 if avfsize>5 & avfsize<=10
	replace dec_ft=3 if avfsize>10 & avfsize<=20
	replace dec_ft=4 if avfsize>20 & avfsize<=50
	replace dec_ft=5 if avfsize>50 & avfsize<=100
	replace dec_ft=6 if avfsize>100 & avfsize<=250
	replace dec_ft=7 if avfsize>250 & avfsize<=500
	replace dec_ft=8 if avfsize>500 & avfsize<=1000
	replace dec_ft=9 if avfsize>1000 & avfsize<.
	lab def dec_ft 1 "1-5" 2 "6-10" 3 "11-20" 4 "21-50" 5 "51-100" 6 "101-250" 7 "251-500" 8 "501-1000" 9 "1001+"
	lab val dec_ft dec_ft

	* define firm size groups based on observed data
	gen dec_n=1 if worker<=5
	replace dec_n=2 if worker>5 & worker<=10
	replace dec_n=3 if worker>10 & worker<=20
	replace dec_n=4 if worker>20 & worker<=50
	replace dec_n=5 if worker>50 & worker<=100
	replace dec_n=6 if worker>100 & worker<=250
	replace dec_n=7 if worker>250 & worker<=500
	replace dec_n=8 if worker>500 & worker<=1000
	replace dec_n=9 if worker>1000 & worker<.
	lab def dec_n 1 "1-5" 2 "6-10" 3 "11-20" 4 "21-50" 5 "51-100" 6 "101-250" 7 "251-500" 8 "501-1000" 9 "1001+"
	lab val dec_n dec_n
	
	di _newline(2) "Joint dist of reported fte and observed fte"
	di _newline "largest connected set"
	ta dec_n dec_ft
	di _newline "dual-connected set"
	ta dec_n dec_ft if dualconnect==1
	di _newline "establishment panel firms"
	ta dec_n dec_ft if idnum!=.

	di _newline(2) "display firmsize & workers (# of firm-years in last column, # of persons in second-to-last column)"
	table dec_ft , c(mean avfsize mean worker freq) format(%9.2gc)

	di _newline(2) "display firmsize & workers (# of firm-years in last column, # of persons in second-to-last column)"
	table dec_ft if dualconnect==1, c(mean avfsize mean worker freq) format(%9.2gc)

	di _newline(2) "display firmsize & workers for establishment panel (# of firm-years in last column, # of persons in second-to-last column)"
	table dec_ft if idnum!=., c(mean avfsize mean worker freq) format(%9.2gc)

restore


/*
* Duncan & Duncan Dissimilarity Index (last column contains # of workers, second-to-last column contains number of firms)
di "across establishments, full sample"
duncan2 betnr fem
bys year : duncan2 betnr fem

di "across 3-digit occupations, full sample"
duncan2 beruf fem
bys year : duncan2 beruf fem

di "across 3-digit industries, full sample"
duncan2 w93_3 fem
bys year : duncan2 w93_3 fem

di "across establishments, dual-connected"
duncan2 betnr fem if dualconnect==1
bys year : duncan2 betnr fem if dualconnect==1

di "across 3-digit occupations, dual-connected"
duncan2 beruf fem if dualconnect==1
bys year : duncan2 beruf fem if dualconnect==1

di "across 3-digit industries, dual-connected"
duncan2 w93_3 fem if dualconnect==1
bys year : duncan2 w93_3 fem if dualconnect==1
*/

* Import gründungsjahr from bhp
gen jahr=year
merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis, keep(1 3) keepus(grd_jahr) nogen
egen fbirthyr=min(grd_jahr), by(betnr)

* Create time-invariant vars for regression of worker-/firm-effects
gen birthcohort=int((fbirthyr-1975)/3)
tab birthcohort, m

preserve
	di "Make sic2-wage"
	keep ln_impwage betnr year sic2 fsize year femshare
	gen dailywage = exp(ln_impwage)
	bys betnr year : gen first=_n==1
	egen wagebill=mean(dailywage*365) if first, by(betnr year)
	gen fshare=femshare if first
	gen avfsize=fsize if first
	di _newline "Collapse to industry-year (2-digit) level"
	collapse (mean) sic2wage=wagebill sic2fem=fshare (rawsum) sic2empl=avfsize (count) firms=avfsize [aw=avfsize], by(sic2 year)
	* from now on, unit of observation: industry-year
	gen sic2fsize=sic2empl/firms
	save ${data}/sic2wage_`1', replace
restore

di _newline "Import to main file"
merge m:1 sic2 year using ${data}/sic2wage_`1'.dta, keep(1 3) nogen keepus(sic2wage)
cap erase ${data}/sic2wage_`1'.dta

cap drop QRpw
gen QRpw=vapw/100-sic2wage/100000		/* follows Gürtzgen */
qui sum year
local yrmin=r(min)
local yrmax=r(max)

di _newline "Trim quasi-rent (constructed from non-trimmed va); use same trimming points (<1 and >99)"
forval y=`yrmin'/`yrmax' {
	qui sum QRpw if year==`y', detail
	replace QRpw=. if (QRpw<=r(p1) | QRpw>=r(p99)) & year==`y'
}

di _newline "Make QR constant w/n firm interval"
egen avQRpw=mean(QRpw), by(betnr)
di _newline "Rename profitability variables for analysis..."
ren avQRpw QR

di "Firms with low VA may have negative QR (subtract industrial wage)"
qui sum sic2wage
scalar minsic2=r(min)
scalar maxsic2=r(max)
qui sum VA
di "max VA : " r(max)
di "min VA : " r(min)
replace QR=. if (QR>exp(r(max))/100-minsic2/100000 & QR<.) | QR<exp(r(min))/100-maxsic2/100000
sum VA REV QR QRpw 

* Prepare tables with summary statistics

foreach sub in allmales allfemales cmales cfemales dualmales dualfemales vamales vafemales EPmales EPfemales {

	preserve
		
		if "`sub'"=="allmales" 		keep if fem==0
		if "`sub'"=="allfemales" 	keep if fem==1
		if "`sub'"=="cmales" 		keep if fem==0 & connected==1
		if "`sub'"=="cfemales" 		keep if fem==1 & connected==1
		if "`sub'"=="dualmales" 	keep if fem==0 & dualconnect==1
		if "`sub'"=="dualfemales" 	keep if fem==1 & dualconnect==1
		if "`sub'"=="vamales" 		keep if fem==0 & VA!=.
		if "`sub'"=="vafemales" 	keep if fem==1 & VA!=.
		if "`sub'"=="EPmales" 		keep if fem==0 & idnum!=.
		if "`sub'"=="EPfemales" 	keep if fem==1 & idnum!=.
		
		di _newline(2) "Summary stats for : `sub'"
		qui mean(age agec1 agec2 agec3 agec4 agec5 tenure school d1 d2 d3 d4 d5 ln_impwage fsize femshare allmale allfemale)
		mat `sub'=(e(b)'\ e(N))

		* Get counts of workers, firms, job-spells
		bys persnr : gen p=_n==1
		bys betnr : gen b=_n==1
		bys betnr persnr : gen s=_n==1
		qui total(p b s)
		mat `sub'=(`sub' \ _b[p] \ _b[b] \ _b[s])
		di _newline(2) "Summarise firm surplus (trimmed) separately + log wage for standard deviation"
		sum VA REV ln_impwage
		svmat `sub'
		keep `sub'1
		keep if `sub'1!=.
		save ${data}/`sub'.dta, replace

	restore

}

di "Save for analysis worker; only dual-connected"
preserve
	keep if dualconnect==1
	lab data "worker all dual-connected"
	save ${data}/worker_dual_`1'_all.dta, replace
restore

end

forval j=1/2 {

	summarise `j'
	
	qui{
		use ${data}/allmales.dta, clear
		merge 1:1 _n using ${data}/allfemales.dta, nogen
		merge 1:1 _n using ${data}/cmales.dta, nogen
		merge 1:1 _n using ${data}/cfemales.dta, nogen
		merge 1:1 _n using ${data}/dualmales.dta, nogen
		merge 1:1 _n using ${data}/dualfemales.dta, nogen
		merge 1:1 _n using ${data}/vamales.dta, nogen
		merge 1:1 _n using ${data}/vafemales.dta, nogen
		merge 1:1 _n using ${data}/EPmales.dta, nogen
		merge 1:1 _n using ${data}/EPfemales.dta, nogen
		mkmat _all, matrix(sumstats_int`j')
		mat rown sumstats_int`j'=	Age:Mean(Age) Age:Age<25 Age:25<=Age<35 Age:35<=Age<45 Age:45<=Age<55 Age:55<=Age ///
									Tenure Education:Mean_schooling Education:Missing Education:Primary Education:Apprentices Education:Some_college Education:University ///
									Mean_ln(wage) Mean_Firm_Size Share_Females Share_All_Male Share_All_Female ///
									Observations:#_of_person-yrs Observations:#_of_workers Observations:#_of_firms Observations:#_of_job-spells
	}

}


/* Display summary statistics, last 4 rows contain number of person-years, persons, firms, and job-matches */

matlist sumstats_int1, 	format(%11.4gc) 	///
							lines(oneline) 		///
							tw(20)  			///
							showcoleq(c)  		///
							underscore  tit(Summary statistics for 1995-2001)	

matlist sumstats_int2, 	format(%11.4gc) 	///
							lines(oneline) 		///
							tw(20)  			///
							showcoleq(c)  		///
							underscore  tit(Summary statistics for 2001-2008)
						




* This program constructs tables with industrial mean wage bills, female shares, and the industrial share of total employment;
* data is aggregated to the level of 2-digit industries; the first table shows the top 10 industries (ranked by average annual wage bill),
* and the second table shows the bottom 10 industries (ranked by average annual wage bill).


cap program drop sicwage
program define sicwage

use ${data}/sic2wage_`1', clear

summarize
codebook sic2

di _newline "Collapse to industry-level, unweighted average across years, get aggregate employment to compute overall mean employment share of industry"
collapse (mean) avsic2wage=sic2wage avsic2fem=sic2fem avsic2fsize=sic2fsize (sum) sic2empl , by(sic2)

* from now on, unit of observation: industry
qui sum sic2empl
gen avsic2emplsh=sic2empl/r(sum)*100
gen avsic2femsh=avsic2fem*100
di _newline "Summary statistics of 2-digit industries, all years"
summarize
codebook sic2
di _newline "Rank sic2 by mean wage, get top and bottom 10"
gsort -avsic2wage
gen top10=_n if _n<=10
mkmat top10 sic2 avsic2wage avsic2fsize avsic2femsh avsic2emplsh, matrix(top) nomissing
sort avsic2wage
gen bot10=_n if _n<=10
gsort -avsic2wage
mkmat bot10 sic2 avsic2wage avsic2fsize avsic2femsh avsic2emplsh, matrix(bot) nomissing

matlist top, 	showcoleq(c) 	///
				linesize(120)	///
				underscore 		///
				cspec(& %5s | %9.0fc & %9.0fc & %9.0fc & %9.0fc & %9.1fc & %9.2fc &)			///
				rspec(& - & & & & & & & & & & )			///
				tit(Employment-size weighted means of annual wage bills, firm size, and firm-level female employment shares ranked by mean industry wage (top 10).)


matlist bot, 	showcoleq(c) 	///
				linesize(120)	///
				underscore 		///
				cspec(& %5s | %9.0fc & %9.0fc & %9.0fc & %9.0fc & %9.1fc & %9.2fc &)			///
				rspec(& - & & & & & & & & & & )			///
				tit(Employment-size weighted means of annual wage bills, firm size, and firm-level female employment shares ranked by mean industry wage (bottom 10).)

end

sicwage 1
sicwage 2


/*--- Get table with firm size distribution ---*/

cap program drop firmsize
program define firmsize
use ${data}/worker_dual_`1'_`2'.dta, clear
*gen runi=uniform()
*keep if runi<=.03
cap drop first

/* Construct 10 deciles based on # of firms, then add firm size, workers, and py-observations for each group. */
qui {
bys betnr : gen first=_n==1
egen rk_fsize=rank(fsize) if first, u
count if first
gen tmp_dec=ceil(10*rk_fsize/r(N))
di as res _newline "Expand ranks to all py-observations..."
egen fsize_dec=max(tmp_dec), by(betnr)
egen ptag_by_dec=tag(persnr fsize_dec)		/* Same workers may be observed in several deciles */
tab fsize_dec first, matcell(freq)
mean(fsize) if first, over(fsize_dec)
mat mfsize=e(b)'
total(ptag_by_dec), over(fsize_dec)
mat pobs=e(b)'
mat firmdist = (mfsize, freq[1..10,1]+freq[1..10,2], freq[1..10,2], pobs)
qui levelsof fsize_dec, l(x)
mat rown firmdist = `x'
mat coln firmdist = Mean_Employment #_of_person-years #_of_firms #_of_workers
sum year
}
matlist firmdist, 	format(%13.0fc)	///
					lines(oneline) 	///
					tw(5) 			///
					showcoleq(c) 	///
					linesize(120) 	///
					underscore		///
					tit(Establishment Distribution: `r(min)' to `r(max)')
end

*firmsize 1 all
*firmsize 2 all


/**************************\
	Wage Distribution
\**************************/

cap program drop wagedist
program define wagedist

use ${data}/fordescr_`1', clear
fastxtile pct_m=ln_impwage if fem==0, nq(100)
fastxtile pct_f=ln_impwage if fem==1, nq(100)
egen pct=rowtotal(pct_m pct_f)
bys pct betnr fem : gen firms=_n==1
bys pct persnr : gen worker=_n==1

* collapse to percentiles; Npers stores number of persons, Nfirm stores number of firms.
collapse (mean) lw=ln_impwage fe_m=psi`1'0 fe_f=psi`1'1 pe_m=theta`1' pe_f=theta`1' (sum) Npers=worker Nfirm=firms, by(fem pct)
mkmat fem pct lw fe_m pe_m Npers Nfirm if fem==0, matrix(male)
mkmat fem pct lw fe_f pe_f Npers Nfirm if fem==1, matrix(female)
matlist male
matlist female

end

*wagedist 1
*wagedist 2



* Summarise mobility and PAM by number of movers

di _newline(2) "Estimate the correlation of worker- and firm-effects for different values of movers per firm"
di "Andrews et al. (2008) show that as # of movers per firm increases, "
di "firm-effects are estimated with greater precision and the corr. becomes positive"


cap program drop PAMbymovers
program define PAMbymovers
	
	* Worker data
	use ${data}/AKM_select_`1' if fem==`2',clear

	* Import estimated person effects (from AKMs, see matlab code)
	merge m:1 persnr using ${data}/peffs_`1'.dta, keep(1 3) keepus(theta`1') nogenerate
	gen connected=theta`1'!=.

	* Import estimated firm effects (from AKMs, see matlab code); 2 effects per firm, 1 for each gender; make indicator for dual-connected set
	merge m:1 betnr using ${data}/feffs_`1'.dta, keep(1 3) keepus(psi`1'0 psi`1'1) nogenerate
	gen dualconnect=(psi`1'0!=. & psi`1'1!=.)

	* Use only largest connected sets
	keep if connected==1
	su
	
	preserve
		* Import firm size and calcualte mean values of t-1 and t.
		gen jahr=year
		merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis.dta, keepus(az_ges_vz) nogen update keep(3)
		drop jahr
		di _newline "Collapse to firm-year level"
		
		collapse (firstnm) az_ges_vz dualconnect idnum (count) worker=fem, by(betnr year) fast

		bys betnr (year) : gen avfsize=(az_ges_vz+az_ges_vz[_n-1])/2
		bys betnr (year) : replace avfsize=az_ges_vz if _n==1
		collapse (sum) cumavfsize=avfsize, by(betnr) fast
		su cumavfsize, det
	save ${data}/firmsize, replace
	restore
	
	* Person-firm matches used for identification of fixed effects in a given time interval
	* (note: here, recalls dont matter. I dont care about the number of moves, only about the number of movers.
	keep betnr persnr idnum dualconnect psi`1'0 psi`1'1 theta`1' year
	egen match=tag(persnr betnr)
	egen matchespw=sum(match), by(persnr)
	gen m=matchespw>1
	egen M=sum(m), by(betnr)
	gen Mgroup=1 if M<5
	replace Mgroup=2 if M>=5 & M<10
	replace Mgroup=3 if M>=10 & M<15
	replace Mgroup=4 if M>=15 & M<20
	replace Mgroup=5 if M>=20 & M<25
	replace Mgroup=6 if M>=25 & M<.
	
	bys betnr : gen j=_n==1
	di _newline(2) "Number of firms by category of number of movers :"
	di _newline "Largest connected set"
	ta Mgroup if j==1
	di _newline "Dual connected set"
	ta Mgroup if j==1 & dualconnect
	di _newline "Establihsment Panel"
	ta Mgroup if j==1 & idnum!=.	

	collapse (firstnm) idnum psi`1'0 psi`1'1 theta`1' dualconnect (count) n=dualconnect , by(persnr betnr) fast
	
	egen matchespw = count(betnr), by(persnr)
	gen m=matchespw>1
	egen M=sum(m), by(betnr)		/*number of movers a firms employs in a time interval*/
	bys betnr : gen f=_n==1
	
	gen Mgroup=1 if M<5
	replace Mgroup=2 if M>=5 & M<10
	replace Mgroup=3 if M>=10 & M<15
	replace Mgroup=4 if M>=15 & M<20
	replace Mgroup=5 if M>=20 & M<25
	replace Mgroup=6 if M>=25 & M<.
	
	di _newline "job spells in firms by number number of movers: check if dual-connected set is already assocuated with more moves"
	ta Mgroup dualconnect
	di _newline "firm by number number of movers: check if firms in dual-connected set tend to have more movers"
	ta Mgroup dualconnect if f==1
	
	qui corr psi`1'`2' theta`1' [fw=n]
	scalar all=r(rho)
	qui corr psi`1'`2' theta`1' [fw=n] if dualconnect
	scalar dual=r(rho)
	qui corr psi`1'`2' theta`1' [fw=n] if Mgroup==1
	scalar lessthan4=r(rho)
	qui corr psi`1'`2' theta`1' [fw=n] if Mgroup==2
	scalar morethan5=r(rho)
	qui corr psi`1'`2' theta`1' [fw=n] if Mgroup==3
	scalar morethan10=r(rho)
	qui corr psi`1'`2' theta`1' [fw=n] if Mgroup==4
	scalar morethan15=r(rho)
	qui corr psi`1'`2' theta`1' [fw=n] if Mgroup==5
	scalar morethan20=r(rho)
	qui corr psi`1'`2' theta`1' [fw=n] if Mgroup==6
	scalar morethan25=r(rho)
	mat corr=(all, dual, lessthan4, morethan5, morethan10, morethan15, morethan20, morethan25)'
	mat rown corr= All Dual-connected <=4_movers >=5_movers >=10_movers >=15_movers >=20_movers >=25_movers
	matlist corr, 	format(%11.4gc) 	///
					lines(oneline) 		///
					tw(20)  			///
					showcoleq(c)  		///
					underscore 			///
					tit(Correlation of person- and firm-effects : gender `2', period `1')
					

	* Collapse to firm level
	collapse (firstnm) Mgroup M dualconnect idnum (sum) py=n (count) p=dualconnect, by(betnr) fast
					
	* Import mean firm size, averaged across years in the data
	* Note: for some firms, the mean nb. of full-time employees is missing
	merge 1:1 betnr using ${data}/firmsize, nogen keep(3)
	su cumavfsize
	gen pmean=py/cumavfsize
	
	lab var p "connected individuals per estab"
	lab var py "number of person-years per estab"
	lab var pmean "observed share of total fte"

	* Construct aggregate N_star, N, M, J and M/J
	di _newline "Largest connected group"
	gen largestset=1
	gen dualconnected=1 if dualconnect==1
	gen ep =1 if idnum!=.
	table largestset, c(mean pmean sum py sum p sum M freq) format(%13.3gc)
	table dualconnected, c(mean pmean sum py sum p sum M freq) format(%13.3gc)
	table ep, c(mean pmean sum py sum p sum M freq) format(%13.3gc)
	
	
	
end

PAMbymovers 1 0
PAMbymovers 1 1
PAMbymovers 2 0
PAMbymovers 2 1

cap log close
***CENSORED***exit




