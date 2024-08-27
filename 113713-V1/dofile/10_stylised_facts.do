clear all
set more off, perm
set linesize 120
cap log close
log using ${log}/10_stylised_facts.log, replace
adopath ++${prog}
set seed 1000

* Set format for regression output
set cformat %9.4f, perm
set pformat %5.4f, perm

* [Part 1]
* Program estimates Mincer-type regressions for the conditional gender gap under different specifications.
* Estimation is performed at the person-year level, parameters on the gender gap are stored in a table that is
* displayed at the end. Parameters and standard deviations are displayed in separate tables.


use ${data}/AKM_select_1.dta if year<=2000, clear
append using ${data}/AKM_select_2.dta


/* Variables for Mincer regressions; specfiied as in Card et al 2013 (adding fem for GWG),
   use experience defined as in BildKorr, treat missing as "no educ" */

gen exper=0
replace exper = age - 16 if educ==0 | educ==1
replace exper = age - 19 if educ==2
replace exper = age - 20 if educ==3
replace exper = age - 25 if educ==4
replace exper = 0 if exper < 0

gen exper2 = exper*exper/100
gen exper3 = exper*exper*exper/1000

gen male=1-fem

global X1 "i.educ exper exper2 exper3"

/* Matrices for regression statistics:
   Cols 1-6: raw GWG and 5 conditional GWG's */
qui levelsof year, l(x)
local num : list sizeof local(x)
mat GWG=J(`num',6,0)
mat SD=J(`num',6,0)

qui sum year
local yrmin=r(min)
local yrmax=r(max)

forval y=`yrmin'/`yrmax' {

	local x=`y'-`yrmin'+1

	di as res _newline "Iteration `x': `y'"

	qui {
		reg ln_impwage male if year==`y'
		mat v=e(V)
		mat GWG[`x',1]=_b[male]
		mat SD[`x',1]=sqrt(v[1,1])
	}
	di "Number of individual observations: " e(N)
	di "Coefficient on female-dummy (raw gender wage gap): " _b[male]

	qui {
		reg ln_impwage male $X1 if year==`y'
		mat v=e(V)
		mat GWG[`x',2]=_b[male]
		mat SD[`x',2]=sqrt(v[1,1])
	}
	di as res _newline "Model 1: male `$X1'"
	di "Number of individual observations: " e(N)
	di "Coefficient on male-dummy: " _b[male]

	qui {
		areg ln_impwage male $X1 if year==`y', absorb(w93_3) vce(cluster w93_3)
		
		mat v=e(V)
		mat GWG[`x',3]=_b[male]
		mat SD[`x',3]=sqrt(v[1,1])
		scalar cells=e(df_a)+1
	}
	di as res _newline "Model 2: male `$X1' i.w93_3"
	di "# of industy-cells: " cells
	di "Number of individual observations: " e(N)
	di "Coefficient on male-dummy: " _b[male]

	qui {
		areg ln_impwage male $X1 if year==`y', absorb(beruf) vce(cluster beruf)
		mat v=e(V)
		mat GWG[`x',4]=_b[male]
		mat SD[`x',4]=sqrt(v[1,1])
		scalar cells=e(df_a)+1
	}
	di as res _newline "Model 3: male `$X1' i.beruf"
	di "# of occupation-cells: " cells
	di "Number of individual observations: " e(N)
	di "Coefficient on male-dummy: " _b[male]


	qui {
		egen indXocc=group(w93_3 beruf)
		areg ln_impwage male $X1 if year==`y', absorb(indXocc) vce(cluster indXocc)
		mat v=e(V)
		mat GWG[`x',5]=_b[male]
		mat SD[`x',5]=sqrt(v[1,1])
		scalar cells=e(df_a)+1
		cap drop indXocc
	}
	di as res _newline "Model 4: male `$X1' i.w93_3#i.beruf"
	di "# of industry-occupation-cells: " cells
	di "Number of individual observations: " e(N)
	di "Coefficient on male-dummy: " _b[male]

	qui {
		areg ln_impwage male $X1 if year==`y', absorb(betnr) vce(cluster betnr)
		mat v=e(V)
		mat GWG[`x',6]=_b[male]
		mat SD[`x',6]=sqrt(v[1,1])
		scalar cells=e(df_a)+1
	}
	di as res _newline "Model 5: male `$X1' i.betnr"
	di "# of establishment-cells: " cells
	di "Number of individual observations: " e(N)
	di "Coefficient on male-dummy: " _b[male]
}
qui levelsof year, l(x)
mat rown GWG =`x'
mat coln GWG = Raw_GWG Model:1 Model:2 Model:3 Model:4 Model:5

di "Display output, number of observations is recorded just before the table via codebook command for person-years, persons, and establishments."
codebook persnr betnr

matlist GWG, 	format(%9.5gc) 	///
				lines(oneline)  ///
				tw(15)  		///
				showcoleq(c)  	///
				linesize(120)  	///
				underscore  	///
				tit(Trends in the Gender Wage Gap for Different Conditioning Sets)

mat rown SD =`x'
mat coln SD = Raw_GWG Model:1 Model:2 Model:3 Model:4 Model:5
matlist SD, 	format(%9.5gc) 	///
				lines(oneline)  ///
				tw(15)  		///
				showcoleq(c)  	///
				linesize(120)  	///
				underscore  	///
				tit(Trends in the Gender Wage Gap for Different Conditioning Sets)


* [Part 2]
* Here, I calculate variance decompositions from models that include establishment-fixed effects, but
* no person-effects. Models are estimated for the gender-pooled sample (allowing for gender-specific covariate returns, but for a single firm effect)
* and separately by gender (allowing additionally for gender-specific firm effects).
			
qui sum year
local yrmin=r(min)
local yrmax=r(max)
local num=`yrmax'-`yrmin'+1
mat V=J(`num',5,0)
mat V_0=J(`num',5,0)
mat V_1=J(`num',5,0)

local x=1
forval yr=`yrmin'/`yrmax' {
	di "`yr'"
	qui areg ln_impwage fem##(i.educ exper c.exper2 c.exper3) if year==`yr', absorb(betnr)
	predict xb if e(sample), xb
	predict psi if e(sample), d
	predict r if e(sample), r
	mat accum sumdev= ln_impwage psi xb r , noc dev
	mat cov = sumdev/(r(N)-1)
	matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore	
	mat V[`x',1]=`yr'
	mat V[`x',2]=cov[1,1]
	mat V[`x',3]=cov[2,2]
	mat V[`x',4]=cov[3,3]
	mat V[`x',5]=cov[4,4]

	forval f=0/1{
		qui areg ln_impwage i.educ exper c.exper2 c.exper3 if year==`yr' & fem==`f', absorb(betnr)
		predict xb_`f' if e(sample), xb
		predict psi_`f' if e(sample), d
		predict r_`f' if e(sample), r
		mat accum sumdev_`f'=ln_impwage psi_`f' xb_`f' r_`f' if fem==`f', noc dev
		mat cov_`f'=sumdev_`f'/(r(N)-1)
		matlist cov_`f', format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore	
		mat V_`f'[`x',1]=`yr'
		mat V_`f'[`x',2]=cov_`f'[1,1]
		mat V_`f'[`x',3]=cov_`f'[2,2]
		mat V_`f'[`x',4]=cov_`f'[3,3]
		mat V_`f'[`x',5]=cov_`f'[4,4]	
	}

	local x=`x'+1
		
	drop psi xb r psi_* xb_* r_*
}

di "Display output, number of observations is recorded just before the table via codebook command for person-years, persons, and establishments."
codebook persnr betnr
matlist V, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore tit(Variance Decomposition, pooled)
matlist V_0, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore tit(Variance Decomposition, men)
matlist V_1, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore tit(Variance Decomposition, women)


* [Part 3]
* Here, I obtain estimates of residual inequality for men and women, calculated as the RMSE from Mincer type regressions as in the first
* part of this dofile.

/*
qui sum year
local yrmin=r(min)
local yrmax=r(max)

forval g=0/1 {

	preserve

	keep if fem==`g'
	mat rmse=J(16,5,0)

	forval y=`yrmin'/`yrmax' {
	
		local x=`y'-`yrmin'+1
		di as res _newline "Iteration `x': `y'"
		qui {
			reg ln_impwage male $X1 if year==`y'
			mat rmse[`x',1]=e(rmse)
		}
		di as res _newline "Model 1: male `$X1'"

		qui {
			areg ln_impwage male $X1 if year==`y', a(w93_3)
			mat rmse[`x',2]=e(rmse)
			scalar cells=e(df_a)+1
		}
		di as res _newline "Model 2: male `$X1' i.w93_3"
		di "# of industry-cells: " cells

		qui {
			areg ln_impwage male $X1 if year==`y', a(beruf)
			mat rmse[`x',3]=e(rmse)
			scalar cells=e(df_a)+1
		}
		di as res _newline "Model 3: male `$X1' i.beruf"
		di "# of occupation-cells: " cells

		qui {
			egen indXocc=group(beruf w93_3)
			areg ln_impwage male $X1 if year==`y', a(indXocc)
			mat rmse[`x',4]=e(rmse)
			scalar cells=e(df_a)+1
			cap drop indXocc
		}
		di as res _newline "Model 4: male `$X1' i.indXocc"
		di "# of industry-occupation-cells: " cells

		qui {
			areg ln_impwage male $X1 if year==`y', a(betnr)
			mat rmse[`x',5]=e(rmse)
			scalar cells=e(df_a)+1
		}
		di as res _newline "Model 5: male `$X1' i.betnr"
		di "# of establishment-cells: " cells
	}
	levelsof year, l(x)
	mat rown rmse=`x'
	mat coln rmse = Model_1 Model_2 Model_3 Model_4 Model_5
	di "Display output, number of observations is recorded just before the table via codebook command for person-years, persons, and establishments."
	codebook persnr betnr
	matlist rmse, 	showcoleq(c) 	///
					linesize(120)	///
					underscore 		///
					cspec(& %5s | %9.4gc & %9.4gc & %9.4gc & %9.4gc & %9.4gc &)	///
					rspec(& - & & & & & & & & & & & & & & & &)	///
					tit(RMSE for Mincer Models; Gender : `g')
	restore
}
*/

* This programme summarises firm surplus measures used in the paper.

cap program drop sd
program define sd

use betnr idnum ind bula year `2' ln_impwage fsize fem using ${data}/AKM_select_`1' if `2'!=., clear
ren ln_impwage lw

* make firm size groups
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

* residual surplus
reg `2' i.fsizegr i.ind i.bula, cluster(idnum)
predict res`2', r

gen lw_m=lw if fem==0
gen lw_f=lw if fem==1
bys betnr: gen f=_n==1
bys persnr: gen p=_n==1

collapse (sd) sd_lw_m=lw_m sd_lw_f=lw_f sd_res`2'=res`2' sd_`2'=`2' (p25) p25_`2'=`2' (p75) p75_`2'=`2' (sum) firms=f workers=p (count) workeryears=fem, by(year)

mkmat year sd_lw_m sd_lw_f sd_res`2' sd_`2' p25_`2' p75_`2' workeryears firms, matrix(sd)

* Display output, last two columns show number of person-years and firms.
matlist sd, format(%9.4gc)

end

/*
sd 1 VA
sd 2 VA

sd 1 lVApw
sd 2 lVApw

sd 1 REV
sd 2 REV

sd 1 lrevpw
sd 2 lrevpw
*/

* The same exercise, but re-compute the mean surplus measures REV and VA using all years (1995-2008).
* By splitting the episodes, the time series exhibit a jump in 2001. This is ok for the period-specific
* analyses, because the trend in firm surplus measures is very similar in both periods, but it looks odd
* in time series plots.

use betnr idnum ind bula year VA* REV* lVApw lrevpw ln_impwage fsize fem using ${data}/AKM_select_1 if year<=2000, clear
append using ${data}/AKM_select_2, keep(betnr idnum ind bula year VA* REV* lVApw lrevpw ln_impwage fsize fem)

* Import cross-section weights
merge m:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(hrf_quer) nogen

ren ln_impwage lw

di _newline "Make profitability measures constant w/n firm interval"
foreach v in lrevpw lVApw {
	di "`v'"
	egen av`v'=mean(`v'), by(betnr)
}

di _newline "Rename profitability variables for analysis..."
ren avlrevpw REV2
ren avlVApw VA2

sum REV REV2 VA VA2, det

di _newline "Trim outliers of VA and REV"
foreach v in VA2 REV2 {

	_pctile `v' if `v'!=., p(95)
	di "Trimming if `v' >= 95% : " r(r1)
	gen `v'trim95=`v' if `v'<=r(r1)
	gen `v'wins95=`v' 
	replace `v'wins95=r(r1) if `v'>=r(r1)

	_pctile `v' if `v'!=., p(1 99)
	di "Trimming if `v' <= 1% : " r(r1) " and >= 99%: " r(r2) 
	gen `v'trim1and99=`v' if `v'>=r(r1) & `v'<=r(r2)
	gen `v'wins1and99=`v' 
	replace `v'wins1and99=r(r1) if `v'<=r(r1)
	replace `v'wins1and99=r(r2) if `v'>=r(r2)

}

gen lw_m=lw if fem==0
gen lw_f=lw if fem==1

bys idnum year: gen f=_n==1

qui {
	preserve
		collapse (sd) sd_lw_m=lw_m sd_lw_f=lw_f (p10) p10_lw_m=lw_m p10_lw_f=lw_f (p25) p25_lw_m=lw_m p25_lw_f=lw_f (p50) p50_lw_m=lw_m p50_lw_f=lw_f (p75) p75_lw_m=lw_m p75_lw_f=lw_f (p90) p90_lw_m=lw_m p90_lw_f=lw_f (count) np=fem (rawsum) nf=f, by(year) fast
		mkmat year sd_lw_m p10_lw_m p25_lw_m p50_lw_m p75_lw_m p90_lw_m sd_lw_f p10_lw_f p25_lw_f p50_lw_f p75_lw_f p90_lw_f np nf, matrix(lwage)
	restore
	preserve
		collapse (sd) sd_lVApw=lVApw sd_VA2=VA2 (p10) p10_lVApw=lVApw p10_VA2=VA2 (p25) p25_lVApw=lVApw p25_VA2=VA2 (p50) p50_lVApw=lVApw p50_VA2=VA2 (p75) p75_lVApw=lVApw p75_VA2=VA2 (p90) p90_lVApw=lVApw p90_VA2=VA2 (count) np=fem (rawsum) nf=f, by(year) fast
			mkmat year sd_lVApw p10_lVApw p25_lVApw p50_lVApw p75_lVApw p90_lVApw np nf, matrix(lVApw)
			mkmat year sd_VA2 p10_VA2 p25_VA2 p50_VA2 p75_VA2 p90_VA2 np nf, matrix(VA2)
	restore
	preserve
		collapse (sd) sd_VA2trim95=VA2trim95 sd_VA2wins95=VA2wins95 (p10) p10_VA2trim95=VA2trim95 p10_VA2wins95=VA2wins95 (p25) p25_VA2trim95=VA2trim95 p25_VA2wins95=VA2wins95 (p50) p50_VA2trim95=VA2trim95  p50_VA2wins95=VA2wins95 (p75) p75_VA2trim95=VA2trim95 p75_VA2wins95=VA2wins95 (p90) p90_VA2trim95=VA2trim95 p90_VA2wins95=VA2wins95 (count) np=fem (rawsum) nf=f, by(year) fast
		mkmat year sd_VA2trim95 p10_VA2trim95 p25_VA2trim95 p50_VA2trim95 p75_VA2trim95 p90_VA2trim95 np nf, matrix(VA2trim95)
		mkmat year sd_VA2wins95 p10_VA2wins95 p25_VA2wins95 p50_VA2wins95 p75_VA2wins95 p90_VA2wins95 np nf, matrix(VA2wins95)
	restore
	preserve
		collapse (sd) sd_VA2trim1and99=VA2trim1and99 sd_VA2wins1and99=VA2wins1and99 (p10) p10_VA2trim1and99=VA2trim1and99 p10_VA2wins1and99=VA2wins1and99 (p25) p25_VA2trim1and99=VA2trim1and99 p25_VA2wins1and99=VA2wins1and99 (p50) p50_VA2trim1and99=VA2trim1and99 p50_VA2wins1and99=VA2wins1and99 (p75) p75_VA2trim1and99=VA2trim1and99 p75_VA2wins1and99=VA2wins1and99 (p90) p90_VA2trim1and99=VA2trim1and99 p90_VA2wins1and99=VA2wins1and99 (count) np=fem (rawsum) nf=f, by(year) fast
		mkmat year sd_VA2trim1and99 p10_VA2trim1and99 p25_VA2trim1and99 p50_VA2trim1and99 p75_VA2trim1and99 p90_VA2trim1and99 np nf, matrix(VA2trim1and99)
		mkmat year sd_VA2wins1and99 p10_VA2wins1and99 p25_VA2wins1and99 p50_VA2wins1and99 p75_VA2wins1and99 p90_VA2wins1and99 np nf, matrix(VA2wins1and99)
	restore
	preserve
		collapse (sd) sd_lrevpw=lrevpw sd_REV2=REV2 (p10) p10_lrevpw=lrevpw p10_REV2=REV2 (p25) p25_lrevpw=lrevpw p25_REV2=REV2 (p50) p50_lrevpw=lrevpw p50_REV2=REV2 (p75) p75_lrevpw=lrevpw p75_REV2=REV2 (p90) p90_lrevpw=lrevpw p90_REV2=REV2 (count) np=fem (rawsum) nf=f, by(year) fast
			mkmat year sd_lrevpw p10_lrevpw p25_lrevpw p50_lrevpw p75_lrevpw p90_lrevpw np nf, matrix(lrevpw)
			mkmat year sd_REV2 p10_REV2 p25_REV2 p50_REV2 p75_REV2 p90_REV2 np nf, matrix(REV2)
	restore
	preserve
		collapse (sd) sd_REV2trim95=REV2trim95 sd_REV2wins95=REV2wins95 (p10) p10_REV2trim95=REV2trim95 p10_REV2wins95=REV2wins95 (p25) p25_REV2trim95=REV2trim95 p25_REV2wins95=REV2wins95 (p50) p50_REV2trim95=REV2trim95  p50_REV2wins95=REV2wins95 (p75) p75_REV2trim95=REV2trim95 p75_REV2wins95=REV2wins95 (p90) p90_REV2trim95=REV2trim95 p90_REV2wins95=REV2wins95 (count) np=fem (rawsum) nf=f, by(year) fast
		mkmat year sd_REV2trim95 p10_REV2trim95 p25_REV2trim95 p50_REV2trim95 p75_REV2trim95 p90_REV2trim95 np nf, matrix(REV2trim95)
		mkmat year sd_REV2wins95 p10_REV2wins95 p25_REV2wins95 p50_REV2wins95 p75_REV2wins95 p90_REV2wins95 np nf, matrix(REV2wins95)
	restore
	preserve
		collapse (sd) sd_REV2trim1and99=REV2trim1and99 sd_REV2wins1and99=REV2wins1and99 (p10) p10_REV2trim1and99=REV2trim1and99 p10_REV2wins1and99=REV2wins1and99 (p25) p25_REV2trim1and99=REV2trim1and99 p25_REV2wins1and99=REV2wins1and99 (p50) p50_REV2trim1and99=REV2trim1and99 p50_REV2wins1and99=REV2wins1and99 (p75) p75_REV2trim1and99=REV2trim1and99 p75_REV2wins1and99=REV2wins1and99 (p90) p90_REV2trim1and99=REV2trim1and99 p90_REV2wins1and99=REV2wins1and99 (count) np=fem (rawsum) nf=f, by(year) fast
		mkmat year sd_REV2trim1and99 p10_REV2trim1and99 p25_REV2trim1and99 p50_REV2trim1and99 p75_REV2trim1and99 p90_REV2trim1and99 np nf, matrix(REV2trim1and99)
		mkmat year sd_REV2wins1and99 p10_REV2wins1and99 p25_REV2wins1and99 p50_REV2wins1and99 p75_REV2wins1and99 p90_REV2wins1and99 np nf, matrix(REV2wins1and99)
	restore
}
	
di "Display output, number of persons and establishments for each year are displayed in last two columns"
	matlist lwage, format(%9.4gc) tit(Distribution of current log value added, person-year weighted)
	matlist lVApw, format(%9.4gc) tit(Distribution of current log value added, person-year weighted)
	matlist VA2, format(%9.4gc) tit(Distribution of mean log value added, person-year weighted)
	matlist VA2trim95, format(%9.4gc) tit(Distribution of mean log value added, trimmed at 95 percent, person-year weighted)
	matlist VA2wins95, format(%9.4gc) tit(Distribution of mean log value added, winsorized at 95 percent, person-year weighted)
	matlist VA2trim1and99, format(%9.4gc) tit(Distribution of mean log value added, trimmed at 1 and 99 percent, person-year weighted)
	matlist VA2wins1and99, format(%9.4gc) tit(Distribution of mean log value added, winsorized at 1 and 99 percent, person-year weighted)
	matlist lrevpw, format(%9.4gc) tit(Distribution of current log sales, person-year weighted)
	matlist REV2, format(%9.4gc) tit(Distribution of mean log sales, person-year weighted)
	matlist REV2trim95, format(%9.4gc) tit(Distribution of mean log sales, trimmed at 95 percent, person-year weighted)
	matlist REV2wins95, format(%9.4gc) tit(Distribution of mean log sales, winsorized at 95 percent, person-year weighted)
	matlist REV2trim1and99, format(%9.4gc) tit(Distribution of mean log sales, trimmed at 1 and 99 percent, person-year weighted)
	matlist REV2wins1and99, format(%9.4gc) tit(Distribution of mean log sales, winsorized at 1 and 99 percent, person-year weighted)


preserve
	collapse (sd) sd_lw_m=lw_m sd_lw_f=lw_f sd_lVApw=lVApw sd_VA=VA sd_VA2=VA2 sd_lrevpw=lrevpw sd_REV=REV sd_REV2=REV2 ///
			(p10) p10_lw_m=lw_m p10_lw_f=lw_f p10_lVApw=lVApw p10_VA2=VA2 p10_lrevpw=lrevpw p10_REV2=REV2 ///
			(p25) p25_lw_m=lw_m p25_lw_f=lw_f p25_lVApw=lVApw p25_VA2=VA2 p25_lrevpw=lrevpw p25_REV2=REV2 ///
			(p50) p50_lw_m=lw_m p50_lw_f=lw_f p50_lVApw=lVApw p50_VA2=VA2 p50_lrevpw=lrevpw p50_REV2=REV2 ///
			(p75) p75_lw_m=lw_m p75_lw_f=lw_f p75_lVApw=lVApw p75_VA2=VA2 p75_lrevpw=lrevpw p75_REV2=REV2 ///
			(p90) p90_lw_m=lw_m p90_lw_f=lw_f p90_lVApw=lVApw p90_VA2=VA2 p90_lrevpw=lrevpw p90_REV2=REV2 ///
			(count) np=fem (rawsum) nf=f [aw=hrf_quer], by(year) fast
			
	mkmat year sd_lw_m p10_lw_m p25_lw_m p50_lw_m p75_lw_m p90_lw_m sd_lw_f p10_lw_f p25_lw_f p50_lw_f p75_lw_f p90_lw_f np nf, matrix(lwage)
	mkmat year sd_lVApw p10_lVApw p25_lVApw p50_lVApw p75_lVApw p90_lVApw, matrix(lVApw)
	mkmat year sd_VA2 p10_VA2 p25_VA2 p50_VA2 p75_VA2 p90_VA2, matrix(VA2)
	mkmat year sd_lrevpw p10_lrevpw p25_lrevpw p50_lrevpw p75_lrevpw p90_lrevpw, matrix(lrevpw)
	mkmat year sd_REV2 p10_REV2 p25_REV2 p50_REV2 p75_REV2 p90_REV2, matrix(REV2)

di "Display output, number of persons and establishments for each year are displayed in last two columns"
	matlist lwage, format(%9.4gc) tit(Distribution of current log value added, cross-section weighted)
	matlist lVApw, format(%9.4gc) tit(Distribution of current log value added, cross-section weighted)
	matlist VA2, format(%9.4gc) tit(Distribution of mean log value added, cross-section weighted)
	matlist lrevpw, format(%9.4gc) tit(Distribution of current log sales, cross-section weighted)
	matlist REV2, format(%9.4gc) tit(Distribution of mean log sales, cross-section weighted)
restore








