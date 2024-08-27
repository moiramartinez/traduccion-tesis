clear all
cap log close
set linesize 120
set more off, perm
cap program drop _all
log using ${log}/22_analyse_childbirth.log, replace
adopath ++${prog}
set seed 1000
set matsize 11000


use betnr psi10 psi11 using ${data}/worker_dual_1_all,clear
bys betnr: keep if _n==1
save ${data}/feffs_dual, replace
use betnr psi20 psi21 using ${data}/worker_dual_2_all,clear
bys betnr: keep if _n==1
merge 1:1 betnr using ${data}/feffs_dual, nogen
su psi*
save ${data}/feffs_dual, replace



/* This program conducts an "event study" to examine how the first major work interruption of women
impacts on the gender wage gap in firm premiums over the lifecycle, and how this contributes to 
the total wage gap between men and women. */

* Select which algorithm is used to identify childbirth
global type="2" 		// 1: Uta Schönberg, 2: Dana Mueller

* Setting bootstrap loops
global bsobs=1000

* Models to be estimated (1: estimate / 0: do not estimate)

global m_baseline = 0
global m_fixedeffects = 0
global m_levels = 0

global m_baseline_X = 0
global m_fixedeffects_X = 0
global m_levels_X = 1

global m_supplementary = 0
global s_meanwages = 0

global age_decomposition = 0

* Age range for which first major work interruption is assumed to be caused by childbirth

global minage_birth = 18
global maxage_birth = 40
global first_birth_cohort = 1993-40
global last_birth_cohort = 2008-40
global kmin = -5
global kmax = 13

global ageX "i.age"
global yearX "i.year"
global educX ""

* Create file with persnr, year, and parental leave status from raw data (incl. entire employment biographies). 
* Parental leave is indicated by employer upon deregistration (grund=51 & grund=52)

	* grund = 52: Employment interruption notification due to parental leave (men should hardly be observed here, though there is a large increase in 2007 due to the reform from "Erziehungsgeld" zu "Elterngeld")
	* grund = 51: Employment interruption notification due to entitlement to other compensation [used for parental leave if employee was in "Schutzfrist" during pregnancy]

* I identify parental leave spells in the raw data, collapse to the person-year and match the result back to the
* main analysis file.


	cap confirm f ${orig}\liab_mm2_9308_v1_pers.dta

	if _rc==0 use ${orig}\liab_mm2_9308_v1_pers, clear
	if _rc>0 use ${orig}\liab_mm_9308_v1_pers, clear
	
	* Import BHP data
	merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis.dta, keepus(ao_kreis) nogen

	gen year = year(begepi)
	gen age = year-gebjahr
	gen byte west=0
	replace west=1 if ao_kreis <=11100
	
	* Only Beh and LeH spells [XASU und XLHG spells do not contain relevant information on potential childbirth]
	keep if quelle<3
	
	* Only women:
	keep if frau==1
		
* Gaps in employment biographies [reconstruct "btyp"-variable as in IABS]
	
	sort persnr spell
	bys persnr (spell): gen gap = begepi-endepi[_n-1]
	replace gap = 0 if gap==.	/*gap might be negative*/
	su gap, det
	
	* Impose restrictions following Data Report IABS-R (http://doku.iab.de/fdz/reporte/2006/DR_01-06.pdf):
	
	* Gap is larger than 7 days [never 1st spell by condition gap=0 if gap==.]
		bys persnr (spell): gen min7days = 1 if gap>7
	
	* Last spell describes...
	* ... employment subject to SSC [here I use the definition that corresponds to erwstat=3 in SIAB-SUF]...
		bys persnr (spell) : gen nonmarg = 1 if ((erwstat!=109 | erwstat!=209) & quelle==1)
		
	* ... with non-missing work district information ...	
		bys persnr (spell) : gen regionexists = 1 if !missing(ao_kreis)
	
	* ... indicates employment (no deregistration) ...	
		bys persnr (spell) : gen empl = 1 if (inrange(grund,50,53) & quelle==1)
		
	* And next spell is BeH-spell
		bys persnr (spell) : gen next_BeH = 1 if quelle[_n+1]==1
	
	* Not the last spell of Konto [indirectly accounted for by "next spell" condition]
		bys persnr (spell) : gen notlastspell = 1 if _n<_N
	
	* Expand
		expand 2 if min7days==1 & nonmarg==1 & regionexists==1 & empl==1 & next_BeH==1 & notlastspell==1, gen(exp)
	
	sort persnr spell exp
	replace tentgelt=0 if exp==1
	replace grund=0 if exp==1
	bys persnr (spell): replace begepi=endepi[_n-1]+1 if exp==1
	bys persnr (spell): replace endepi=begepi[_n+1]-1 if exp==1
	*br persnr betnr begorig endorig begepi endepi grund ao_kreis stib tentgelt quelle erwstat gap exp
	drop min7days nonmarg regionexists empl next_BeH notlastspell
	replace year = year(begepi)
	
* Define btyp as in IABS; btyp 2, 4, 5, 6 mark generated values [see Uta Schönberg, 2009; p. 54)
* [Note: I was unable to completely rebuild the original IABS variable. In particular, it is not
* clear how the last category is defined. Also, the original btyp variable does not close all gaps,
* and I am not sure why.
* For my purpose, this is not a big issue because I only want to identify the gaps, and be able
* to exclude category 2, which I am.
	gen btypnew = 1 if quelle==1 & tentgelt>0 & !missing(tentgelt) & grund>0
	bys persnr (spell exp) : replace btypnew = 7 if quelle==2 & grund>0
	replace btypnew = 3 if quelle==1 & tentgelt==0 & grund>0 & !missing(grund)
	
	bys persnr (spell exp) : replace btypnew = 2 if (grund[_n-1]!=34 & grund[_n-1]!=35 & grund[_n-1]!=51 & grund[_n-1]!=52 & grund[_n-1]!=53) & begepi==mdy(1,1,year) & endepi==mdy(12,31,year) & grund==0	
	bys persnr (spell exp) : replace btypnew = 4 if (grund[_n-1]==34 | grund[_n-1]==35 | grund[_n-1]==51 | grund[_n-1]==52 | grund[_n-1]==53) & betnr==betnr[_n+1] & !missing(betnr) & btypnew!=2 & grund==0
	bys persnr (spell exp) : replace btypnew = 5 if (grund[_n-1]==34 | grund[_n-1]==35 | grund[_n-1]==51 | grund[_n-1]==52 | grund[_n-1]==53) & betnr!=betnr[_n+1] & !missing(betnr) & !missing(betnr[_n+1]) & btypnew!=2 & grund==0
	bys persnr (spell exp) : replace btypnew = 6 if btypnew==.

	* Distribution before episode splitting
	ta grund btypnew, mis
	
* Some gaps exceed 31.12 => require Episodensplitting
	
	* Identify generated observations that lap over 31.12.
	gen lapoveryear = 1 if year(begepi)<year(endepi) & exp==1
	gen num=year(endepi)-year(begepi)+1
	su num
	replace num=0 if num<0
	expand num+1, gen(exp2)
	sort persnr spell begepi exp2
	*br persnr spell quelle_gr begepi endepi grund erwstat_gr betnr exp lapoveryear num
	*bys persnr spell begepi exp2 : gen N=_N		/*max # of generated observations*/
	drop if exp2==1 /*drop generated observations*/
	
	su num 
	cap drop exp2
	local nmin=r(min)+1
	local nmax=r(max)
	gen begepi2=begepi 
	gen endepi2=endepi
	format begepi2 endepi2 %td
	
	ren exp gen0
	
	forval Y = 1(1)`nmax'{
		local X = `Y'-1
		cap drop lapoveryear 
		gen lapoveryear = 1 if year(begepi2)<year(endepi) & gen`X'==1	/*which generated spells lap over one year?*/
		expand 2 if lapoveryear==1, gen(gen`Y')
		sort persnr spell begepi2 gen`Y'
		bys persnr (spell begepi2 gen`Y'): replace endepi2=mdy(12,31,year(begepi2)) if lapoveryear==1 & gen`Y'==0 
		bys persnr (spell begepi2 gen`Y'): replace begepi2=endepi2[_n-1]+1 if lapoveryear==1 & gen`Y'==1
		bys persnr (spell begepi2 gen`Y'): replace endepi2=min(begepi[_n+1]-1, mdy(12,31,year(begepi2))) if lapoveryear==1 & gen`Y'==1
	}
	
	drop begepi endepi
	ren begepi2 begepi
	ren endepi2 endepi
	egen exp = rowtotal(gen*)
	bys persnr (spell exp): replace spell=_n
	drop gen*
	drop exp
		
	* Distribution after episode splitting
	ta grund btypnew, mis
	
* Proximate identification of child birth leaves
	
	* Identify leave spells (crude)
		gen leave=0 if year>=1995 & year<=2008
		replace leave=1 if (inrange(btypnew,2,6) & leave==0)
		bys persnr (spell) : gen leavei = 1 if leave==1 & leave[_n-1]==0
		bys persnr (spell) : gen leavespell = sum(leavei) if leave==1

	* Duration of spells in months
		egen mindate=min(begepi) if leave==1, by(persnr leavespell)
		egen maxdate=max(endepi) if leave==1, by(persnr leavespell)
		format mindate maxdate %td
		gen ayear = year(mindate)
		gen amonth = month(mindate)
		gen eyear = year(maxdate)
		gen emonth = month(maxdate)
		gen durmonth = eyear*12+emonth - (ayear*12+amonth)
		drop ayear amonth eyear emonth
		
	* Mark leave spells starting on 1st of month (unlikely to be childbirth, see Ludsteck & Schönberg)
		gen day=day(begepi)
	
	* Women, 18-35, min duration 2 months [as in Schönberg (2009)]
	bys persnr (spell) : gen leave_clean=1 if leave==1 & age>=18 & age<=35 & durmonth>2 & day>1

	* Keep only relevant spells	
	keep if leavei==1
	keep if west==1
	keep persnr year mindate maxdate leavei leavespell durmonth dur leave_clean btypnew age
	duplicates report persnr year						/*there are duplicate reports if a person has more than one leave spell in a year*/
	duplicates report persnr year if leavespell==1		/*find first leave spell of a person account*/
	
* Summary [compare with Uta Schönbergs Methodenreport]
		
	* Number of leave spells per year
		*all
		ta year if leave_clean==1 
		* only first leave spell
		ta year if leave_clean==1 & leavespell==1
	
	* Length of leave spells
	
		* all
		ta durmonth if leave_clean==1 & durmonth<=70
		* only first leave spells
		ta durmonth if leave_clean==1 & durmonth<=70 & leavespell==1
	
	* Leave spells by birth month
		gen birthmonth = month(mindate) if leavei==1
		
		* all 
		ta birthmonth if leave_clean==1
		* only first leave spells
		ta birthmonth if leave_clean==1 & leavespell==1
		* exclude btypnew==2 
		ta birthmonth if leave_clean==1 & leavespell==1 & btypnew!=2
	
* Distribution of leave spells by age
	gen durmonthgroup=1 if durmonth==1
	replace durmonthgroup=2 if durmonth==2
	replace durmonthgroup=3 if durmonth==3
	replace durmonthgroup=4 if durmonth==4
	replace durmonthgroup=5 if durmonth>=5 & durmonth<=6
	replace durmonthgroup=6 if durmonth>=7 & durmonth<=8
	replace durmonthgroup=7 if durmonth>=9 & durmonth<=11
	replace durmonthgroup=8 if durmonth==12
	replace durmonthgroup=9 if durmonth>12 & durmonth<.
	gen agegroup =1 if (age>=16 & age<=20)
	replace agegroup =2 if (age>=21 & age<=25)
	replace agegroup =3 if (age>=26 & age<=30)
	replace agegroup =4 if (age>=31 & age<=35)
	replace agegroup =5 if (age>=36 & age<=40)
	replace agegroup =6 if (age>=41 & age<=45)
	
	* all
	ta durmonthgroup agegroup if leave_clean==1
	* only first leave spell
	ta durmonthgroup agegroup if leave_clean==1 & leavespell==1
	* exclude btypnew==2
	ta durmonthgroup agegroup if leave_clean==1 & leavespell==1 & btypnew!=2
	
* Restrict to first birth spell with 2+ months labour market leave
keep if leavespell==1 & leave_clean==1
rename leave_clean childbirth

* Re-build person-year panel starting from "age at birth of first child"
duplicates report persnr
keep persnr year age durmonth birthmonth childbirth
rename age ageatbirth
rename year yearofbirth
expand 14
sort persnr
bys persnr: gen year = 1994+_n
bys persnr: gen age = year-(yearofbirth-ageatbirth)
gen k = year-yearofbirth
*replace childbirth=0 if yearofbirth!=year
*replace durmonth=0 if yearofbirth!=year
*replace birthmonth=0 if yearofbirth!=year
gen evermother=1
drop if age<20|age>60

compress
save ${data}/childbirth1, replace
cap erase ${data}/childbirth.dta

* Estimate AKMs for 1995-2008 separately for men and women
*cap erase ${data}/AKM_fe_allyears.dta
capture confirm f ${data}/AKM_fe_allyears.dta

/*
if _rc>0{

	forval g=0/1{

		* Load analysis file and rebuild panel for 1995-2008
			use persnr betnr year age fem educ ln_impwage using ${data}/AKM_select_1 if year<=2000, clear
			append using ${data}/AKM_select_2, keep(persnr betnr year age fem educ ln_impwage)
			
		* Restrict to gender
			keep if fem==`g'
			
		* Find largest connected set in pooled sample
			qui {
				egen id=group(persnr)
				egen firmid=group(betnr)

				di "Run adapted grouping algorithm... Require at least 0.99 of person-years to be allocated (possibly to multiple groups)."
				grouping2 group, i(id) j(firmid) lowbound(0.99)

				bys group : gen gs=_N
				sum gs
				mat coverage=(r(max)\r(N)\ 100*r(max)/r(N))
			}
			matlist coverage, tit(Largest connected set)

		* Restrict to largest mobility group
			sum gs
			replace group=(gs==r(max))
			keep if group==1
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
			sum theta psi xb r
			bys fem : sum theta psi xb r

		* Variance Decomposition
			mat accum sumdev= ln_impwage theta psi xb r , noc dev
			mat cov = sumdev/(r(N)-1)
			mat corr = corr(sumdev)
			matlist cov, format(%9.4gc) lines(oneline) tw(5) showcoleq(c) linesize(120) underscore		

		* Save
			keep persnr betnr year fem ln_impwage theta psi xb r
			compress
			if `g'==0 {
				save ${data}/AKM_fe_allyears, replace
			}
			if `g'==1 {
				append using ${data}/AKM_fe_allyears		/*Note: I have not generated female FEs for males and vice versa*/
				save ${data}/AKM_fe_allyears, replace
			}
	}
}
*/

* Load analysis file and rebuild panel for 1995-2008
	use persnr betnr idnum year beruf w93_3 age fem educ fsize tage_bet ln_impwage using ${data}/AKM_select_1 if year<=2000, clear
	append using ${data}/AKM_select_2, keep(persnr betnr idnum year beruf w93_3 age fem educ fsize tage_bet ln_impwage)

* Import maternity leave status
	merge 1:1 persnr year using ${data}/childbirth${type}, keep(1 3) nogen keepus(yearofbirth ageatbirth durmonth childbirth birthmonth k evermother)		// some women are mothers but I dont see their childbirth anymore after imposing the restriction to full-time, 20-60, 1995-2008
	replace evermother=0 if evermother==.
	replace k=0 if k==.
	egen tmpk=min(k) if k>=0, by(persnr)
	gen retfulltime=(k==tmpk & k>=0)
	drop tmpk
	
	lab var childbirth "leave spell due to child birth"
	lab var durmonth "duration of leave spell [months]"
	lab var yearofbirth "calender year of birth of first child"
	lab var ageatbirth "age of mother at birth of first child"
	lab var k "years relative to birth of first child"
	lab var evermother "identified as mother in admin data"

* Import AKM-effects
	capture confirm f ${data}/feffs_periodpooled.dta
	if _rc>0{
		gen psi0 = uniform()*0.5
		gen psi1 = uniform()*0.5
		bys betnr (year): replace psi0 = psi0[1]
		bys betnr (year): replace psi1 = psi1[1]
		gen theta = uniform()+3.5
		bys persnr (year): replace theta = theta[1]
		gen xb = uniform()*0.25
		gen r = 0
		replace r = ln_impwage - psi0 - theta - xb if fem==0
		replace r = ln_impwage - psi1 - theta - xb if fem==1
		drop xb r
	}
	if _rc==0 {
		merge m:1 betnr using ${data}/feffs_periodpooled, keep(1 3) keepus(psi1 psi0) nogen
		bys fem : su psi0 psi1
	}

* ADDED 1/2/2018 (on request of Ilyana Kuziemko, RR #2)
* Import AKM-effects (gender-specific, by period)
	merge m:1 betnr using ${data}/feffs_dual, keep(1 3) keepus(psi10 psi11 psi20 psi21) nogenerate
	gen interval=1 if year>=1995 & year<=2001
	replace interval=2 if year>=2001 & year<=2008
	bys fem interval: su psi10 psi11 psi20 psi21
	cap erase ${data}/feffs_dual.dta
	
* Restrict to dual-connected set in pooled sample (all years)
	keep if psi0!=.& psi1!=.
	
* Rename AKM-effects
	rename tage_bet tenure
	rename ln_impwage lw

* Rescale AKM-effects to mean zero [to make comparable across gender]
	sum psi0 if fem==0
	gen fe_male = psi0 - r(mean)
	sum psi1 if fem==1
	gen fe_female = psi1 - r(mean)
	
	* ADDED 1/2/2018 (on request of Ilyana Kuziemko, RR #2)
	sum psi10 if interval==1 & fem==0
	gen fe_male_90s = psi10 - r(mean)
	sum psi20 if interval==2 & fem==0
	gen fe_male_00s = psi20 - r(mean)
	sum psi11 if interval==1 & fem==1
	gen fe_female_90s = psi11 - r(mean)
	sum psi10 if interval==2 & fem==1
	gen fe_female_00s = psi21 - r(mean)

	drop psi? psi??
	bys fem interval: su fe_male* fe_female*	
	
* Industry- and occupation codes
	gen sic2_code = int(w93_3/10)
	gen occ2_code = int(beruf/10)
	gen sic1_code = int(w93_3/100)
	gen occ1_code = int(beruf/100)
	drop w93_3 beruf

	
* Table minimum and maximum duration of leave spells by year since birth
* [check that maximum does not exceed 12*k]	
	*egen durmonth_ = max(durmonth), by(persnr)
	table k if k>=$kmin, c(min durmonth max durmonth)
	table k if k>=$kmin & retfulltime==1, c(min durmonth max durmonth)

qui compress
cap erase ${data}/temp_lc1.dta


**************************************    
* DD-Analysis with Placebo Treatment *
**************************************

* Follow the event study construction as in Kleven et al. (2017); see Appendix A.1 for details.

* STEP 1: define control group (those who never have children)
* Truncation issue: women may give birth between 18 and 35 years old, hence women born after 1973 may become
* first time mothers (and belong to treatment group) despite them not being observed turning a mother during
* the sample period.
* >> drop them from control group.
	
	* Import region information from BHP
	ren year jahr
	merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis.dta, keepus(ao_kreis) nogen
	ren jahr year
	
	* Restrict to women
	keep if fem==1

	* Relevant age range: 40 by 2008 (1995-40=1955 and 2008-40=1968)
	* Women must be at most 40 when giving birth to their first child.
	gen bjahr = year-age	
	gen notrunc = (bjahr>=$first_birth_cohort & bjahr<=$last_birth_cohort)
	
	* Covariates for linear probability model

		* Quartile of wage distribution of cohort
		gen lw4=.
		forval c=$first_birth_cohort(1)1989{
			capture{
				fastxtile lw4_ = lw if bjahr==`c', nq(4)
				replace lw4 = lw4_ if bjahr==`c'
				drop lw4_
			}
		}
			
		* Quartile of AKM firm effect distribution
		gen fe_female4=.
		forval c=1960(1)1989{
			capture {
				fastxtile fe_female4_ = fe_female if bjahr==`c', nq(4)
				replace fe_female4 = fe_female4_ if bjahr==`c'
				drop fe_female4_ 
			}
		}
		*gen fe_female4 = int((psi-1)/5)

	* Estimate LPM (quartile of wage distr, quartile of AKM distr, 5 educ groups, district dummies) for 
	* non-truncated cohorts (born between 1995-40=1955 and 2008-40=1968)
	gen nevermother = 1-evermother
	qui reg nevermother i.lw4 i.fe_female4 i.educ i.ao_kreis if notrunc==1, ro

	* Out-of-sample prediction for women born after 1973 (not constant at person-level!!!)
	predict xbhat
	egen mxbhat = mean(xbhat) if xbhat!=., by(persnr)

	* Share of nevermothers in non-truncated birth cohorts
	egen avneverm_notrunc_=mean(nevermother) if notrunc==1
	egen avneverm_notrunc=max(avneverm_notrunc_)

	* Pick n such that the fraction of nevermothers in birth cohorts 1968+ is
	* equal to the fraction of nevermothers in the 1955-1967 birth cohorts, conditional
	* on that person being a nevermother by 2008 (i.e., every woman who is a mother by
	* then is not considered here).
	* Put differently: add to the control group those women who are most unlikely to get
	* a child in each cohort.
	preserve
		drop if (notrunc==1 | mxbhat==. /*w/o valid obs*/| nevermother==0 /*and ever mother*/)
		gsort bjahr -mxbhat year
		by bjahr : gen n=_n
		by bjahr : gen N=_N
		keep if n/N<=avneverm_notrunc
		gen pcontrolimp=1
		drop n N avneverm_notrunc* xbhat
		save ${data}/imputed_nevermothers, replace
	restore
	
	* Re-import
	merge 1:1 persnr year using ${data}/imputed_nevermothers, nogen keepus(pcontrolimp) /*pcontrolimp does not flag complete biographies because there is missing information in some of the variables used to generate xbhat*/
	cap erase ${data}/imputed_nevermothers.dta
	replace notrunc=pcontrolimp if pcontrolimp==1
	gen impflag=(pcontrolimp==1)
	drop pcontrolimp

	* Consistency check: control group includes women without children from birthcohorts 1955-1968, and
	* some women without children from birthcohorts >1968
	gen controlgroup=notrunc if nevermother==1
	replace controlgroup=0 if nevermother==0
	ta bjahr controlgroup, mis
	ta bjahr controlgroup if impflag==1, mis
	drop notrunc


* STEP 2: Assign placebo births to control group

	* For birthcohort 1955-1968: Approximate "age at first child" by log normal distribution within 
	* birthcohort and education. Use mean/variance from actual distribution.
	
	su childbirth evermother if evermother==1
	replace childbirth=0 if age!=ageatbirth
	su childbirth evermother if childbirth==1
	codebook persnr if childbirth==1
	codebook persnr if evermother==1
	egen tmpmother=max(childbirth), by(persnr)
	ta evermother tmpmother
	ta evermother tmpmother if childbirth==1
	ta childbirth evermother
	ta childbirth tmpmother
	ta year childbirth if evermother==0
	ta year childbirth if evermother==1
	ta year childbirth if tmpmother==0
	ta year childbirth if tmpmother==1	
	drop tmpmother
	tab year childbirth
	tab year childbirth, nofreq row
	
	* Mean age at first child of all women who ever become mothers (both those who have done in the past, and who will in the future => conditional on birth being observed in the data)
	table year if evermother==1, c(mean ageatbirth sd ageatbirth min ageatbirth max ageatbirth) format(%9.2fc)
	
	* Mean age at first child of mothers giving birth in a certain year
	table year if childbirth==1, c(mean ageatbirth sd ageatbirth min ageatbirth max ageatbirth) format(%9.2fc)
	
	gen bjahrz= int((bjahr-1900)/10)
	ta bjahr bjahrz, mis
	gen randageat1stbirth=.
	gen lnageatbirth=ln(ageatbirth)
	
	* Random draw of "age at first birth" from lognormal distribution
	* NOTE: since I only look at actual mothers here, the last birth cohort decade is $last_birth_cohort. 
	
	forval d=3(1)6	{
		forval e=0(1)4	{
			su lnageatbirth if educ==`e' & bjahrz==`d' & childbirth==1 & retfulltime==1 & bjahr<=$last_birth_cohort
			global sd=r(sd)
			global m=r(mean)
			bys persnr (year) : replace randageat1stbirth=exp($m + ${sd}*invnorm(uniform())) if controlgroup==1 & _n==1 & impflag==0 & educ==`e' & bjahrz==`d' & exp($m + ${sd}*invnorm(uniform()))!=.
		}
	}
	gen ageatbirth2 = floor(randageat1stbirth)
	bys persnr (year): replace ageatbirth2 = ageatbirth2[1] if controlgroup==1 & impflag==0
	drop randageat1stbirth
	replace ageatbirth2=. if ageatbirth2<$minage_birth | ageatbirth2>$maxage_birth
	
	* a) non-truncated birth cohorts (simple case)
	* Assign nevermothers a synthetic birth (note: predicted age at birth might be in a year where I dont observe a person)
	gen childbirth2 = 1 if ageatbirth2==age & ageatbirth2>=$minage_birth & ageatbirth2<=$maxage_birth & controlgroup==1 & (bjahr<=$last_birth_cohort & impflag==0)
	gen yearofbirth2_ = year-age+ageatbirth2
	gen k2 = year-yearofbirth2

	* b) for truncated birth cohorts (complicated case)
	* Draw random "age at first child" using the same variance as above, but instead of actual mean, 
	* use predicted mean from a linear-quadratic trend on non-truncated birth cohorts to allow for women
	* becoming their first child at an older age, as seen in the data.
	gen t=year-1994
	gen t2=t*t
	gen ageatbirth_hat=.
	
	forval e=0(1)4 {
	
		* Regress ageatbirth on linear-quadratic time trend for each education group and store predicted value
		*reg ageatbirth t t2 if controlgroup==0 & evermother==1 & retfulltime==1
		reg ageatbirth t t2 if controlgroup==0 & childbirth==1 & retfulltime==1 & educ==`e'
		predict xb_, xb
		replace ageatbirth_hat=xb_ if educ==`e'
		drop xb_
	
	}

	reg ageatbirth t t2 if controlgroup==0 & childbirth==1 & retfulltime==1
	predict ageatbirth_hat2, xb
	
	su ageatbirth ageatbirth_hat ageatbirth_hat2

	* log age at birth
	gen lnageatbirth_hat=ln(ageatbirth_hat)
	drop t t2 ageatbirth_hat2

	* summary statistics of age at birth
	table bjahr if childbirth==1, c(mean ageatbirth mean ageatbirth_hat) format(%9.2fc)
	table year if childbirth==1, c(mean ageatbirth mean ageatbirth_hat) format(%9.2fc)
	
	* Random draw of "age at first birth" from lognormal distribution
	* NOTE: here, the last birth cohort may be as late as 2008-18 (the first year, when a woman might become a mother)
	gen randageat1stbirth=.
	forval d=6(1)9	{
		forval e=0(1)4	{
			di "education = `e', jahrzent = `d'"
			su lnageatbirth_hat if educ==`e' & bjahrz==`d' & controlgroup==1 & bjahr>$last_birth_cohort & impflag==1	 /*allow for variable mean*/
			global m=r(mean)
			* I fix the variance at the level of the birth cohort (1960-1968); in the Appendix of Kleven et al., it only says that the variance
			* is constant, but with the notation used, this is not possible, since the birth cohort decade (index d) changes for the later birth
			* cohorts. It is not feasible to hold the variance constant at the birth cohort level in 1980 if there is no birth cohort of 1980 in the
			* non-truncated sample. I instead assume that it is constant (for each education group) at the level of the last non-truncated decade (1960s).
			su lnageatbirth if educ==`e' & bjahrz==6 & childbirth==1 & retfulltime==1 & bjahr<=$last_birth_cohort
			global sd=r(sd)
			bys persnr (year) : replace randageat1stbirth=exp($m + ${sd}*invnorm(uniform())) if controlgroup==1 & _n==1 & impflag==1 & educ==`e' & bjahrz==`d' & exp($m + ${sd}*invnorm(uniform()))!=.
		}
	}

	gen ageatbirth3 = floor(randageat1stbirth)
	bys persnr (year): replace ageatbirth3 = ageatbirth3[1] if controlgroup==1 & impflag==1
	drop randageat1stbirth
	replace ageatbirth3=. if ageatbirth3<$minage_birth | ageatbirth3>$maxage_birth
	
	* Assign nevermothers in old cohorts a synthetic birth
	gen childbirth3 = 1 if ageatbirth3==age & ageatbirth3>=$minage_birth & ageatbirth3<=$maxage_birth & controlgroup==1 & (bjahr>$last_birth_cohort & impflag==1)
	gen yearofbirth3_ = year-age+ageatbirth3
	gen k3 = year-yearofbirth3_
	
	*su ageatbirth ageatbirth2 ageatbirth3 if controlgroup==1 & impflag==1
	*su ageatbirth ageatbirth2 ageatbirth3 if controlgroup==1 & impflag==0
	replace ageatbirth=ageatbirth2 if ageatbirth2!=.
	replace ageatbirth=ageatbirth3 if ageatbirth3!=.
	replace k = k2 if k2!=.
	replace k = k3 if k3!=.
	replace childbirth=childbirth2 if childbirth2!=.
	replace childbirth=childbirth3 if childbirth3!=.
	
	drop ageatbirth? yearofbirth?_ k? childbirth? avneverm* xbhat mxbhat 

	* Keep only mothers or women who were successfully assigned a placebo birth
	drop if ageatbirth==.

* DD-style regressions comparing women who gave birth between 1995 and 2008 to women who were 
* assigned a placebo birth.

	* Check consistency: no overlap of actual lmothers and control group?
	ta evermother controlgroup, mis
	ta childbirth controlgroup, mis
	
	*ADDED 1/2/2018 in request of Ilyana Kuziemko RR #2.
	* Cross-tabulations
	gen fe_allyrs = 1 if fe_female!=.
	lab var fe_allyrs "women with period-pooled female fe's"
	count if fe_allyrs==. //should report 0
	gen fe_90s = 1 if fe_female_90s!=.
	lab var fe_90s "women with female fe's in 1990s"
	gen fe_00s = 1 if fe_female_00s!=.
	lab var fe_00s "women with female fe's in 2000s"
	
	*Overlap
	ta fe_90s fe_00s, mis
	*Overlap for mothers
	ta fe_90s fe_00s if controlgroup==0, mis
	*Overlap for non-mothers
	ta fe_90s fe_00s if controlgroup==1, mis
	
	codebook persnr if controlgroup==1
	codebook persnr if controlgroup==0
	codebook persnr if controlgroup==1 & fe_90s==1
	codebook persnr if controlgroup==0 & fe_90s==1
	codebook persnr if controlgroup==1 & fe_00s==1
	codebook persnr if controlgroup==0 & fe_00s==1
	
	codebook persnr if childbirth==1
	codebook persnr if childbirth==0
	codebook persnr if childbirth==1 & fe_90s==1
	codebook persnr if childbirth==0 & fe_90s==1
	codebook persnr if childbirth==1 & fe_00s==1
	codebook persnr if childbirth==0 & fe_00s==1
	
	codebook persnr if evermother==1
	codebook persnr if evermother==0
	codebook persnr if evermother==1 & fe_90s==1
	codebook persnr if evermother==0 & fe_90s==1
	codebook persnr if evermother==1 & fe_00s==1
	codebook persnr if evermother==0 & fe_00s==1
	drop fe_90s fe_00s
		
	
	* Keep treated (evermothers) and constructed control observations
	*keep if (evermother==1 | controlgroup==1)
	
	* Mean age at birth in treatment and control group
	gen g3=1 if evermother==1
	replace g3=2 if evermother==0 & controlgroup==1 & impflag==0
	replace g3=3 if evermother==0 & controlgroup==1 & impflag==1
	lab def g3 1 "actual mother" 2 "actual nevermother" 3 "imputed nevermother"
	lab val g3 g3
	table g3, c(mean ageatbirth)

	* Recompute first year of full-time return in sample:
	egen tmpk=min(k) if k>=0, by(persnr)
	gen retfulltime2=(k==tmpk & k>=0)
	drop tmpk
	
	* Re-flag women who change firm upon return to labour market [in full-time employment]
	cap drop jobchange
	bys persnr (year): gen jobchange_ = 1 if betnr!=betnr[_n-1] & _n>1
	egen jobchange=max(jobchange_), by(persnr)
	replace jobchange=0 if jobchange==.
	replace jobchange_=0 if jobchange_==.
	tab year jobchange_, mis
	tab year jobchange, mis
	table year if controlgroup==0, c(mean jobchange_ mean jobchange) format(%9.4fc)
	table year if controlgroup==1, c(mean jobchange_ mean jobchange) format(%9.4fc)	
	
	drop jobchange_
	
	keep if year>=1995 & year<=2008
	* Restrict years
	
	* Restrict lags
	keep if k>=$kmin & k<=$kmax
	
	* Number of observations by k
	tab k if k>=$kmin
	
	gen treated = 1-controlgroup
	
	ta k, gen(k)
	local m = r(r)
	forval r = 1(1)`m'{
		gen k`r'_1stbirth = k`r'*treated
		replace k`r'_1stbirth=0 if k`r'_1stbirth==.
	}
	
	egen cum = rowtotal(k*_1st*)
	replace cum=0 if k<=0
	egen cum2 = rowtotal(k1_1stbirth - k18_1stbirth)
	replace cum2=0 if k<0
	
	* Indicators for leave duration
	table k if k>=0, c(min durmonth max durmonth)	/*k=0 and durmonth>11*/

	* Restrict to spells where women are observed in the main sample after their return (full-time, 20-60, ...). 
	* If women return to part-time, I do not see them anymore. 

	bys persnr (year): gen obsgap=year[_n+1]-year if controlgroup==0

	bys persnr (year): gen r3_ = (childbirth==1 & durmonth==3 & obsgap<=1)								/*durations < 12 months might mix women before they give birth with returnees*/
	bys persnr (year): gen r12_ = (childbirth==1 & durmonth>3 & durmonth<=12 & obsgap<=1)				/*durations < 12 months might mix women before they give birth with returnees*/
	bys persnr (year): gen r24_ = (childbirth==1 & durmonth>12 & durmonth<=24 & obsgap>=1 & obsgap<=3)	/*cannot exclude that e.g. 14 months range over three years */
	bys persnr (year): gen r36_ = (childbirth==1 & durmonth>24 & durmonth<=36 & obsgap>=3 & obsgap<.) 	/*last condition ensures that person returns at all*/

	foreach x in 3 12 24 36 {
		egen r`x'=max(r`x'_), by(persnr)
		drop r`x'_
	}
	
	gen r = r3*3+r12*12+r24*24+r36*36
	
	* Check that only actual mothers are marked with duration
	ta r controlgroup, mis
	table r, c(min durmonth max durmonth)
	

* Some descriptive evidence:
	table k controlgroup, c(mean lw)
	table k controlgroup, c(p50 lw)
	drop k? k??
	gen k_5=(k==-5)
	gen k_4=(k==-4)
	gen k_3=(k==-3)
	gen k_2=(k==-2)
	gen k_1=(k==-1)
	forval v=0/13 {
		gen k`v'=(k==`v')
	}
	
	reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, cluster(persnr)
	mat b=e(b)'
	mat se=vecdiag(cholesky(diag(vecdiag(e(V)))))'
	mat treated= b, se
	reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
	mat b=e(b)'
	mat se=vecdiag(cholesky(diag(vecdiag(e(V)))))'
	mat control= b, se
	matlist treated, format(%9.4fc) tit(Wage profile of actual mothers - treatment group)
	matlist control, format(%9.4fc) tit(Wage profile of placebo mothers - control group)
		
* Summary statistics
cap drop d?
tab educ , g(d)

su age ageatbirth lw fe_female pe d? if controlgroup==0
su age ageatbirth lw fe_female pe d? if controlgroup==1
	
* Saving for regressions
save ${data}/regchildbirth, replace


use ${data}/regchildbirth, clear
cap erase ${data}/regchildbirth.dta
gen t=k>=0
gen lr12=k12*cum2
		
		
if "$m_baseline"=="1" {

**** BASELINE REGRESSIONS

* Wage Regressions
	
	* All
	
		* average effect (here: equivalent to dummy=1 for treated; if I estimate separately for control and treatment group, it is simply a mean comparison)
	
		reg lw t treated cum2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		
			*ONLY TESTING FOR EQUIVALENT SPECIFICATIONS
			*reg lw k*_1stbirth, cluster(persnr)
			
			*reg lw k*_1stbirth if controlgroup==0, cluster(persnr)
			*reg lw k_5 k_4 k_3 k_2 k_1 k0-k13 if controlgroup==0, cluster(persnr)
			*reg lw k*_1stbirth if controlgroup==1, cluster(persnr)
			*reg lw k_5 k_4 k_3 k_2 k_1 k0-k13 if controlgroup==1, cluster(persnr)
			
			*reg lw k*_1stbirth if controlgroup==0, cluster(persnr)

			* Include main effects to control for mean difference between treatment and control group and the lifecycle wage increase
			*reg lw treated k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 k1_1stbirth-k4_1stbirthk /*k5_1stbirth*/ k6_1stbirth-k19_1stbirth, cluster(persnr)
		
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, cluster(persnr)
		*reg lw k*_1stbirth if controlgroup==1, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
	
	
	* Return to same job
		
		* average effect
		reg lw t treated cum2 if jobchange==0, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==1, cluster(persnr)

	* Return to other job
		
		* average effect
		reg lw t treated cum2 if jobchange==1, cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==1, cluster(persnr)

	
	* Low skilled
		
		* average effect
		reg lw t treated cum2 if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==1, cluster(persnr)
			
	* Medium skilled
		
		* average effect
		reg lw t treated cum2 if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==1, cluster(persnr)

	* High skilled
		
		* average effect
		reg lw t treated cum2 if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==1, cluster(persnr)

	* Return in 3 months
		
		* average effect
		reg lw t treated cum2 if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==3, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		reg lw t treated cum2 if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==12, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		reg lw t treated cum2 if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==24, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		reg lw t treated cum2 if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==36, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

* AKM Effect Regressions

	* All
		
		* average effect
		reg fe_female t treated cum2, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

	* Return to same job
		
		* average effect
		reg fe_female t treated cum2 if (jobchange==0|evermother==0), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==1, cluster(persnr)

	* Return to other job
		
		* average effect
		reg fe_female t treated cum2 if (jobchange==1|evermother==0), cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==1, cluster(persnr)
		
	* Low skilled

		* average effect
		reg fe_female t treated cum2 if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==1, cluster(persnr)
			
	* Medium skilled
		
		* average effect
		reg fe_female t treated cum2 if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==1, cluster(persnr)

	* High skilled
		
		* average effect
		reg fe_female t treated cum2 if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==1, cluster(persnr)

	* Return in 3 months
		
		* average effect
		reg fe_female t treated cum2 if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==3, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		reg fe_female t treated cum2 if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==12, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		reg fe_female t treated cum2 if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==24, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		reg fe_female t treated cum2 if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==36, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

}
		
		
		
		

		
if "$m_fixedeffects"=="1" {		

**** REGRESSIONS WITH PERSON-FIXED EFFECTS

xtset persnr year

* Wage Regressions
	
	* All
	
		* average effect (here: equivalent to dummy=1 for treated; if I estimate separately for control and treatment group, it is simply a mean comparison)
		xtreg lw t treated cum2, fe cluster(persnr)
	
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)
	
	
	* Return to same job
		
		* average effect
		xtreg lw t treated cum2 if jobchange==0, fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==1, fe cluster(persnr)

	* Return to other job
		
		* average effect
		xtreg lw t treated cum2 if jobchange==1, fe cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==1, fe cluster(persnr)

	
	* Low skilled
		
		* average effect
		xtreg lw t treated cum2 if educ<=1, fe cluster(persnr)

		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==1, fe cluster(persnr)
			
	* Medium skilled
		
		* average effect
		xtreg lw t treated cum2 if educ==2, fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==1, fe cluster(persnr)

	* High skilled
		
		* average effect
		xtreg lw t treated cum2 if educ>=3 & educ<., fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==1, fe cluster(persnr)

	* Return in 3 months
		
		* average effect
		xtreg lw t treated cum2 if (r==3 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==3, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		xtreg lw t treated cum2 if (r==12 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==12, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		xtreg lw t treated cum2 if (r==24 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==24, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		xtreg lw t treated cum2 if (r==36 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==36, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)

* AKM Effect Regressions

	* All
		
		* average effect
		xtreg fe_female t treated cum2, fe cluster(persnr)

		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)

	* Return to same job
		
		* average effect
		xtreg fe_female t treated cum2 if (jobchange==0|evermother==0), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==1, fe cluster(persnr)

	* Return to other job
		
		* average effect
		xtreg fe_female t treated cum2 if (jobchange==1|evermother==0), fe cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==1, fe cluster(persnr)
		
	* Low skilled

		* average effect
		xtreg fe_female t treated cum2 if educ<=1, fe cluster(persnr)

		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==1, fe cluster(persnr)
			
	* Medium skilled
		
		* average effect
		xtreg fe_female t treated cum2 if educ==2, fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==1, fe cluster(persnr)

	* High skilled
		
		* average effect
		xtreg fe_female t treated cum2 if educ>=3 & educ<., fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==1, fe cluster(persnr)

	* Return in 3 months
		
		* average effect
		xtreg fe_female t treated cum2 if (r==3 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==3, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		xtreg fe_female t treated cum2 if (r==12 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==12, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		xtreg fe_female t treated cum2 if (r==24 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==24, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		xtreg fe_female t treated cum2 if (r==36 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==36, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, fe cluster(persnr)

}
		
	
		
if "$m_levels"=="1" {

**** REGRESSIONS IN LEVELS

replace lw=exp(lw)
replace fe_female=exp(fe_female)


* Wage Regressions
	
	* All
	
		* average effect (here: equivalent to dummy=1 for treated; if I estimate separately for control and treatment group, it is simply a mean comparison)
		
		reg lw t treated cum2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
	
	
	* Return to same job
		
		* average effect
		reg lw t treated cum2 if jobchange==0, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==1, cluster(persnr)

	* Return to other job
		
		* average effect
		reg lw t treated cum2 if jobchange==1, cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==1, cluster(persnr)

	
	* Low skilled
		
		* average effect
		reg lw t treated cum2 if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==1, cluster(persnr)
			
	* Medium skilled
		
		* average effect
		reg lw t treated cum2 if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==1, cluster(persnr)

	* High skilled
		
		* average effect
		reg lw t treated cum2 if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==1, cluster(persnr)

	* Return in 3 months
		
		* average effect
		reg lw t treated cum2 if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==3, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		reg lw t treated cum2 if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==12, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		reg lw t treated cum2 if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==24, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		reg lw t treated cum2 if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==36, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

* AKM Effect Regressions

	* All
		
		* average effect
		reg fe_female t treated cum2, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

	* Return to same job
		
		* average effect
		reg fe_female t treated cum2 if (jobchange==0|evermother==0), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==1, cluster(persnr)

	* Return to other job
		
		* average effect
		reg fe_female t treated cum2 if (jobchange==1|evermother==0), cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==1, cluster(persnr)
		
	* Low skilled

		* average effect
		reg fe_female t treated cum2 if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==1, cluster(persnr)
			
	* Medium skilled
		
		* average effect
		reg fe_female t treated cum2 if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==1, cluster(persnr)

	* High skilled
		
		* average effect
		reg fe_female t treated cum2 if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==1, cluster(persnr)

	* Return in 3 months
		
		* average effect
		reg fe_female t treated cum2 if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==3, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		reg fe_female t treated cum2 if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==12, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		reg fe_female t treated cum2 if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==24, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		reg fe_female t treated cum2 if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if r==36, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

	replace lw=ln(lw)
	replace fe_female=ln(fe_female)
}



if "$m_baseline_X"=="1" {

**** BASELINE REGRESSIONS

* Wage Regressions
	
	* All
	
		* average effect (here: equivalent to dummy=1 for treated; if I estimate separately for control and treatment group, it is simply a mean comparison)
		reg lw t treated cum2 $ageX $yearX $educX, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
	
	* Return to same job
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if jobchange==0, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1, cluster(persnr)

	* Return to other job
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if jobchange==1, cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1, cluster(persnr)

	
	* Low skilled
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1, cluster(persnr)
			
	* Medium skilled
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1, cluster(persnr)

	* High skilled
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1, cluster(persnr)

	* Return in 3 months
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==3, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==12, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==24, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		reg lw t treated cum2 $ageX $yearX $educX if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==36, cluster(persnr)
		reg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)

* AKM Effect Regressions

	* All
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)

	* Return to same job
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if (jobchange==0|evermother==0), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1, cluster(persnr)

	* Return to other job
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if (jobchange==1|evermother==0), cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1, cluster(persnr)
		
	* Low skilled

		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1, cluster(persnr)
			
	* Medium skilled
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1, cluster(persnr)

	* High skilled
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1, cluster(persnr)

	* Return in 3 months
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==3, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==12, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==24, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		reg fe_female t treated cum2 $ageX $yearX $educX if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==36, cluster(persnr)
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)

}
		
		
		
		

		
if "$m_fixedeffects_X"=="1" {		

**** REGRESSIONS WITH PERSON-FIXED EFFECTS

xtset persnr year

* Wage Regressions
	
	* All
	
		* average effect (here: equivalent to dummy=1 for treated; if I estimate separately for control and treatment group, it is simply a mean comparison)
		xtreg lw t treated cum2 $ageX $yearX $educX, fe cluster(persnr)

		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)
	
	* Return to same job
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if jobchange==0, fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1, fe cluster(persnr)

	* Return to other job
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if jobchange==1, fe cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1, fe cluster(persnr)

	
	* Low skilled
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if educ<=1, fe cluster(persnr)

		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1, fe cluster(persnr)
			
	* Medium skilled
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if educ==2, fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1, fe cluster(persnr)

	* High skilled
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if educ>=3 & educ<., fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1, fe cluster(persnr)

	* Return in 3 months
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if (r==3 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==3, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if (r==12 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==12, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if (r==24 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==24, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		xtreg lw t treated cum2 $ageX $yearX $educX if (r==36 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==36, fe cluster(persnr)
		xtreg lw k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)

* AKM Effect Regressions

	* All
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX, fe cluster(persnr)

		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)

	* Return to same job
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if (jobchange==0|evermother==0), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1, fe cluster(persnr)

	* Return to other job
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if (jobchange==1|evermother==0), fe cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1, fe cluster(persnr)
		
	* Low skilled

		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if educ<=1, fe cluster(persnr)

		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1, fe cluster(persnr)
			
	* Medium skilled
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if educ==2, fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1, fe cluster(persnr)

	* High skilled
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if educ>=3 & educ<., fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1, fe cluster(persnr)

	* Return in 3 months
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if (r==3 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==3, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)
		
	* Return in 4-12 months
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if (r==12 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==12, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)
	
	* Return in 13-24 months
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if (r==24 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==24, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)

	* Return in 25-36 months
		
		* average effect
		xtreg fe_female t treated cum2 $ageX $yearX $educX if (r==36 | controlgroup==1), fe cluster(persnr)
		
		* dynamic effect separately for treatment and control
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==36, fe cluster(persnr)
		xtreg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, fe cluster(persnr)

}
		
	
		
if "$m_levels_X"=="1" {

**** REGRESSIONS IN LEVELS

replace lw=exp(lw)
replace fe_female=exp(fe_female)
replace fe_female_90s=exp(fe_female_90s)
replace fe_female_00s=exp(fe_female_00s)
gen lw_90s=lw
gen lw_00s=lw
replace lw_90s=. if fe_female_90s==.
replace lw_00s=. if fe_female_00s==.

global pred_k = "(_b[k_5]*k_5 + _b[k_4]*k_4 + _b[k_3]*k_3 +_b[k_2]*k_2 + 0 + _b[k0]*k0 + _b[k1]*k1 + _b[k2]*k2 + _b[k3]*k3 + _b[k4]*k4 + _b[k5]*k5 + _b[k6]*k6 + _b[k7]*k7 + _b[k8]*k8 + _b[k9]*k9 + _b[k10]*k10 + _b[k11]*k11 + _b[k12]*k12 + _b[k13]*k13)"
cap drop yhat
cap drop ytilde

* Wage Regressions

	foreach LW in lw lw_90s lw_00s {

	* All
	
		* long run effect (here: equivalent to dummy=1 for treated; if I estimate separately for control and treatment group, it is simply a mean comparison)

		*reg `LW' t treated lr12 $ageX $yearX $educX, cluster(persnr)
	
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			/*
			gen ytilde2=_b[_cons]+_b[21.age]*(age==21)+_b[22.age]*(age==22)+_b[23.age]*(age==23)+_b[24.age]*(age==24) ///
						+_b[25.age]*(age==25)+_b[26.age]*(age==26)+_b[27.age]*(age==27)+_b[28.age]*(age==28) ///
						+_b[29.age]*(age==29)+_b[30.age]*(age==30)+_b[31.age]*(age==31)+_b[32.age]*(age==32) ///
						+_b[33.age]*(age==33)+_b[34.age]*(age==34)+_b[35.age]*(age==35)+_b[36.age]*(age==36) ///
						+_b[37.age]*(age==37)+_b[38.age]*(age==38)+_b[39.age]*(age==39)+_b[40.age]*(age==40) ///
						+_b[41.age]*(age==41)+_b[42.age]*(age==42)+_b[43.age]*(age==43)+_b[44.age]*(age==44) ///
						+_b[45.age]*(age==45)+_b[46.age]*(age==46)+_b[47.age]*(age==47)+_b[48.age]*(age==48) ///
						+_b[49.age]*(age==49)+_b[50.age]*(age==50)+_b[51.age]*(age==51)+_b[52.age]*(age==52) ///
						+_b[1996.year]*(year==1996)+_b[1997.year]*(year==1997)+_b[1998.year]*(year==1998) ///
						+_b[1999.year]*(year==1999)+_b[2000.year]*(year==2000)+_b[2001.year]*(year==2001) ///
						+_b[2002.year]*(year==2002)+_b[2003.year]*(year==2003)+_b[2004.year]*(year==2004) ///
						+_b[2005.year]*(year==2005)+_b[2006.year]*(year==2006)+_b[2007.year]*(year==2007)+_b[2008.year]*(year==2008)
			table k if e(sample), c(mean lw mean ytilde mean ytilde2) format(%9.3fc)*/
			*table k if e(sample), c(mean lw mean ytilde) format(%9.3fc)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = All, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
	
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = All, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
			
		* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: All Mothers"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore

	* Return to same job
		
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if jobchange==0, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Stayers, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Stayers, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde
			
	* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Job stayers"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore

				
	* Return to other job
		
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if jobchange==1, cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Movers, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {	
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Movers, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
			
	* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Job movers"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore		
					
	* Low skilled
	
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Low-skilled, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Low-skilled, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		
	* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Low skilled"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore				
							
	* Medium skilled
		
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Medium-skilled, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Medium-skilled, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		
		
	* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Medium skilled"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore				
					
	* High skilled
		
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = High-skilled, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = High-skilled, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		
	* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: High skilled"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore				
			
/*	* Return in 3 months
		
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==3, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 3 months, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 3 months, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		
	* Return in 4-12 months
		
		* long run effect
		reg `LW' t treated lr12 $ageX $yearX $educX if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==12, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 4-12 months, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 4-12 months, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
	
	* Return in 13-24 months
		
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==24, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 13-24 months, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 13-24 months, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 

	* Return in 25-36 months (by construction, no observations for 1 and 2 years after childbirth)
		
		* long run effect
		
		*reg `LW' t treated lr12 $ageX $yearX $educX if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==36, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			drop yhat ytilde 
		reg `LW' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			drop yhat ytilde 
	*/
	}
	
* AKM Effect Regressions

	foreach FE in "fe_female" "fe_female_90s" "fe_female_00s" {

	* All
		
		* long run effect
		
		*reg `FE' t treated lr12 $ageX $yearX $educX, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = All, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = All, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
				
			
		* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: All Mothers"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore

	* Return to same job
		
		* long run effect
		
		*reg `FE' t treated lr12 $ageX $yearX $educX if (jobchange==0|evermother==0), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Stayers, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Stayers, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
				
		* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==0 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Same job"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore

	* Return to other job
		
		* long run effect
		*reg `FE' t treated lr12 $ageX $yearX $educX if (jobchange==1|evermother==0), cluster(persnr)		
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Movers, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 

		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Movers, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
			
		* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if jobchange==1 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Other job"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore

	* Low skilled

		* long run effect
		*reg `FE' t treated lr12 $ageX $yearX $educX if educ<=1, cluster(persnr)

		* dynamic effect separately for treatment and control
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Low-skilled, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Low-skilled, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		
		* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ<=1 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Low skilled"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore


			
	* Medium skilled
		
		* long run effect
		
		*reg `FE' t treated lr12 $ageX $yearX $educX if educ==2, cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Medium-skilled, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Medium-skilled, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		
		* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ==2 & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: Medium skilled"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore


	* High skilled
		
		* long run effect
		*reg `FE' t treated lr12 $ageX $yearX $educX if educ>=3 & educ<., cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = High-skilled, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local `o'=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = High-skilled, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 

		* Pairs Bootstrap
			
			_dots 0, title("Bootstrap sample") reps($bsobs)
			mat bs=J($bsobs+1,3,.)
			forval bs=0/$bsobs {
			_dots 1 0
			qui{
				preserve
					if `bs'==0 {
					  gen persnr_new=persnr
					}
					if `bs'!=0 {
						bsample, cluster(persnr) idcluster(persnr_new)
					}
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==0
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_treated=_b[k12]/r(mean)
					drop ytilde yhat
					reg `FE' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if educ>=3 & educ<. & controlgroup==1
					predict yhat, xb
					gen ytilde=yhat - $pred_k
					su ytilde if k12==1 & e(sample)
					scalar coef_control=_b[k12]/r(mean)
					mat bs[`bs'+1,1]=`bs'+1
					if `bs'==0 mat bs[`bs'+1,2]=coef_treated-coef_control
					if `bs'!=0 mat bs[`bs'+1,3]=coef_treated-coef_control
				restore	
			}
			}
			preserve
			di "Display Bootstrap Output: High skilled"
			svmat bs
			collapse (sd) sd=bs3 (firstnm) coef=bs2
			mkmat coef sd, matrix(bsout)
			di "Display Bootstrap Output: All Mothers"
			di "Column 1: coefficient on year 12 after childbirth"
			di "Column 3: bootstrapped standard error"
			matlist bsout, format(%9.5gc)
			restore

/*
	* Return in 3 months
		
		* long run effect
		reg fe_female t treated lr12 $ageX $yearX $educX if (r==3 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==3, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 3 months, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 3 months, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
			
			
		
	* Return in 4-12 months
		
		* long run effect
		reg fe_female t treated lr12 $ageX $yearX $educX if (r==12 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==12, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 4-12 months, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 4-12 months, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
	
	* Return in 13-24 months
		
		* average effect
		reg fe_female t treated lr12 $ageX $yearX $educX if (r==24 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		capture qui {
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==24, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 13-24 months, treatment = 1"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 
		capture qui {
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			mat b=e(b)
			mat b=b[1,1..18]'
			mat v=e(V)
			mat se=vecdiag(cholesky(diag(vecdiag(v[1..18,1..18]))))'
			local o=e(N)
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			mat r=(r(Stat1)\r(Stat2)\r(Stat3)\r(Stat4)/*\r(Stat5)*/\r(Stat6)\r(Stat7)\r(Stat8)\r(Stat9)\r(Stat10)\r(Stat11)\r(Stat12)\r(Stat13)\r(Stat14)\r(Stat15)\r(Stat16)\r(Stat17)\r(Stat18)\r(Stat19)) 
			mat b=vecdiag(diag(b)*inv(diag(r)))'
			mat se=vecdiag(diag(se)*inv(diag(r)))'
			mat r=[b,se,b-se*1.95996,b+se*1.95996]
		}
			di "Sample = Return 13-24 months, treatment = 0"
			di "N = " `o'
			matlist r, format(%9.4gc)
			drop yhat ytilde 

	* Return in 25-36 months
		
		* long run effect
		reg fe_female t treated lr12 $ageX $yearX $educX if (r==36 | controlgroup==1), cluster(persnr)
		
		* dynamic effect separately for treatment and control
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if r==36, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			drop yhat ytilde 
		reg fe_female k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 $ageX $yearX $educX if controlgroup==1, cluster(persnr)
			predict yhat, xb
			gen ytilde=yhat - $pred_k
			tabstat ytilde if e(sample), col(stats) by(k) save nototal
			drop yhat ytilde 

	replace lw=ln(lw)
	replace fe_female=ln(fe_female)
*/
	}
}


if "$m_supplementary"=="1" {

* Supplementary analysis: What is the proximate role of workplace amenities and family friendliness for the motherhood penalty?

* Import idnum
*merge 1:1 persnr year using ${data}/AKM_select_1, nogen keep(1 3) keepus(idnum)
*merge 1:1 persnr year using ${data}/AKM_select_2, nogen keep(1 3) keepus(idnum) update

* Import indicators for family friendliness and female management share
merge m:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare, keep(1 3) keepus(famfriendly femsharemanagement) nogen

* Import industry indicator
merge m:1 persnr year using ${data}/masterfile, keep(1 3) keepus(ind) nogen
ta ind, mis
ta ind, mis nola

gen dpublic = (ind>=16 & ind<=16)
replace dpublic=. if ind==.

* Restrict to IAB-establishments
gen iab=idnum!=.
ta iab
keep if iab==1
replace famfriendly=. if year!=2002			/*use most complete information*/

egen famfriendly2=max(famfriendly), by(idnum)
egen femsharemanagement2=max(femsharemanagement), by(idnum)

su famfriendly famfriendly2 femsharemanagement femsharemanagement2, det

ren famfriendly famfriendly_old
ren femsharemanagement femsharemanagement_old

ren famfriendly2 famfriendly
ren femsharemanagement2 femsharemanagement

* Make indicator for presence of female managers
gen dfemsharemanagement=(femsharemanagement>0 & femsharemanagement<.)
replace dfemsharemanagement=. if femsharemanagement==.

table year, c(mean famfriendly mean femsharemanagement mean dfemsharemanagement mean dpublic) col format(%9.4fc)


	foreach var in femsharemanagement dfemsharemanagement dpublic{

		* All	
			
			* long run effect
			reg `var' t treated lr12, cluster(persnr)
			su lr12 if e(sample)
			di _b[lr12]*r(sd)

			* dynamic effect separately for treatment and control
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==0, cluster(persnr)
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if controlgroup==1, cluster(persnr)

		* Return to same job
			
			* long run effect
			reg `var' t treated lr12 if (jobchange==0|evermother==0), cluster(persnr)
			su lr12 if e(sample)
			di _b[lr12]*r(sd)
			
			* dynamic effect separately for treatment and control
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==0, cluster(persnr)
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==0 & controlgroup==1, cluster(persnr)

		* Return to other job
			
			* long run effect
			reg `var' t treated lr12 if (jobchange==1|evermother==0), cluster(persnr)		
			su lr12 if e(sample)
			di _b[lr12]*r(sd)
			
			* dynamic effect separately for treatment and control
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==0, cluster(persnr)
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if jobchange==1 & controlgroup==1, cluster(persnr)

		* Low skilled

			* long run effect
			reg `var' t treated lr12 if educ<=1, cluster(persnr)
			su lr12 if e(sample)
			di _b[lr12]*r(sd)
			
			* dynamic effect separately for treatment and control
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==0, cluster(persnr)
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ<=1 & controlgroup==1, cluster(persnr)
				
		* Medium skilled
			
			* long run effect
			reg `var' t treated lr12 if educ==2, cluster(persnr)
			su lr12 if e(sample)
			di _b[lr12]*r(sd)
			
			* dynamic effect separately for treatment and control
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==0, cluster(persnr)
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ==2 & controlgroup==1, cluster(persnr)

		* High skilled
			
			* long run effect
			reg `var' t treated lr12 if educ>=3 & educ<., cluster(persnr)
			su lr12 if e(sample)
			di _b[lr12]*r(sd)
			
			* dynamic effect separately for treatment and control
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==0, cluster(persnr)
			reg `var' k_5 k_4 k_3 k_2 /*k_1*/ k0-k13 if educ>=3 & educ<. & controlgroup==1, cluster(persnr)

	}

}



if "$s_meanwages"=="1" {

	*** Obtain a summary measure of the impact of childbirth on the GWG / firm GWG:
	* The problem is: I do not observe when men get a child
	* 
	* Suggested workaround:
	* 1. estimate mean wage/AKM effect of mothers, placebo mothers, and men at age: "age at birth"+"10 years"
	* 2. store mean wage and number of observations to compute an overall wage impact of childbirth

	use persnr betnr year age fem educ ln_impwage VA using ${data}/AKM_select_1 if year<=2000, clear
	append using ${data}/AKM_select_2, keep(persnr betnr year age fem educ ln_impwage)

	* Import AKM effects and restrict to dual-connected set
		capture confirm f ${data}/AKM_fe_allyears.dta
		if _rc>0{
			gen psi0 = uniform()*0.5
			gen psi1 = uniform()*0.5
			bys betnr (year): replace psi0 = psi0[1]
			bys betnr (year): replace psi1 = psi1[1]
			gen theta = uniform()+3.5
			bys persnr (year): replace theta = theta[1]
			gen xb = uniform()*0.25
			gen r = 0
			replace r = ln_impwage - psi0 - theta - xb if fem==0
			replace r = ln_impwage - psi1 - theta - xb if fem==1
			drop xb r
		}
		if _rc==0 {
			merge 1:1 persnr year using ${data}/AKM_fe_allyears, keep(1 3) keepus(psi theta) nogen
			gen tmpfe0=psi if fem==0
			egen psi0=max(tmpfe0), by(betnr)
			gen tmpfe1=psi if fem==1
			egen psi1=max(tmpfe1), by(betnr)
			bys fem : su psi psi0 psi1
		}

	* Restrict to dual-connected set in pooled sample (all years)
	keep if psi0!=.& psi1!=.
	
	* Rescale AKM-effects to mean zero [to make comparable across gender]
	sum psi0 if fem==0
	gen fe_male = psi0 - r(mean)
	sum psi1 if fem==1
	gen fe_female = psi1 - r(mean)
	drop psi0 psi1	
	
	ren ln_impwage lw
		
	* Import treatment/control group
	merge 1:1 persnr year using ${data}/regchildbirth, keep(1 3) keepus(controlgroup ageatbirth evermother k) nogen

	tab fem controlgroup, mis /*should be all missing for males and missing for some females (those who are not mothers and were not selected into the control group using the algorithm from above; they were too likely to be mothers at some point)*/
			
	mat W=J(30,9,.)
	local r=0

	qui forval age=21(1)50 {
		local r=`r'+1

		su lw if evermother==1 & k==10 & age==`age' & controlgroup==0
			mat W[`r',1]=r(mean)
			mat W[`r',7]=r(N)
		su fe_female if evermother==1 & k==10 & age==`age' & controlgroup==0
			mat W[`r',4]=r(mean)
		su lw if evermother==0 & k==10 & age==`age' & controlgroup==1
			mat W[`r',2]=r(mean)
			mat W[`r',8]=r(N)
		su fe_female if evermother==0 & k==10 & age==`age' & controlgroup==1
			mat W[`r',5]=r(mean)
		su lw if fem==0 & age==`age'
			mat W[`r',3]=r(mean)
			mat W[`r',9]=r(N)
		su fe_male if fem==0 & age==`age'
			mat W[`r',6]=r(mean)
		
	}

	matlist W
	
	ta age, g(age)
	forval x=20(1)60{
		local y=`x'-19
		cap ren age`y' age_`y'
		cap ren age`x' age_`x'
		ren age_`y' age`x'
	}
	
	* Age-earnings profiles by age at first birth
	su ageatbirth if controlgroup==0 & evermother==1
	local m=20
	local n=40
	
	/*
	forval j=`m'(1)`n'{
	
		di "age at birth: `j'"
		di "actual mothers"
		count if controlgroup==0 & evermother==1 & ageatbirth==`j'
		if r(N)>0 {
			table age if controlgroup==0 & evermother==1 & ageatbirth==`j', c(mean lw) format(%9.4gc)
		}
		/*
		di "control mothers"
		count if controlgroup==1 & evermother==0 & ageatbirth==`j'
		if r(N)>0 {
			table age if controlgroup==1 & evermother==0 & ageatbirth==`j', c(mean lw) format(%9.4gc)
		}
		*/
	}
	*/
	
	* Wages of men by mean age of women at different leads and lags relative to birth
	mat V=J(19,3,.)
	local r=0
	
	forval j=-5(1)13{
	
		local r=`r'+1
	
		qui su age if controlgroup==0 & evermother==1 & k==`j', det
		di "mean age of actual mothers at k==`j': " round(r(mean),1)
		di "median age of actual mothers at k==`j': " r(p50)
		local medage=r(p50)
		mat V[`r',1]=`j'
		su lw if age==`medage' & fem==0
		mat V[`r',2]=r(mean)		
		su fe_male if age==`medage' & fem==0
		mat V[`r',3]=r(mean)		
	
	}
	
	matlist V
	
}

if "$age_decomposition"=="1" {

****************************************************************************************
* Compute the mean firm effects of men, women, and mothers over the life cycle to 
* illustrate the impact of childbirth/motherhood on the firm wage premium gap between
* men and women (Figure 8 of WP-version only hints at an association with childbirth,
* but now I can look into this more explicitly.
****************************************************************************************



* For pooled sample with AKM effects estimates above
*****************************************************

	global tau_VA_allyears	"3.15"

	use persnr betnr year age fem educ ln_impwage VA using ${data}/AKM_select_1 if year<=2000, clear
	append using ${data}/AKM_select_2, keep(persnr betnr year age fem educ ln_impwage VA)

	* Import AKM effects and restrict to dual-connected set
		capture confirm f ${data}/feffs_periodpooled.dta
		if _rc>0{
			gen psi0 = uniform()*0.5
			gen psi1 = uniform()*0.5
			bys betnr (year): replace psi0 = psi0[1]
			bys betnr (year): replace psi1 = psi1[1]
			gen theta = uniform()+3.5
			bys persnr (year): replace theta = theta[1]
			gen xb = uniform()*0.25
			gen r = 0
			replace r = ln_impwage - psi0 - theta - xb if fem==0
			replace r = ln_impwage - psi1 - theta - xb if fem==1
			drop xb r
		}
	if _rc==0 {
		merge m:1 betnr using ${data}/feffs_periodpooled, keep(1 3) keepus(psi1 psi0) nogen
		bys fem : su psi0 psi1
	}

* Restrict to dual-connected set in pooled sample (all years)
	keep if psi0!=.& psi1!=.
	
* Rename AKM-effects
	rename ln_impwage lw

	* Import treatment/control group
	merge 1:1 persnr year using ${data}/regchildbirth, keep(1 3) keepus(controlgroup ageatbirth evermother) nogen

	tab fem controlgroup, mis /*should be all missing for males and missing for some females (those who are not mothers and were not selected into the control group using the algorithm from above; they were too likely to be mothers at some point)*/
		
	forval g=0/1{

		di _newline(1)
		di "Normalisation: Estimated optimal cutoff value"
		di "---------------------------------------------------------"
		qui sum psi`g' if VA<=${tau_VA_allyears}	
		scalar norm=r(mean)
		scalar N=r(N)
		di "`: label (fem)`g''"
		di "Mean firm effect for zero surplus firms (VA/L)                   : " r(mean)
		qui count if VA!=.	
		di "Share of person-years below threshold with non-missing VA/L      : " N/r(N)
		qui count if psi`g'<=norm
		di "Share of person-years with psi`g' < " norm	" : " r(N)/_N
		gen fe_`g'_nVA=psi`g'-norm	
			
	}

		* Age groups
		gen ageg14=int((age+1)/3)-6
		qui levelsof ageg14, l(l)
		foreach f of local l {
			qui su age if ageg14==`f'
			local min=r(min)
			local max=r(max)
			lab def ageg14 `f' "`min'-`max'", modify
		}
		lab val ageg14 ageg14
		ta ageg14
		
		gen lwM=lw if fem==0
		gen lwF=lw if fem==1
		gen lwF_control=lw if fem==1 & controlgroup==1
		gen lwF_treated=lw if fem==1 & controlgroup==0
		gen lwF_rest=lw if fem==1 & controlgroup==.
		
		gen feM=fe_0_nVA if fem==0
		gen feF=fe_1_nVA if fem==1
		gen feF_control=fe_1_nVA if fem==1 & controlgroup==1
		gen feF_treated=fe_1_nVA if fem==1 & controlgroup==0
		gen feF_rest=fe_1_nVA if fem==1 & controlgroup==.
		
		* Count variables for collapse
		gen countall=1
		bys betnr ageg14 fem: gen countfirms=_n==1
		bys persnr ageg14: gen countworkers=_n==1
		
		collapse (mean) lwM lwF lwF_control lwF_treated lwF_rest feM feF feF_control feF_treated feF_rest (sum) persyrs=countall firms=countfirms workers=countworkers, by(ageg14) fast
		decode ageg14, g(label)
		mkmat lwM lwF lwF_control lwF_treated lwF_rest feM feF feF_control feF_treated feF_rest persyrs workers firms, matrix(decomp) rownames(label)

		matlist decomp,		format(%10.4gc)	///
							lines(oneline)	///
							tw(5)			///
							showcoleq(c)	///
							linesize(120)	///
							underscore		///
							tit(Decomposition based on VA over ageg14 (last 6 cols show # of male(M)/female(F) person-years, workers, and firms))

	
	
* For period-specific samples with AKM effects estimated in baseline models
****************************************************************************	
	global tau_VA_1_all	"3.2"
	global tau_VA_2_all	"3.1"
		
	forval j=1/2{

		use ${data}/worker_dual_`j'_all, clear
		keep persnr betnr year age idnum fem educ VA psi?0 psi?1 ln_impwage
		ren ln_impwage lw
		
		* Import treatment/control group
		merge 1:1 persnr year using ${data}/regchildbirth, keep(1 3) keepus(controlgroup ageatbirth evermother) nogen

		tab fem controlgroup, mis /*should be all missing for males and missing for some females (those who are not mothers and were not selected into the control group using the algorithm from above; they were too likely to be mothers at some point)*/
			
		forval g=0/1{
		
			di _newline(1)
			di "Normalisation: Estimated optimal cutoff value"
			di "---------------------------------------------------------"
			qui sum psi`j'`g' if VA<=${tau_VA_`j'_all}	
			scalar norm=r(mean)
			scalar N=r(N)
			di "`: label (fem)`g''"
			di "Mean firm effect for zero surplus firms (VA/L)                   : " r(mean)
			qui count if VA!=.	
			di "Share of person-years below threshold with non-missing VA/L      : " N/r(N)
			qui count if psi`j'`g'<=norm
			di "Share of person-years with psi`j'`g' < " norm	" : " r(N)/_N
			gen fe_`j'`g'_nVA=psi`j'`g'-norm	
			
		}
		
		* Age groups
		gen ageg14=int((age+1)/3)-6
		qui levelsof ageg14, l(l)
		foreach f of local l {
			qui su age if ageg14==`f'
			local min=r(min)
			local max=r(max)
			lab def ageg14 `f' "`min'-`max'", modify
		}
		lab val ageg14 ageg14
		ta ageg14
		
		gen lwM=lw if fem==0
		gen lwF=lw if fem==1
		gen lwF_control=lw if fem==1 & controlgroup==1
		gen lwF_treated=lw if fem==1 & controlgroup==0
		gen lwF_rest=lw if fem==1 & controlgroup==.
		
		gen feM=fe_`j'0_nVA if fem==0
		gen feF=fe_`j'1_nVA if fem==1
		gen feF_control=fe_`j'1_nVA if fem==1 & controlgroup==1
		gen feF_treated=fe_`j'1_nVA if fem==1 & controlgroup==0
		gen feF_rest=fe_`j'1_nVA if fem==1 & controlgroup==.
		
		* Count variables for collapse
		gen countall=1
		bys betnr ageg14 fem: gen countfirms=_n==1
		bys persnr ageg14: gen countworkers=_n==1
		
		collapse (mean) lwM lwF lwF_control lwF_treated lwF_rest feM feF feF_control feF_treated feF_rest (sum) persyrs=countall firms=countfirms workers=countworkers, by(ageg14) fast
		decode ageg14, g(label)
		mkmat lwM lwF lwF_control lwF_treated lwF_rest feM feF feF_control feF_treated feF_rest persyrs workers firms, matrix(decomp) rownames(label)

		matlist decomp,		format(%10.4gc)	///
							lines(oneline)	///
							tw(5)			///
							showcoleq(c)	///
							linesize(120)	///
							underscore		///
							tit(Decomposition based on VA over ageg14 (last 6 cols show # of male(M)/female(F) person-years, workers, and firms))

	}

}

cap log close
