clear all
set linesize 120
cap log close
set more 1
log using ${log}/11_eventstudy.log, replace
adopath ++${prog}


* Part 1: Compute mean wage profiles of workers moving across origin-destination quartiles.
* Construction inspired by CCK, 2015 (SAS code by David Card, 2015)
* Procedure first calculates firm means, then merges those to worker level data and computes transition profiles in terms of wages.


cap program drop events
program define events

use ${data}/AKM_select_`1'.dta,clear

* Step 1: generate firm level variables

* Approximate years of schooling (taken from CHK) *
gen school=.
replace school=10.5 if educ==0 	// missing, 0
replace school=11   if educ==1	// dropouts/primary
replace school=13	if educ==2	// apprentice
replace school=15	if educ==3	// some college
replace school=18	if educ==4	// university

* Import estimated person effects (from AKMs)
merge m:1 persnr using ${data}/peffs_`1'.dta, keep(1 3) keepus(theta`1') nogenerate
gen connected=theta`1'!=.

* Import estimated firm effects (from AKMs, see matlab code); 2 effects per firm, 1 for each gender; make indicator for dual-connected set
merge m:1 betnr using ${data}/feffs_`1'.dta, keep(1 3) keepus(psi`1'0 psi`1'1) nogenerate
gen dualconnect=(psi`1'0!=. & psi`1'1!=.)

ren ln_impwage lw
keep persnr betnr idnum year lw age educ school fem connected dualconnect psi`1'0 psi`1'1

tab fem connected,mis

di _newline "Now drop observations outside largest connected set of each gender"
keep if connected==1

gen lwF=fem*lw if fem==1
gen lwM=(1-fem)*lw if fem==0
di _newline "Create firm-year data set with worker-means"
di "Collapse by betnr-year, note that idnum and betnr refer to same firm if in IAB, so no loss of info"
collapse (mean) mlw = lw 	///
				mlwF = lwF	///
				mlwM = lwM	///
				mschool=school	///
		 (sum)	Fempl=fem			///
		 (count) empl=lw			///
		 (firstnm) dual=dualconnect	, by(betnr year)
gen Mempl=empl-Fempl
egen avFempl = mean(Fempl), by(betnr)
egen avMempl = mean(Mempl), by(betnr)
egen avempl  = mean(empl), by(betnr)
egen avmschool=mean(mschool), by(betnr)
gen nofemales=(avFempl==0)
gen nomales=(avMempl==0)

di _newline "Tabulate distribution of single gender firms."
bys betnr : gen t=_n==1
tab nofemales nomales if t

di _newline "Check consistency with dual-connect indicator (@ firm level)"
di "Should report 0 : " _continue
count if dual==1 & nofemales==1 & t
di "Should report 0 : " _continue
count if dual==1 & nomales==1 & t
di "Should report all firms with nofemales==1 (next to numbers should be identical): " _continue
count if dual==0 & nofemales==1 & t
count if nofemales==1 & t
di "Should report all firms with nomales==1  (next to numbers should be identical): " _continue
count if dual==0 & nomales==1 & t
count if nomales==1 & t

drop t

sum

* check variable if all firms are dual-connected (no* should be mean 0)!
sum if nofemales==1
sum if nomales==1

* check variable if all firms are dual-connected (no* should be mean 0)!
sum if nomales==0 & nofemales==0

lab data "firm-year data, means by year"
save ${data}/fymeans.dta, replace



* Step 2: Prepare worker level data

use ${data}/AKM_select_`1'.dta,clear

* Approximate years of schooling (taken from CHK) *
gen school=.
replace school=10.5 if educ==0 	// missing, 0
replace school=11   if educ==1	// dropouts/primary
replace school=13	if educ==2	// apprentice
replace school=15	if educ==3	// some college
replace school=18	if educ==4	// university

* Import estimated person effects (from AKMs)
merge m:1 persnr using ${data}/peffs_`1'.dta, keep(1 3) keepus(theta`1') nogenerate
gen connected=theta`1'!=.

* Import estimated firm effects (from AKMs); 2 effects per firm, 1 for each gender
merge m:1 betnr using ${data}/feffs_`1'.dta, keep(1 3) keepus(psi`1'0 psi`1'1) nogenerate
gen dualconnect=(psi`1'0!=. & psi`1'1!=.)

ren ln_impwage lw
keep persnr betnr idnum year lw age educ school fem connected dualconnect psi`1'0 psi`1'1

* re-check if all workers/firms are dual-connected
tab fem connected,mis

di _newline "Now drop observations outside largest connected set of each gender"
keep if connected==1

* Import firm level means
merge m:1 betnr year using ${data}/fymeans.dta, keep(3) nogen


/* Create leave-out means for each worker at a firm with 2+ employees in dataset */

gen lom_lw=.
gen lom_sc=.
gen lom_lwM=.
gen lom_lwF=.

replace lom_lw=(mlw*empl-lw)/(empl-1) if empl>1
replace lom_sc=(mschool*empl-school)/(empl-1) if empl>1

* only male employees: get leave-out-mean if 2+ workers are male, and substitute mean
* firm wage of women for female leave-out-mean wage in male data rows.
replace lom_lwM=(mlwM*Mempl-lw)/(Mempl-1) if Mempl>1 & fem==0
replace lom_lwF=mlwF if Fempl>0 & fem==0

* only female employees: get leave-out-mean if 2+ workers are male, and substitute mean
* firm wage of women for female leave-out-mean wage in male data rows.
replace lom_lwF=(mlwF*Fempl-lw)/(Fempl-1) if Fempl>1 & fem==1
replace lom_lwM=mlwM if Mempl>0 & fem==1

di _newline(2) "Partial correlations: full sample"
pwcorr lw mlw lom_lw lom_lwM lom_lwF school mschool lom_sc avmschool

di _newline(2) "Partial correlations: for males" 
pwcorr lw mlw lom_lw lom_lwM lom_lwF school mschool lom_sc if fem==0

di _newline(2) "Partial correlations: all females"
pwcorr lw mlw lom_lw lom_lwM lom_lwF school mschool lom_sc if fem==1

di _newline(2) "Partial correlations: mixed gender firms, all"
pwcorr lw mlw lom_lw lom_lwM lom_lwF school mschool lom_sc avmschool if nofemales==0 & nomales==0

di _newline(2) "Partial correlations: mixed gender firms, males"
pwcorr lw mlw lom_lw lom_lwM lom_lwF school mschool lom_sc avmschool if nofemales==0 & nomales==0 & fem==0

di _newline(2) "Partial correlations: mixed gender firms, females"
pwcorr lw mlw lom_lw lom_lwM lom_lwF school mschool lom_sc avmschool if nofemales==0 & nomales==0 & fem==1


* Construct EVENT STUDY

di "# of obs before eliminating single-gender firms :  " _continue
count

* mixed gender firms
keep if nofemales==0 & nomales==0
di "# of obs after eliminating single-gender firms :  " _continue
count

fastxtile qlom_lw=lom_lw, nq(4)
tab qlom_lw, mis

gen obs1=(lom_lw==.)
replace qlom_lw=0 if obs1==1

* singleton firms coded as separate category (0)
tab qlom_lw obs1, mis




/*--- Identifying event spells and stayer spells ---*/

* this follows CCK's SAS code as closely as possible.

xtset persnr year

gen firmid=betnr
format firmid %13.0f

sort persnr year

by persnr : gen last1firmid=firmid[_n-1]
by persnr : gen last2firmid=firmid[_n-2]
by persnr : gen next1firmid=firmid[_n+1]
by persnr : gen next2firmid=firmid[_n+2]
by persnr : gen lag1qlom_lw=qlom_lw[_n-1]

by persnr : gen lag1lw=lw[_n-1]
by persnr : gen lag2lw=lw[_n-2]
by persnr : gen lag3lw=lw[_n-3]
by persnr : gen lag4lw=lw[_n-4]
by persnr : gen lead1lw=lw[_n+1]
by persnr : gen lead2lw=lw[_n+2]
by persnr : gen lead3lw=lw[_n+3]
by persnr : gen lead4lw=lw[_n+4]

gen jobch=0


/*--- Identify events and job-spell counter per worker ---*/

/* First, sort by persnr year (ascending) */
sort persnr year

*  treat first observation separately
by persnr : gen tenure=1 if _n==1
by persnr : gen startyr=year if _n==1

* ... if firmid!=last1firmid, job is quit
by persnr : replace jobch=1 if firmid!=last1firmid		 // first person-obs is indicated as jobch (intentionally!)
by persnr : gen jobnum=sum(jobch)
by persnr : replace tenure=1 if firmid!=last1firmid
by persnr : replace startyr=year if firmid!=last1firmid & _n>1

* ... if firmid==last1firmid, job continues, so tenure +1, 
bys persnr jobnum (year) : replace tenure=tenure[_n-1]+1 if firmid==last1firmid & _n>1


/* Second, sort by persnr year (descending) */
gsort persnr -year

* treat last observation separately
by persnr : gen yearstogo=1 if _n==1
by persnr : gen endyr=year if _n==1

* ... if firmid!= next1firmid, job has been quit
by persnr : replace yearstogo=1 if firmid!=next1firmid
by persnr : replace endyr=year if firmid!=next1firmid

* ... if firmid== next1firmid, job continues in the future
gen jobnum2=-1*jobnum
gen year2 = -1*year
bys persnr jobnum2 (year2): replace yearstogo=yearstogo[_n-1]+1 if firmid==next1firmid & _n>1
drop jobnum2 year2


/* Obtain year entry, exit, and total # of jobs per worker */
egen firstyr=min(year), by(persnr)
egen lastyr=max(year), by(persnr)
egen numjobs=max(jobnum), by(persnr)


/* Get lagged values of remaining variables */
sort persnr year
foreach v in tenure lom_lw lom_lwM lom_lwF lom_sc avmschool mschool avempl empl {
by persnr : gen lag1`v'=`v'[_n-1]
}



/*---	 Event/Stayer spells; selection step-by-spep 	---*/

gen eventspell=0
gen stayspell=0

/* Event spells: jobnum>=2, startyr==year  */
replace eventspell=1 if jobnum>=2 & startyr==year

gen numpre=lag1tenure if eventspell==1
gen numpost=yearstogo if eventspell==1

/* Stay spells: no change @ anytime */
replace stayspell=1 if last2firmid==firmid & last1firmid==firmid & next1firmid==firmid & next2firmid==firmid

lab data "data w/ event/stay spell indicators and leads/lags assigned"


/*--- Selecting EVENT spells for analysis (2+ obs pre and 2+ obs post move), no singleton firms ---*/

preserve
di "Event spells"
* eliminate singletons in selection of event spells
keep if eventspell==1 & numpre>=2 & numpost>=2 & lag1qlom_lw>0 & qlom_lw>0

* Wage change (4 year, leave out latest year)
gen dlw=lead1lw-lag2lw
gen yr=year-2
gen age2 =age*age/100
gen event=1

keep persnr year yr fem school educ age age2 lag1qlom_lw qlom_lw lag1lom_lw lom_lw lag2lw lag1lw lw lead1lw dlw lag1lom_lwM lom_lwM lag1lom_sc lom_sc event last1firmid firmid
lab data "event spells, no singleton firms, 2+past, 2+future"
save ${data}/eventspells_`1'.dta,replace
sum


/* Wage changes of event spells, 2+years at old firm and 2+ years at new firm, exclude singleton firms */

* Frequencies shown below means
lab var lag1qlom_lw "Origin"
lab var qlom_lw "Destination"

di _newline "Pooled"
table lag1qlom_lw qlom_lw, c(mean lw freq) format(%9.5gc)

di _newline "Men"
table lag1qlom_lw qlom_lw if fem==0, c(mean lw freq) format(%9.5gc)

di _newline "Women"
table lag1qlom_lw qlom_lw if fem==1, c(mean lw freq) format(%9.5gc)
restore




/* Selecting STAY spells for analysis (2+ obs pre and 2+ obs post move), no singleton firms */

preserve
di "Stayspells"
* selection eliminates singletons (Achtung: "assignment von workplaces ist nicht konstant, daher wechseln auch stayer im zeitverlauf")
keep if stayspell==1 & lag1qlom_lw>0

* Wage change (4 year, leave out latest year, only used for selection)
gen dlw=lead1lw-lag2lw
gen yr=year-2
gen age2 =age*age/100
gen event=0

keep persnr year yr fem school educ age age2 lag1qlom_lw qlom_lw lag1lom_lw lom_lw lag2lw lag1lw lw lead1lw dlw lag1lom_lwM lom_lwM lag1lom_sc lom_sc event last1firmid firmid
lab data "stay spells, no singleton firms, 2+past, 2+future"
save ${data}/stayspells_`1'.dta,replace
sum


/* Wage changes of stay spells, 2+years at old firm and 2+ years at new firm, exclude singleton firms */

* Frequencies shown below means
lab var lag1qlom_lw "Origin"
lab var qlom_lw "Destination"

* Pooled
table lag1qlom_lw qlom_lw, c(mean lw freq) format(%9.5gc)

* Men
table lag1qlom_lw qlom_lw if fem==0, c(mean lw freq) format(%9.5gc)

* Women
table lag1qlom_lw qlom_lw if fem==1, c(mean lw freq) format(%9.5gc)
restore


/*--- Analyse event and stay spells ---*/

use ${data}/eventspells_`1'.dta, clear
append using ${data}/stayspells_`1'.dta

*use ${data}/eventspells.dta, clear
*append using ${data}/stayspells.dta
gen dlws=dlw if event==0

/* Summarise frequencies of event and stay spells */
tab event fem
tab yr event
tab lag1qlom_lw event
tab lag1qlom_lw event if fem==0
tab lag1qlom_lw event if fem==1

sum dlw dlws fem school



/* Fit OLS to STAYERS, make out-of-sample prediction based on stayer coefficients
  either fit and predict separately by gender, or run pooled model fully interacted */

gen dlwr=.
gen dlwhat=.
forval g=0/1 {
reg dlws i.yr i.educ age age2 i.educ#c.age educ#c.age2 if fem==`g'
predict yhat_`g' if fem==`g', xb
replace dlwr=dlw-yhat_`g' if fem==`g'
replace dlwhat=yhat_`g' if fem==`g'
}

/* Assert that results are really the same */
reg dlws fem##(i.yr i.educ c.age c.age2 i.educ#c.age educ#c.age2)
predict yhat_2, xb
gen gap2=dlw-yhat_2

gen samefirm=(firmid==last1firmid)
bys samefirm : compare dlwr gap2
sum dlwr gap2
cap drop gap2 yhat_*

* Get summary statistics
bys event : sum dlw dlws dlwhat dlwr fem school educ

lab data "event & stay spells combined, 2+ past, 2+ togo, no singleton firms"
save ${data}/allspells_`1'.dta, replace


/* Final tables with all results */

* wage profiles at MEANS; variable "nevents" stores the number of transitions within each origin-destination quartile. 
* each transition involes 1 worker and 2 establishments.

preserve
collapse (count) nevents=dlw (mean) lag2lw lag1lw lw lead1lw dlw dlwr lag1lom_lw lom_lw lag1lom_sc lom_sc if event==1, by(fem lag1qlom_lw qlom_lw)
ren lag1qlom_lw originQ
ren qlom_lw destinatQ
ren dlw d4_lw_raw
ren dlwr d4_lw_adj
mkmat fem originQ destinatQ nevents lag2lw lag1lw lw lead1lw d4_lw_raw d4_lw_adj , matrix(eventout)
matlist eventout,	format(%9.5gc)	///
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Event study results by gender: Wage profiles at Means)
restore

* wage profiles at 25th percentile; variable "nevents" stores the number of transitions within each origin-destination quartile. 
* each transition involes 1 worker and 2 establishments.

preserve					
collapse (count) nevents=dlw (p25) lag2lw lag1lw lw lead1lw lag1lom_lw lom_lw lag1lom_sc lom_sc if event==1, by(fem lag1qlom_lw qlom_lw)
ren lag1qlom_lw originQ
ren qlom_lw destinatQ
mkmat fem originQ destinatQ nevents lag2lw lag1lw lw lead1lw  , matrix(eventout)
matlist eventout,	format(%9.5gc)	///
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Event study results by gender: Wag profiles at 25th percentile)					
restore


* wage profiles at 50th percentile; variable "nevents" stores the number of transitions within each origin-destination quartile. 
* each transition involes 1 worker and 2 establishments.

preserve
collapse (count) nevents=dlw (p50) lag2lw lag1lw lw lead1lw lag1lom_lw lom_lw lag1lom_sc lom_sc if event==1, by(fem lag1qlom_lw qlom_lw)
ren lag1qlom_lw originQ
ren qlom_lw destinatQ
mkmat fem originQ destinatQ nevents lag2lw lag1lw lw lead1lw  , matrix(eventout)
matlist eventout,	format(%9.5gc)	///
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Event study results by gender: Wag profiles at 50th percentile)							
restore

* wage profiles at 75th percentile; variable "nevents" stores the number of transitions within each origin-destination quartile. 
* each transition involes 1 worker and 2 establishments.

preserve
collapse (count) nevents=dlw (p75) lag2lw lag1lw lw lead1lw lag1lom_lw lom_lw lag1lom_sc lom_sc if event==1, by(fem lag1qlom_lw qlom_lw)
ren lag1qlom_lw originQ
ren qlom_lw destinatQ
mkmat fem originQ destinatQ nevents lag2lw lag1lw lw lead1lw  , matrix(eventout)
matlist eventout,	format(%9.5gc)	///
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Event study results by gender: Wag profiles at 75th percentile)				
restore					

cap erase ${data}/eventspells_`1'.dta
cap erase ${data}/stayspells_`1'.dta

end


events 1
events 2


* Part 2 of the event study. Compute 2-way clustered standard errors for firm transitions.
* Code follows CCK, 2015 (dofile check_symmetry.do) by P.Kline.

cap program drop eventsym
program define eventsym

use ${data}/allspells_`1'.dta, clear

* Creating crossid for two-clustering
egen crossid=group(firmid last1firmid)

* Drop variables
keep dlw yr age age2 educ fem last1firmid firmid crossid qlom_lw lag1qlom_lw samefirm

* Rename variables for loop
ren lag1qlom_lw lagqlom 
ren qlom_lw qlom
ren last1firmid lagfirmid

*Loop over all origin destination quartile combinations and gender
*compute point estimates and two-way clustered standard error
*report results in matrix

xi i.yr i.educ i.educ|age i.educ|age2

mat A=J(32,6,.) //empty results matrix
local counter=1

forval f=0/1{	//gender
	forval m=1/4{	//origin
		forval q=1/4{	//destination
			cap drop D
			gen D=0 if samefirm==1&fem==`f'
			replace D=1 if qlom==`q'&lagqlom==`m'&fem==`f'&samefirm==0
			di "Now running `m'-->`q' for female=`f'"
			preserve
				keep if D!=. //restrict sample to verify Oaxaca routine works
				qui x_ols dlw _I* age age2 , bo(D) cluster(lagfirmid)
				local se1=_se[D]
				qui x_ols dlw _I* age age2 , bo(D) cluster(firmid)
				local se2=_se[D]
				qui x_ols dlw _I* age age2 , bo(D) cluster(crossid)
				local se3=_se[D]
				local V=(`se1'^2 + `se2'^2 - `se3'^2)
				di _b[D]
				di sqrt(`V')
				count 
				local N=r(N)
				mat A[`counter',1]=`f'
				mat A[`counter',2]=`m'
				mat A[`counter',3]=`q'
				mat A[`counter',4]=_b[D]
				mat A[`counter',5]=sqrt(`V')
				mat A[`counter',6]=`N'
				local ++counter
			restore
		}
	}
}
mat coln A = Gender OriginQ DestinatQ Determ Clust_StdErr Observations

* Table shows output of x_ols procedure. Last column contains number of person-years.

matlist A, 	format(%9.5gc)	///
			lines(oneline)	///
			tw(5)			///
			showcoleq(c)	///
			linesize(120)	///
			underscore		///
			tit(Event study results by gender)
svmat A
keep A*
ren A1 fem
ren A2 origQ
ren A3 destQ
ren A4 b
ren A5 se

save ${data}/oaxresults_`1', replace

end

eventsym 1
eventsym 2


* Part 3 of event study: compare wage changes of men and women who move between exactly the same employers.

cap program drop dualtrans
program define dualtrans

use ${data}/allspells_`1'.dta if event==1, clear

di _newline "# of event spells : " _continue
count

* get cross-id
egen crossid=group(firmid last1firmid)

* collapse by crossid-fem
collapse (count) movers=dlw (mean) lag2lw lag1lw lw lead1lw dlw dlwr lag1lom_lw lom_lw lag1lom_lwM lom_lwM lag1lom_sc lom_sc (max) lag1qlom_lw qlom_lw, by(crossid fem)
bys crossid : gen dual=_N>1
sum


* each observation represents an OD-gender combination
bys crossid : gen v=_n==1
di _newline "# of OD-combinations in data : " _continue
count if v==1

di _newline "# of OD-combinations in data with both male and female workers involved : " _continue
count if v==1 & dual==1

tab dual if v


* now restrict to dual-gender transitions
keep if dual==1

bys crossid (fem): gen dlw_m=dlw[1]
bys crossid (fem): gen dlw_f=dlw[2]

bys crossid (fem): gen dlwr_m=dlwr[1]
bys crossid (fem): gen dlwr_f=dlwr[2]


* change in male co-worker wages contained in first obs per cross-id
bys crossid (fem): keep if _n==1
gen dlom_lw_m=lom_lwM-lag1lom_lwM

* number of dual-gender transitions
count

* regress change in female wages on change in male wages instrumenting male wage changes
* by the instantaneous change in male coworker wages associated with the transition.

ivregress 2sls dlw_f (dlw_m=dlom_lw_m), first
ivregress 2sls dlwr_f (dlwr_m=dlom_lw_m), first


collapse (sum) movers (count) nevents=crossid (mean) lag2lw lag1lw lw lead1lw dlw dlwr lag1lom_lw lom_lw lag1lom_sc lom_sc, by(fem lag1qlom_lw qlom_lw)
ren lag1qlom_lw originQ
ren qlom_lw destinatQ
ren dlw d4_lw_raw
ren dlwr d4_lw_adj
mkmat fem originQ destinatQ nevents movers lag2lw lag1lw lw lead1lw d4_lw_raw d4_lw_adj , matrix(eventout)
matlist eventout,	format(%9.5gc)	///
					lines(oneline)	///
					tw(5)			///
					showcoleq(c)	///
					linesize(120)	///
					underscore		///
					tit(Event study results by gender)					
	
cap erase ${data}/allspells_`1'.dta

end

dualtrans 1
dualtrans 2


* Part 4 of event study: repeat event study analysis using changes in estimated firm fixed effects.

cap program drop eventakms
program define eventakms

use ${data}/AKM_select_`1'.dta,clear

* Step 1: generate firm level variables

* Approximate years of schooling (taken from CHK) *
gen school=.
replace school=10.5 if educ==0 	// missing, 0
replace school=11   if educ==1	// dropouts/primary
replace school=13	if educ==2	// apprentice
replace school=15	if educ==3	// some college
replace school=18	if educ==4	// university

* Import estimated person effects (from AKMs)
merge m:1 persnr using ${data}/peffs_`1'.dta, keep(1 3) keepus(theta`1') nogenerate
gen connected=theta`1'!=.

* Import estimated firm effects (from AKMs, see matlab code); 2 effects per firm, 1 for each gender; make indicator for dual-connected set
merge m:1 betnr using ${data}/feffs_`1'.dta, keep(1 3) keepus(psi`1'0 psi`1'1) nogenerate
gen dualconnect=(psi`1'0!=. & psi`1'1!=.)

ren ln_impwage lw
keep persnr betnr idnum year lw age educ school fem connected dualconnect psi`1'0 psi`1'1

tab fem connected,mis

* restrict to dual-connected firms
keep if dualconnect==1

gen lwF=fem*lw if fem==1
gen lwM=(1-fem)*lw if fem==0
di _newline "Create firm-year data set with worker-means"
di "Collapse by betnr-year, note that idnum and betnr refer to same firm if in IAB, so no loss of info"
collapse (mean) mlw = lw 	///
				mlwF = lwF	///
				mlwM = lwM	///
				mschool=school	///
		 (sum)	Fempl=fem			///
		 (count) empl=lw			///
		 (firstnm) dual=dualconnect	, by(betnr year)
gen Mempl=empl-Fempl
egen avFempl = mean(Fempl), by(betnr)
egen avMempl = mean(Mempl), by(betnr)
egen avempl  = mean(empl), by(betnr)
egen avmschool=mean(mschool), by(betnr)
gen nofemales=(avFempl==0)
gen nomales=(avMempl==0)

lab data "firm-year data, means by year"
save ${data}/fymeans.dta, replace



/* Load worker-level data and merge firm-level means */

use ${data}/AKM_select_`1'.dta,clear

* Step 1: generate firm level variables

* Approximate years of schooling (taken from CHK) *
gen school=.
replace school=10.5 if educ==0 	// missing, 0
replace school=11   if educ==1	// dropouts/primary
replace school=13	if educ==2	// apprentice
replace school=15	if educ==3	// some college
replace school=18	if educ==4	// university

* Import estimated person effects (from AKMs)
merge m:1 persnr using ${data}/peffs_`1'.dta, keep(1 3) keepus(theta`1') nogenerate
gen connected=theta`1'!=.

* Import estimated firm effects (from AKMs, see matlab code); 2 effects per firm, 1 for each gender; make indicator for dual-connected set
merge m:1 betnr using ${data}/feffs_`1'.dta, keep(1 3) keepus(psi`1'0 psi`1'1) nogenerate
gen dualconnect=(psi`1'0!=. & psi`1'1!=.)

ren ln_impwage lw
keep persnr betnr idnum year lw age educ school fem connected dualconnect psi`1'0 psi`1'1

tab fem connected,mis

* restrict to dual-connected firms
keep if dualconnect==1

* Import firm means
merge m:1 betnr year using ${data}/fymeans.dta, keep(3) nogen

* Compute quartiles of firm fixed effects
forval g=0/1 {
	fastxtile qpsi`1'`g'=psi`1'`g' if fem==`g', nq(4)
}



/*--- Identifying event spells and stayer spells ---*/

* this follows CCK's SAS code as closely as possible.

xtset persnr year

gen firmid=betnr
format firmid %13.0f

sort persnr year

by persnr : gen last1firmid=firmid[_n-1]
by persnr : gen last2firmid=firmid[_n-2]
by persnr : gen next1firmid=firmid[_n+1]
by persnr : gen next2firmid=firmid[_n+2]
by persnr : gen lag1qpsi`1'0=qpsi`1'0[_n-1]
by persnr : gen lag1qpsi`1'1=qpsi`1'1[_n-1]

by persnr : gen lag1lw=lw[_n-1]
by persnr : gen lag2lw=lw[_n-2]
by persnr : gen lag3lw=lw[_n-3]
by persnr : gen lag4lw=lw[_n-4]
by persnr : gen lead1lw=lw[_n+1]
by persnr : gen lead2lw=lw[_n+2]
by persnr : gen lead3lw=lw[_n+3]
by persnr : gen lead4lw=lw[_n+4]

gen jobch=0


/*--- Identify events and job-spell counter per worker ---*/

/* First, sort by persnr year (ascending) */
sort persnr year

*  treat first observation separately
by persnr : gen tenure=1 if _n==1
by persnr : gen startyr=year if _n==1

* ... if firmid!=last1firmid, job is quit
by persnr : replace jobch=1 if firmid!=last1firmid		 // first person-obs is indicated as jobch (intentionally!)
by persnr : gen jobnum=sum(jobch)
by persnr : replace tenure=1 if firmid!=last1firmid
by persnr : replace startyr=year if firmid!=last1firmid & _n>1

* ... if firmid==last1firmid, job continues, so tenure +1, 
bys persnr jobnum (year) : replace tenure=tenure[_n-1]+1 if firmid==last1firmid & _n>1


/* Second, sort by persnr year (descending) */
gsort persnr -year

* treat last observation separately
by persnr : gen yearstogo=1 if _n==1
by persnr : gen endyr=year if _n==1

* ... if firmid!= next1firmid, job has been quit
by persnr : replace yearstogo=1 if firmid!=next1firmid
by persnr : replace endyr=year if firmid!=next1firmid

* ... if firmid== next1firmid, job continues in the future
gen jobnum2=-1*jobnum
gen year2 = -1*year
bys persnr jobnum2 (year2): replace yearstogo=yearstogo[_n-1]+1 if firmid==next1firmid & _n>1
drop jobnum2 year2


/* Obtain year entry, exit, and total # of jobs per worker */
egen firstyr=min(year), by(persnr)
egen lastyr=max(year), by(persnr)
egen numjobs=max(jobnum), by(persnr)


/* Get lagged values of remaining variables */
sort persnr year
foreach v in tenure {
by persnr : gen lag1`v'=`v'[_n-1]
}



/*---	 Event/Stayer spells; selection step-by-spep 	---*/

gen eventspell=0
gen stayspell=0

/* Event spells: jobnum>=2, startyr==year  */
replace eventspell=1 if jobnum>=2 & startyr==year

gen numpre=lag1tenure if eventspell==1
gen numpost=yearstogo if eventspell==1

/* Stay spells: no change @ anytime */
replace stayspell=1 if last2firmid==firmid & last1firmid==firmid & next1firmid==firmid & next2firmid==firmid


/*--- Selecting EVENT spells for analysis (2+ obs pre and 2+ obs post move), no singleton firms ---*/
preserve
di "Event spells"
* eliminate singletons in selection of event spells
keep if eventspell==1 & numpre>=2 & numpost>=2

* Wage change (4 year, leave out latest year)
gen dlw=lead1lw-lag2lw
gen yr=year-2
gen age2 =age*age/100
gen event=1

keep persnr year yr fem age age2 lag1qpsi`1'0 qpsi`1'0 lag1qpsi`1'1 qpsi`1'1 lag2lw lag1lw lw lead1lw dlw event last1firmid firmid
lab data "event spells, no singleton firms, 2+past, 2+future"
save ${data}/eventspells_`1'_ffe.dta,replace
sum
restore


/* Selecting STAY spells for analysis (2+ obs pre and 2+ obs post move), no singleton firms */
preserve

di "Stayspells"
* selection eliminates singletons (Achtung: "assignment von workplaces ist nicht konstant, daher wechseln auch stayer im zeitverlauf")
keep if stayspell==1

* Wage change (4 year, leave out latest year, only used for selection)
gen dlw=lead1lw-lag2lw
gen yr=year-2
gen age2 =age*age/100
gen event=0

keep persnr year yr fem age age2 lag1qpsi`1'0 qpsi`1'0 lag1qpsi`1'1 qpsi`1'1 lag2lw lag1lw lw lead1lw dlw event last1firmid firmid
lab data "stay spells, no singleton firms, 2+past, 2+future"
save ${data}/stayspells_`1'_ffe.dta,replace
sum
restore


/*--- Analyse event and stay spells ---*/

use ${data}/eventspells_`1'_ffe.dta, clear
append using ${data}/stayspells_`1'_ffe.dta

/* Final table with all results */
forval g=0/1 {
preserve
	collapse (count) nevents=dlw (mean) lag2lw lag1lw lw lead1lw dlw if event==1 & fem==`g', by(fem lag1qpsi`1'`g' qpsi`1'`g')
	ren lag1qpsi`1'`g' originQ
	ren qpsi`1'`g' destinatQ
	ren dlw d4_lw_raw
	mkmat fem originQ destinatQ nevents lag2lw lag1lw lw lead1lw d4_lw_raw , matrix(eventout)
	matlist eventout,	format(%9.5gc)	///
						lines(oneline)	///
						tw(5)			///
						showcoleq(c)	///
						linesize(120)	///
						underscore		///
						tit(Event study results by gender: Mean wage profiles of workers moving between quartiles of gender-specific firm fixed effects )
	restore
}
cap erase ${data}/eventspells_`1'_ffe.dta
cap erase ${data}/stayspells_`1'_ffe.dta
cap erase ${data}/fymeans.dta
end


eventakms 1
eventakms 2



cap log close
