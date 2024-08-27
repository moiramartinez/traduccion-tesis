* 	AUTOR: 	Benjamin Bruns
*	INHALT:	Aufbereitung der Personendaten des LIAB-Mover Models 2 9308 und
*			Generierung von Variablen.
*	Sample: Person-data
*	Aggregation: none, individual level
*	Aggregationsebenen der zugespielten Daten:
*	1. Zusammenfassung von Kreisen zu 402 Regionen;
*	2. Zusammenfassung Nationalität zu 15 Kategorien;
*	3. Zusammenfassung von 3-Steller-Berufsklassifikation
*	4. Zusammenspielen mit CPI-Deflator
*	5. Zusammenspielen mit IAB-BP
*	STAND: 01.06.2015

clear all
capture log close
set linesize 120
set more off, perm
cap program drop _all
adopath ++${prog}
set seed 1000

log using ${log}\03_prepare_ieb.log, text replace
timer on 1
di c(current_date)
di c(current_time)

* Loading IEB data (person-spells)
use persnr betnr idnum begepi endepi frau gebjahr nation bild grund tentgelt 	///
gleitz beruf stib erwstat estatvor quelle spell level1	///
jahr betr_st tage_erw ein_bet tage_bet ein_job tage_job anz_lst	///
tage_lst2 lohn8tv estat2_8tv tage_estat2_8tv using ${orig}\liab_mm_9308_v1_pers.dta, clear

lab def dummy 0 "No" 1 "Yes"

* Import BHP data
sort betnr jahr
merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis.dta, keepus(betnr jahr w93_3_gen az_ges ao_kreis te_med)
drop _merge

* Saving Labels (Fehler in JoSuA)
*lab save _all using ${log}/origlab.do, replace


* Preparation procedures

	* Rename variables
	rename begepi adate
	rename endepi edate
	format adate %dD_m_CY
	format edate %dD_m_CY
	rename frau fem

	* Year variable
	rename jahr year
	lab var year "Year"

	* Age variable
	gen age = year-gebjahr
	lab var age "Age"
	tab age, m

	keep if quelle ==1	 	/* Keep BeH spells for analysis, no unemployment spells */
	tab level1 				/* level1>0 indicates parallel employment spell */

	* Parallel job spells? I deal with this below.
	gen mjob = level1>0	
	lab var mjob "Multiple Jobs/Income sources"		
	keep if level1==0	// Drop all parallel spells: Keep only "Hauptbeschäftigung" in BeH
	*tab iabid, mis		// If the parallel employment spell is in an establishment with valid idnum,
						// but the main employment spell is not, this procedure might reduce
						// the number of available idnum. B

	* Distribution of IAB-plants among betnr across years
	tab year betr_st , mis

	* Recode all missing values to "."
	recode betnr idnum nation bild gleitz beruf stib erwstat estatvor lohn8tv  ///
	estat2_8tv tage_estat2_8tv te_med ao_kreis (mis=.)

	* Plausibility checks
	assert(adate<=edate)
	assert(year(adate)==year(edate))
	assert(edate-adate<=365)

	* Define new spell variable
	drop spell
	bys persnr (adate quelle): gen spell = _n	/* New Spell-counter; Ordering has not been changed with respect to "geringfügiger Beschäftigung"  */
	by persnr : gen nspell = _N
	lab var nspell "Number of spells per persnr"
	tab nspell


* Some imputations: industry, nationality, kreis, occupation, occupation status

	* Industry: impute modal industry of firm to all firm-years
	* Interpolation at firm-level (time-invariant)
	rename w93_3_gen w93_3

	bys betnr : egen modInd = mode(w93_3), maxmode	/* Mode industry-classification by betnr */
	tab modInd if modInd!=. & w93_3==.
	bys betnr : replace w93_3 = modInd

	drop modInd

	* Merge "similar" industries with few observations:
	recode w93_3 (603=602) (183=182) (120 132=131) (15=14)


	* Nationality: 
		
		* Impute mode nationality of person to all person-spells
		count if nation==.
		egen mnation=mode(nation), by(persnr) maxmode
		replace nation=mnation
		di as res "Remaining missings in nationality:"
		count if nation==.
		lab var nation "nationality (mode)"
		drop mnation
		
		* Create dummy variable for German / non-German nationality
		gen germnat = (nation==0)
		label var germnat "Nationality"
		label define germnat_lb 0 "Non-German" 1 "German"
		label values germnat germnat_lb
		replace germnat=. if nation==.

	* District: 
		
		* Impute mode district by betnr
		count if ao_kreis==.
		ren ao_kreis kreis
		egen mkreis=mode(kreis), by(betnr) maxmode
		gen kreis_old=kreis
		replace kreis=mkreis
		lab var kreis "region (mode)"
		lab var kreis_old "ao_kreis (original)"

		* Remove invalid regions
		keep if kreis>=1000
		lab val kreis
		count if kreis==.
		cap drop mkreis
		
		* Dummy for West Germany
		gen byte west=0
		replace west=1 if kreis <=11100		/* Note: 11100 corresponds to "Berlin, West-Stadt" */
		replace west=. if missing(kreis)
		lab var west "Dummy for working in West Germany"
		lab def westlb 0 "East" 1 "West"
		lab val west westlb


	* Occupation: mode occupation within employer-match
	di as res "Missing occupation information"
	count if beruf==.
	bys betnr persnr year: egen mberuf = mode(beruf), maxmode
	*bys betnr persnr year: replace berufc = modBeruf
	gen beruf_old=beruf
	replace beruf = mberuf
	cap drop mberuf
	lab var beruf "occupation, 3-digit (mode)"


	* Occupation status (= STIB); simple forward-backward induction
	gen stibc = stib
	gen spell2=(-1)*spell
	bys persnr (spell) : replace stibc = stibc[_n-1] if missing(stibc)
	bys persnr (spell2): replace stibc = stibc[_n-1] if missing(stibc)

	tab stibc quelle, m
	replace stib = stibc

	cap drop berufc stibc first

* Import SSC-max and define censoring dummy

	* SSC-max
	gen upbound=.
	replace upbound=47.06442 if year==1975 & west==1 & upbound==.
	replace upbound=51.9677 if year==1976 & west==1 & upbound==.
	replace upbound=57.15221 if year==1977 & west==1 & upbound==.
	replace upbound=62.19354 if year==1978 & west==1 & upbound==.
	replace upbound=67.23999 if year==1979 & west==1 & upbound==.
	replace upbound=70.40489 if year==1980 & west==1 & upbound==.
	replace upbound=73.96349 if year==1981 & west==1 & upbound==.
	replace upbound=79.00482 if year==1982 & west==1 & upbound==.
	replace upbound=84.04617 if year==1983 & west==1 & upbound==.
	replace upbound=87.17016 if year==1984 & west==1 & upbound==.
	replace upbound=90.76965 if year==1985 & west==1 & upbound==.
	replace upbound=94.13395 if year==1986 & west==1 & upbound==.
	replace upbound=95.81609 if year==1987 & west==1 & upbound==.
	replace upbound=100.5813 if year==1988 & west==1 & upbound==.
	replace upbound=102.5396 if year==1989 & west==1 & upbound==.
	replace upbound=105.8988 if year==1990 & west==1 & upbound==.
	replace upbound=109.2631 if year==1991 & west==1 & upbound==.
	replace upbound=113.9925 if year==1992 & west==1 & upbound==.
	replace upbound=121.0279 if year==1993 & west==1 & upbound==.
	replace upbound=127.7514 if year==1994 & west==1 & upbound==.
	replace upbound=131.1157 if year==1995 & west==1 & upbound==.
	replace upbound=134.1118 if year==1996 & west==1 & upbound==.
	replace upbound=137.8392 if year==1997 & west==1 & upbound==.
	replace upbound=141.1984 if year==1998 & west==1 & upbound==.
	replace upbound=142.8805 if year==1999 & west==1 & upbound==.
	replace upbound=144.169 if year==2000 & west==1 & upbound==.
	replace upbound=146.2448 if year==2001 & west==1 & upbound==.
	replace upbound=147.95 if year==2002 & west==1 & upbound==.
	replace upbound=167.67 if year==2003 & west==1 & upbound==.
	replace upbound=168.85 if year==2004 & west==1 & upbound==.
	replace upbound=170.96 if year==2005 & west==1 & upbound==.
	replace upbound=172.6 if year==2006 & west==1 & upbound==.
	replace upbound=172.6 if year==2007 & west==1 & upbound==.
	replace upbound=173.77 if year==2008 & west==1 & upbound==.
	replace upbound=177.53 if year==2009 & west==1 & upbound==.
	replace upbound=180.82 if year==2010 & west==1 & upbound==.
	replace upbound=45.38738 if year==1990 & west==0 & upbound==.
	replace upbound=57.15221 if year==1991 & west==0 & upbound==.
	replace upbound=80.46712 if year==1992 & west==0 & upbound==.
	replace upbound=89.09261 if year==1993 & west==0 & upbound==.
	replace upbound=99.17529 if year==1994 & west==0 & upbound==.
	replace upbound=107.5809 if year==1995 & west==0 & upbound==.
	replace upbound=113.9925 if year==1996 & west==0 & upbound==.
	replace upbound=119.3457 if year==1997 & west==0 & upbound==.
	replace upbound=117.6687 if year==1998 & west==0 & upbound==.
	replace upbound=121.0279 if year==1999 & west==0 & upbound==.
	replace upbound=119.0236 if year==2000 & west==0 & upbound==.
	replace upbound=122.7101 if year==2001 & west==0 & upbound==.
	replace upbound=123.29 if year==2002 & west==0 & upbound==.
	replace upbound=139.73 if year==2003 & west==0 & upbound==.
	replace upbound=142.62 if year==2004 & west==0 & upbound==.
	replace upbound=144.66 if year==2005 & west==0 & upbound==.
	replace upbound=144.66 if year==2006 & west==0 & upbound==.
	replace upbound=149.59 if year==2007 & west==0 & upbound==.
	replace upbound=147.54 if year==2008 & west==0 & upbound==.


	* Censoring dummy
	gen cens=0
	replace cens=1 if tentgelt>=round(upbound)
	lab var cens "Censoring dummy SSC-max"

	* Cap tentgelt at censoring bound, strict
	gen dwage=tentgelt
	replace dwage=round(upbound) if tentgelt>=round(upbound)


* Import CPI-deflator and deflate wages
	
	* CPI-deflator
	gen D_CPI=.
	replace D_CPI=50.08555 if year==1975
	replace D_CPI=52.23752 if year==1976
	replace D_CPI=54.1657 if year==1977
	replace D_CPI=55.62066 if year==1978
	replace D_CPI=57.90998 if year==1979
	replace D_CPI=61.04891 if year==1980
	replace D_CPI=64.91031 if year==1981
	replace D_CPI=68.329 if year==1982
	replace D_CPI=70.5675 if year==1983
	replace D_CPI=72.26666 if year==1984
	replace D_CPI=73.8438 if year==1985
	replace D_CPI=73.75149 if year==1986
	replace D_CPI=73.92993 if year==1987
	replace D_CPI=74.87154 if year==1988
	replace D_CPI=76.95159 if year==1989
	replace D_CPI=79.02659 if year==1990
	replace D_CPI=81.91365 if year==1991
	replace D_CPI=86.11435 if year==1992
	replace D_CPI=89.965 if year==1993
	replace D_CPI=92.29871 if year==1994
	replace D_CPI=93.93232 if year==1995
	replace D_CPI=95.21587 if year==1996
	replace D_CPI=97.08285 if year==1997
	replace D_CPI=98.01633 if year==1998
	replace D_CPI=98.59977 if year==1999
	replace D_CPI=100 if year==2000
	replace D_CPI=101.9837 if year==2001
	replace D_CPI=103.3839 if year==2002
	replace D_CPI=104.5508 if year==2003
	replace D_CPI=106.1844 if year==2004
	replace D_CPI=107.9347 if year==2005
	replace D_CPI=109.5683 if year==2006
	replace D_CPI=112.1354 if year==2007
	replace D_CPI=115.0525 if year==2008

	* Compute Real Wages
	gen rwage     = dwage / (D_CPI/100)
	label var rwage "Real daily wage"


	* Dummy for trainees
	gen ausb=0
	replace ausb=1 if (year<1999 & stib==0) | (year>=1999 & inlist(erwstat, 102, 105, 106, 141))
	lab var ausb "1 if trainee"


* Initial sample restrictions

	* Drop marginal jobs for comparability over time
	drop if inlist(erwstat, 109, 110, 202, 209, 210)

	* restrict to full-time
	keep if stib<=4

	* age b/w 15 and 64
	keep if age >=15 & age<=64

	* only West Germany
	keep if west==1


	* Exclude irrelevant occupations:
	drop if   beruf==555 	/* Behinderte */						///
			| beruf==666	/* Rehabilitanden */					///
			| beruf==888 	/* Pflegeperson */						///
			| beruf==924 	/* Mit Haushaltssscheckverfahren gem.*/ ///
			| beruf==971	/* Mithelfende FamAngehörige */			///		/* Keep Azubis and Interns, but create dummy for them. */
			| beruf==981	/* Trainees with recognized training */	///		/* Mobility may be particularly high for that group.   */
			| beruf==982	/* Interns, unpaid trainees	*/			///
			| beruf==983	/* Arbeitskräfte arbeitssuchend */		///
			| beruf==991	/* Arbeitskräfte ohne Angabe */			///
			| beruf==995	/* Vorruhestand, Alterübergangsgeld */	///
			| beruf==996	/* ATZ */									///
			| beruf==997	/* Ausgleichsgeldbezieher */



	compress
	save ${data}\temp1_iebprep, replace

use ${data}\temp1_iebprep.dta, clear



* Collapse multiple person-firm-records within a year into a single person-firm-year observation.
* Construction based on the definition in Card, Heining, and Kline (2013).

egen spellct = tag(persnr year betnr adate)
levelsof fem, l(lev)
  foreach x of local lev {
	 qui sum spellct if fem==`x'
	 di as text "`: label (fem) `x'' --> Number of person-firm-records : " as result `r(sum)'
}

* Store labels to reattach after collapse
#d ;
local varlist "beruf w93_3 bild stib ausb gebjahr erwstat rwage tage_erw tage_bet anz_lst tage_job age germnat
			idnum fem nation betr_st ein_job az_ges te_med kreis cens";
#d cr

foreach x in `varlist' {

di "`x'"
local l`x' : variable label `x'
if `"`l`x''"' == "" {
local l`x' "`x'"
}
}

* Length of employment spells:
gen days = edate-adate+1

gen spellinc = days*rwage

gen ct=1	// keep track of multiple person-firm combinations within a year

collapse	(sum) days spellinc	///
			(rawsum) ct		///
			(max) beruf ausb w93_3 bild stib gebjahr erwstat tage_erw tage_bet anz_lst tage_job age germnat cens kreis nation ///
			(firstnm) idnum fem betr_st ein_job az_ges te_med  ///
			, by(persnr betnr year) fast


egen spellct = tag(persnr year betnr)
levelsof fem, l(lev)
  foreach x of local lev {
	 qui sum spellct if fem==`x'
	 di as text "`: label (fem) `x'' --> Number of person-firm-records : " as result `r(sum)'
 }


gen rwage = spellinc/days


* Select main income spell
gsort persnr year -spellinc
bys persnr year : keep if _n==1

drop spellinc

* Drop inactive spells and spells with implausibly low wages (<10 EUR real);
* Do this AFTER collapse, i.e. on avg wages within person-firm spell within a year.
drop if rwage<10


* Re-attach labels
foreach x in `varlist' {
label var `x' "`l`x''"
}
lab var ct "Person-firm matches w/n year"


* Check if everything makes sense:
bys year : sum days
assert(days<=366)


* Keep only complete observations:
drop if missing(beruf, stib, w93_3, rwage)

tab betr_st, m

* Drop vartiables without further use to save space; können später wieder rangespielt werden.
keep beruf ausb w93_3 bild stib gebjahr erwstat tage_erw tage_bet anz_lst tage_job age germnat cens ///
	idnum fem nation betr_st ein_job az_ges te_med kreis persnr betnr year rwage days

compress
save ${data}/temp2_iebprep.dta, replace


use ${data}/temp2_iebprep.dta, clear

* Log real daily wages
gen lnrwage = ln(rwage)
lab var lnrwage 	"Log real daily wage"


* Indicator for IAB-BP firm
gen liab = idnum!=.
cap drop _merge

* Generate Bundesland for all observations (so far only in IAB)
cap drop bula
gen bula = int(kreis/1000)

tab bula, m


* Temporary variables for imputation of bildung:
gen quelle=1
bys persnr : gen spell=_n
bys persnr : gen nspell = _N

qui compress
save ${data}/ieb.dta, replace

forval x =1/2 {
	cap erase ${data}/temp`x'_iebprep.dta
}

* next step: impute "bild"
timer off 1
timer list 1

di c(current_date)
di c(current_time)

cap log close




