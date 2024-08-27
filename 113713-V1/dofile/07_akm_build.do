clear all
set linesize 255
cap log close
set more off, perm
log using ${log}/07_akm_build.log, replace

/*
	Autor: Benjamin Buns, fdz1213

	Content: Prepare sample for AKM-analysis. Input is the main data set with unit of observation being the person-year.
			 Selection: full-time, age>=20, no missings in education, age, wage, isco, female.
			 Use two overlapping periods: 1993-2001 and 2000-2008.

*/

/****** SETTING ******/
local from1		= 1995
local to1		= 2001
local from2		= 2001
local to2		= 2008

local agemin	= 20
local agemax	= 60

local minwage	= 10

/*
/* Loading the main file and clean up... */
use ${data}/masterfile.dta ,clear
des
foreach v in stib erwstat tage_erw anz_lst tage_job tage_lst2 adate ein_erw ein_bet ein_job lohn8tv estat2_8tv tage_estat2_8tv te_med germnat rwage spellct spell spells spellearn nspell spell2 valid1 ed0 ed1 ed2 ed3 ed4 agec1 agec2 agec3 agec4 agec5 agec6 agegroup largefirm lnrwage mlnrwage mcens ilnrwage icens obs1 obsfirm1 tradables {
cap drop `v'
}
compress
des
save ${data}/masterfile, replace
*/

use persnr idnum betnr days fem age year fsize beruf tage_bet ln_impwage liab educ ausb ind w93_3 bula using ${data}/masterfile,clear

* Merge Anzahl VZ aus BHP; az_ges hat Strukturbruch in 1999, daher ungeeignet für per capita values.
gen jahr=year
merge m:1 betnr jahr using ${orig}\liab_mm_9308_v1_btr_basis.dta, keepus(az_ges_vz) nogen update
drop jahr
preserve
bys betnr year : keep if _n==1
keep betnr year az_ges_vz
bys betnr (year) : gen avfsize=(az_ges_vz+az_ges_vz[_n-1])/2
bys betnr (year) : replace avfsize=az_ges_vz if _n==1
save ${data}/fsize_btr, replace
restore

* Merge avfsize, use as current firm size
merge m:1 betnr year using ${data}/fsize_btr, keepus(avfsize) nogen

sum az_ges_vz avfsize fsize, det
drop az_ges_vz
ren fsize fsize_break_99
ren avfsize fsize
table year , c(mean fsize mean fsize_break_99) format(%9.2fc)
corr fsize fsize_break_99
gen post99=year>=1999

* Trend break regressions:

* For all establishment IDs:
******************************
* All
reg fsize fsize_break_99, cluster(betnr)
reg fsize post99##c.fsize_break_99, cluster(betnr)

* Men
reg fsize fsize_break_99 if fem==0, cluster(betnr)
reg fsize post99##c.fsize_break_99 if fem==0, cluster(betnr)

* Women
reg fsize fsize_break_99 if fem==1, cluster(betnr)
reg fsize post99##c.fsize_break_99 if fem==1, cluster(betnr)

* Gender-pooled with dummy interaction
reg fsize fem##c.fsize_break_99, cluster(betnr)
reg fsize fem##post99##c.fsize_break_99, cluster(betnr)


* For all EP establishments only:
**********************************
* All
reg fsize fsize_break_99, cluster(idnum)
reg fsize post99##c.fsize_break_99, cluster(idnum)

* Men
reg fsize fsize_break_99 if fem==0, cluster(idnum)
reg fsize post99##c.fsize_break_99 if fem==0, cluster(idnum)

* Women
reg fsize fsize_break_99 if fem==1, cluster(idnum)
reg fsize post99##c.fsize_break_99 if fem==1, cluster(idnum)

* Gender-pooled with dummy interaction
reg fsize fem##c.fsize_break_99, cluster(idnum)
reg fsize fem##post99##c.fsize_break_99, cluster(idnum)




* Import data from IAB Betriebspanel:
merge m:1 idnum year using ${data}/2_LIAB_mm_IAB_prepare.dta, keep(1 3) keepus(geschvol antvorl geschart)
cap drop _merge
ren geschart typeofsales
ren geschvol revenues
ren antvorl shareinputs
lab var typeofsales "type of business vol. (edited)"
lab var revenues "sales (edited)"
lab var shareinputs "share of inputs in sales"
ta typeofsales, mis 

* ISCO-88 Occupation classification
gen isco =.
Aggocc, newvar(isco) oldvar(beruf)



** Initial Sample Restrictions

	* Estimation periods:
	gen interval1=(year>=`from1' & year<=`to1')
	gen interval2=(year>=`from2' & year<=`to2')

	* Drop year <1995
	drop if year<`from1'

	* Drop workers below 20 and above 60:
	drop if age<`agemin' | age>`agemax'

	* Drop workers earning less than 10 Euro a day; if still in.
	drop if ln_impwage<ln(`minwage')

	* Drop trainees
	drop if ausb==1


* Generate firm-profitability
replace shareinputs=. if shareinputs>.99 | shareinputs<.01
gen rev=revenues*10000
gen va=(1-shareinputs)*rev

di _newline "Make profitability variables real, world bank GDP deflator, rescaled to base 2000"
foreach v in rev va {

	*replace `v'=`v'/0.9474232 if year==1993
	*replace `v'=`v'/0.9679503 if year==1994
	replace `v'=`v'/0.9870185 if year==1995
	replace `v'=`v'/0.9931172 if year==1996
	replace `v'=`v'/0.9955576 if year==1997
	replace `v'=`v'/1.0014900 if year==1998
	replace `v'=`v'/1.0046620 if year==1999
	replace `v'=`v'/1.0000000 if year==2000
	replace `v'=`v'/1.0127854 if year==2001
	replace `v'=`v'/1.0263863 if year==2002
	replace `v'=`v'/1.0388818 if year==2003
	replace `v'=`v'/1.0501606 if year==2004
	replace `v'=`v'/1.0566991 if year==2005
	replace `v'=`v'/1.0598610 if year==2006
	replace `v'=`v'/1.0777900 if year==2007
	replace `v'=`v'/1.0869260 if year==2008

}

* Drop missings:
foreach v in age educ isco fem ln_impwage tage_bet w93_3 ind bula {
	di "Observations dropped due to missings in : `v' "
	drop if missing(`v')
}


* Drop if isco code is invalid (-9 for beruf==981)
drop if isco<0

cap drop days empl

/* Additional variables: tenure, sic2 */
gen tenure=tage_bet/365
gen sic2=int(w93_3/10)
lab var tenure "Yrs with employer"
lab var sic2 "2-digit industry"


* Firm surplus measures:
* I could follow two routes here. 
*  1. calculate average surplus for all years (1995-2008) in the sample and split the sample afterwards:
*	> only exit/entry of old/new firms changes the mean surplus distribution
*  2. calculate average surplus within each period (1995-2001, 2001-2008) so that the mean surplus of a 
* 	firm can change in concordance with the firm effect.
* Number 2 is more in line with my analysis. However, for some descriptives that display annual time series 
* values, I use mean values calculated over the pooled sample (1995-2008).

* The following routine builds the AKM estimation samples.

cap program drop makesamples
program define makesamples

	preserve

		keep if interval`1'

		di _newline "Define measures of firm surplus"
		gen vapw=va/fsize/1000
		gen revpw=rev/fsize/1000

		sum vapw revpw
		di "Set surplus measure =. if type-of-sales is not revenues"
		di "type-of-sales is constant at firm-level, so truncation excludes entire firms, not just firm-years"
		foreach s in vapw revpw {
			di "`s'"
			replace `s'=. if typeofsales>1
		}


		gen lVApw=ln(vapw)
		gen lrevpw=ln(revpw)

		di _newline "Summarize profitability measures, non-trimmed"
		sum revpw vapw lrevpw lVApw shareinputs typeofsales

		di _newline "Make firmsize constant within interval..."
		count if fsize_break_99==.
		count if fsize==.
		count if fsize==. & fsize_break_99!=.
		replace fsize=0 if fsize==. & fsize_break_99>0
		egen mfsize=mean(fsize), by(betnr)
		replace fsize=mfsize
		drop mfsize
		drop if missing(fsize)

		di _newline "Trim profitability measures (<1 and >99)"
		qui sum year
		local yrmin=r(min)
		local yrmax=r(max)
		foreach v in lrevpw lVApw {
			forval y=`yrmin'/`yrmax' {
			di "`v' : `y'"
				qui sum `v' if year==`y', detail
				replace `v'=. if (`v'<=r(p1) | `v'>=r(p99)) & year==`y'
			}
		}


		di _newline "Make profitability measures constant w/n firm interval"
		foreach v in lrevpw lVApw {
			di "`v'"
			egen av`v'=mean(`v'), by(betnr)		//using idnum is inconsequential, because idnum and betnr are always filled simultaneously. Also it is consistent with the using mean productivity for at the firm level (and we are talking here about the same firm)
		}

		di _newline "Rename profitability variables for analysis..."
		ren avlrevpw REV
		ren avlVApw VA


		di _newline "Trim outliers of VA and REV"
		foreach v in VA REV {

			_pctile `v' if `v'!=., p(95)
			di "Trimming if `v' >= 95% : " r(r1)
			gen `v'trim95=`v' if `v'<r(r1)
			gen `v'wins95=`v' 
			replace `v'wins95=r(r1) if `v'>=r(r1) & `v'<.

			_pctile `v' if `v'!=., p(1 99)
			di "Trimming if `v' <= 1% : " r(r1) " and >= 99%: " r(r2) 
			gen `v'trim1and99=`v' if `v'>r(r1) & `v'<r(r2)
			gen `v'wins1and99=`v' 
			replace `v'wins1and99=r(r1) if `v'<=r(r1)
			replace `v'wins1and99=r(r2) if `v'>=r(r2) & `v'<.

		}

		sum REV REVtrim95 REVwins95 REVtrim1and99 REVwins1and99 VA VAtrim95 VAwins95 VAtrim1and99 VAwins1and99, det

		qui su year
		local min=r(min)
		local max=r(max)

		di _newline "Pink (mainly female) / Blue (mainly male) occupation; use 3-digit"
		egen fshare_occ = mean(fem), by(beruf)
		egen fshare_occ_avpers = mean(fshare_occ), by(persnr)
		sum fshare_occ, detail
		gen pink=1     if fshare_occ_avpers>=r(p50) & fshare_occ_avpers!=.
		replace pink=0 if fshare_occ_avpers<r(p50)  & fshare_occ_avpers!=.
		ta fem pink, mis row

		qui {
		bys beruf : gen countoccs=_n==1
		egen median_fshare_occ=median(fshare_occ)		// fshare_occ is constant at occ-level, so might not be exactly at 50%
		gen pinkocc=fshare_occ>=median_fshare_occ
		}

		di _newline "# of occupations that are female-dominated"
		ta pinkocc if countoccs

		/*
		sum fshare_occ
		gen pink2 =1 if fshare_occ_avpers>=r(mean) & fshare_occ_avpers!=.
		replace pink2=0 if fshare_occ_avpers<r(mean)  & fshare_occ_avpers!=.
		ta fem pink2, mis row

		sum fem
		gen pink3 =1 if fshare_occ_avpers>=r(mean) & fshare_occ_avpers!=.
		replace pink3=0 if fshare_occ_avpers<r(mean)  & fshare_occ_avpers!=.
		ta fem pink3, mis row
		*/
		cap drop pinkocc median_fshare_occ countoccs


		qui sum age
		local age = "`r(min)'-`r(max)'"
		qui sum year
		local year = "`r(min)'-`r(max)'"
		qui sum ln_impwage
		local minwage = round(r(min),0.0001)
		lab data "M/W, `age', no trainees, `year', >`minwage' Eur"

		drop ausb fsize_break_99 post99 revenues typeofsales  fshare_occ fshare_occ_avpers
		compress

		des
		save ${data}/AKM_select_`1'.dta, replace

	restore
	
end

makesamples 1
makesamples 2



/* Save for AKMs */
/*
cap program drop toakm
program define toakm

use persnr betnr ln_impwage educ year age fsize fem pink tenure using ${data}/AKM_select_`1'.dta, clear
lab data "for AKM analysis"
save ${data}/AKM_r`1'.dta, replace

end

toakm 1
toakm 2
*/


cap erase ${data}/tmpall.dta

cap log close
***CENSORED***exit




