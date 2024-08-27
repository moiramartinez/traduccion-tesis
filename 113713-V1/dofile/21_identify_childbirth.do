clear all
cap log close
set linesize 120
set more off, perm
cap program drop _all
log using ${log}/21_identify_childbirth.log, replace
adopath ++${prog}
set seed 1000
set matsize 11000

* This programme identifies childbirth in administrative data. I use a cascade of *
* - reason of notification
* - age at notification
* - age at birth
* - duration between two consecutive births

* Load spell data
cap confirm f ${orig}\liab_mm2_9308_v1_pers.dta
if _rc==0 use ${orig}\liab_mm2_9308_v1_pers, clear
if _rc>0 use ${orig}\liab_mm_9308_v1_pers, clear
gen year=year(begepi)
ren frau fem
ren tentgelt wage
ren grund reason

* Set globals
global minage_birth = 18
global maxage_birth = 40
global maxage_announce=38


* Restricting years
ta year
keep if year>=1993 & year<=2008
ta year

* Keep females only
keep if fem==1

* Replace single payments by 0 (could occur prior to return to labour market).
gen wage2=wage
replace wage2=0 if reason==54

* Find start of potential maternity protection period
tab quelle reason if reason==51 | reason==2015 | reason==4005 | reason==6010
gen start=0 if reason==51 | reason==2015 | reason==4005 | reason==6010

* Restrict potential births to childbearing age (see above)
gen jahr_begepi=year(begepi)
gen alterszaehler = jahr_begepi-gebjahr
replace start = . if alterszaehler > $maxage_birth & start == 0 & (reason==51 | reason==2015 | reason==4005 | reason==6010) 

* Consider only main employment spells
gsort persnr begepi quelle -wage betnr 
cap drop level1
bysort persnr begepi: gen byte level1 = _n-1

* Generating a variable containing start date of maternity protection
cap drop help_start help_startm
sort persnr level1 start begepi
gen help_start=begepi if start==0 & level1==0
format %td help_start 

* Count start spells
sort persnr level1 start begepi
bysort persnr: gen zaehler=_n if start==0 & level1==0 // numbering the rows start==0
tab zaehler

* Announcement must be before maxage_announce
gen help_gr=1 if alterszaehler>$maxage_announce & start==0 & zaehler==1 & quelle==1
bysort persnr: egen help_grm=max(help_gr)
count if help_grm==1 & persnr~=persnr[_n+1]
replace start=. if help_grm==1
cap drop zaehler
sort persnr level1 start begepi
bysort persnr: gen zaehler=_n if start==0 & level1==0

* If there are parallel spells, impute start date of first spell
sort persnr level1 start begepi  
replace help_start=. if start==0 & start[_n+1]==0 & persnr[_n+1]==persnr & level1==0 & begepi[_n+1]-1<=endepi+7 & quelle[_n+1]==1
replace help_start=. if start==0 & start[_n+1]==0 & persnr[_n+1]==persnr & level1==0 & begepi[_n+1]-1<=endepi+13*7 & quelle~=1 & quelle[_n+1]~=1
bysort persnr: egen help_startm=min(help_start)
format %td help_startm 

* Redefine counter
replace start=. if help_start==.
cap drop zaehler
sort persnr level1 start begepi
bysort persnr: gen zaehler=_n if start==0 & level1==0

* Identify end of maternity leave
sort persnr level1 begepi
by persnr: replace start=1 if begepi>endepi[_n-1] & (wage2!=0 & wage2<=. ) & start~=0 & level1==0 & quelle==1 
by persnr: replace start=. if endepi<help_startm

* Mark out work interruption period for potential maternity leave periods
cap drop help
cap drop help_start_n
gen help_start_n=1 if start==0 | start==1
sort persnr quelle help_start_n level1 begepi
gen help=zaehler 
bysort persnr quelle help_start_n level1: replace help=help[_n-1] if help==. & help[_n+1]!=help[_n-1] & level1==level1[_n-1] 

* pro Startzeitpunkt fuer potentielle Mutterschaft wird der Zeitraum bis zur naechsten Mutterschaft gesetzt
sort persnr quelle help level1 begepi
cap drop help_startm
bysort persnr quelle help level1: egen help_startm=min(help_start)
format %td help_startm

cap drop luecke
gen luecke=(begepi[_n+1]-(endepi)) if start~=. & start[_n+1]~=. & start~=start[_n+1] & start==0 & persnr==persnr[_n+1] & help==help[_n+1]
gen eyear=year(begepi[_n+1]) if start~=. & start[_n+1]~=. & start~=start[_n+1] & start==0 & persnr==persnr[_n+1] & help==help[_n+1]
gen ayear=year(endepi) if start~=. & start[_n+1]~=. & start~=start[_n+1] & start==0 & persnr==persnr[_n+1] & help==help[_n+1]
gen emonth=month(begepi[_n+1]) if start~=. & start[_n+1]~=. & start~=start[_n+1] & start==0 & persnr==persnr[_n+1] & help==help[_n+1]
gen amonth=month(endepi) if start~=. & start[_n+1]~=. & start~=start[_n+1] & start==0 & persnr==persnr[_n+1] & help==help[_n+1]

* Checking the interruption of work, if it's illness or maternity protection a gap of 98 days is maternity protection
* Omit employment during first 6 weeks
count if luecke<98 
replace start=. if luecke<98 & start==0

* Generating the variable of birth
* Exit + 6 weeks (42 days) if employment
* Exit + 6 weeks if maternity payment by the Federal Employment Agency
cap drop geburt
gen geburt=endepi+42 if start==0 & quelle==1 & level1==0
replace geburt=endepi+42 if start==0 & quelle~=1 & level1==0
format %td geburt


* Generating leave duration in months
gen durmonth = eyear*12+emonth - (ayear*12+amonth)
drop eyear emonth ayear amonth
su durmonth, det

* Gap between the births
sort persnr geburt
cap drop luecke_kind
cap drop zaehler
bysort persnr: gen zaehler=_n if geburt~=.
tab zaehler
gen luecke_kind=geburt-geburt[_n-1] if geburt~=. & persnr==persnr[_n-1] 

* Checking how often the gap between the children is less than 281 respectively 224 days 
sum luecke_kind if luecke_kind<281, d //40th week
sum luecke_kind if luecke_kind<224,d //32th week

* If the gap is smaller than 224 days and it is an announcement of the employer, the birth is deleted
* The gap is too small and it could be supposed, that the announcement is because of illness
* The measurement of gaps is made in order of the children, because too many births would be deleted
replace geburt=. if luecke_kind<224 & reason==51 & zaehler==2
ta zaehler
local z=r(r)
foreach num of numlist 3/`z'{
	cap drop zaehler luecke_kind
	bysort persnr: gen zaehler=_n if geburt~=.
	gen luecke_kind=geburt-geburt[_n-1] if geburt~=. & persnr==persnr[_n-1] 
	sum luecke_kind if luecke_kind<281, d //40th week
	sum luecke_kind if luecke_kind<224,d //32th week
	replace geburt=. if luecke_kind<224 & reason==51 & zaehler==`num'
	}
	
* We let the children before the 32th week of pregnancy in, if the announcement is given by the Federal Employment Agency, because the maternity payment is an explicit identifier
* Furthermore we identify the birth by the fee payment compensation. The time children are born before the calculated date is unknown and 
* has to be involved for the calculation of the following child

*number of children
cap drop kind_anz
sort persnr geburt
bysort persnr: gen kind_anz=_n if geburt~=. 
tab kind_anz
su kind_anz, det

gen kind_anz_help=kind_anz
replace kind_anz_help=0 if kind_anz_help==.
bys persnr (begepi) : gen kind_anz2=sum(geburt>0 & geburt<.)

gen mother=kind_anz2>0

* mean number of mothers and children by year
table jahr, c(mean kind_anz2 mean mother)
table jahr if mother==1, c(mean kind_anz2)

* mean age of mothers at birth of first child by year
table jahr if kind_anz==1, c(mean alterszaehler min alterszaehler max alterszaehler) format(%9.3gc)

* age of the mothers at birth of the child
table kind_anz, c(mean alterszaehler) format(%9.3gc)


* the notification 'childcare leave' is not taken because employer notify this information if mothers take the last year of the parental leave later (possible until 8 birthday of child)

bysort persnr: egen kind_max=max(kind_anz)

drop wage2 start alterszaehler zaehler help_start help_startm help luecke luecke_kind

tab kind_max if persnr~=persnr[_n+1], m 
gen mutter=1 if kind_max~=.
replace mutter=0 if mutter==.
tab mutter if persnr~=persnr[_n+1]

tab gebjahr mutter if persnr~=persnr[_n+1], row


* added:

* Restrict to first birth spell 
keep if kind_anz==1

* Re-build person-year panel starting from "age at birth of first child"
duplicates report persnr
gen birthmonth=month(geburt)
gen age = jahr-gebjahr
gen childbirth=1
ren jahr year
keep persnr year age birthmonth childbirth durmonth
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

ta durmonth, mis
ta k, mis

compress
save ${data}/childbirth2, replace



cap log close
