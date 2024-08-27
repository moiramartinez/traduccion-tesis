* 	Benjamin Bruns (fdz1213)
* 	Generate additional variables/covariates for analysis and imputation of wages.
*	Sample:	Worker-level; no aggregation to firm-level; all variables are constructed w/o conditioning on IAB-BP

clear all
set more off, perm
set linesize 120
cap log close

log using ${log}/05_addvars.log, replace

di c(current_date)
di c(current_time)

* Load IEB-data
use ${data}/ieb_impeduc.dta, clear

* Check if bildk1 has still missings; code as additional category
count if bildk1==. | bildk1<0 | bildk1==.z | bildk1==.n
replace bildk1=0 if bildk1==. | bildk1<0 | bildk1==.z | bildk1==.n

* Distribution of educ groups
tab bildk1, mis nola
tab bildk1, mis

* Generate edgroups consistent with CHK using mputation of Fitzenberger as pre-step
cap drop educ
gen educ=0 if bildk1==0 // missing (around 1 mio)
replace educ=1 if bildk1==1 // no degree (around 4.8 mio), dropouts
replace educ=2 if bildk1==2 // vocational (around 23.5 mio), no high school
replace educ=3 if bildk1==3 | bildk1==4 // high school w/ or w/o vocational (around 2.7 mio)
replace educ=4 if bildk1==5 | bildk1==6 //polytech/university (around 5 mio)

* Generate education dummies (5)
tab educ, mis
forval ed=0/4 {
	gen ed`ed'=educ==`ed'
}

* Construct age groups; use CHK definition, add below 20 and above 60, but in separate 
* categories; include 60 in 2nd-to-last group
cap drop agegroup
gen agec1=age<20
gen agec2=(age>=20 & age<30)
gen agec3=(age>=30 & age<40)
gen agec4=(age>=40 & age<50)
gen agec5=(age>=50 & age<=60)
gen agec6=age>60
gen agegroup=agec1*1+agec2*2+agec3*3+agec4*4+agec5*5+agec6*6

gen fsize=az_ges
gen fsize2=fsize^2

* Dummy for large establishments
gen largefirm = (fsize>10)

* Mean wage of coworkers
egen mlnrwage=mean(lnrwage), by(persnr)
egen mcens=mean(cens), by(persnr)
bys persnr : gen ilnrwage=(mlnrwage*_N-lnrwage)/(_N-1)
bys persnr : gen icens=(mcens*_N-cens)/(_N-1)

* Indicator for single-year job-spells
bys persnr : gen obs1=_N==1
bys betnr : gen obsfirm1=_N==1

* Industry Classifications :
egen sic3=mode(w93_3), by(betnr)
di as res _newline "Main industry categories..."
gen ind=.
replace ind = 1 if sic3<=50
replace ind = 2 if (sic3>=101 & sic3<=103) | (sic3>=111 & sic3<=112) | (sic3==120) | (sic3>=131 & sic3<=132) | (sic3>=141 & sic3<=145)
replace ind = 3 if (sic3>=201 & sic3<=205) | (sic3>=231 & sic3<=233) | (sic3>=251 & sic3<=252)
replace ind = 4 if (sic3>=241 & sic3<=247)
replace ind = 5 if (sic3>=271 & sic3<=275) | (sic3>=281 & sic3<=287) | (sic3>=291 & sic3<=297)
replace ind = 6 if (sic3>=300 & sic3<=316) | (sic3>=321 & sic3<=323) | (sic3>=331 & sic3<=335) | (sic3>=341 & sic3<=343) | (sic3>=351 & sic3<=355)
replace ind = 7 if (sic3>=151 & sic3<=160) | (sic3>=171 & sic3<=177) | (sic3>=181 & sic3<=183) | (sic3>=191 & sic3<=193) | (sic3>=211 & sic3<=212) ///
					| (sic3>=221 & sic3<=223) | (sic3>=261 & sic3<=268) | (sic3>=361 & sic3<=366) | (sic3>=371 & sic3<=372)
replace ind = 8 if (sic3>=551 & sic3<=555)
replace ind = 9 if (sic3>=511 & sic3<=517) | (sic3>=521 & sic3<=527)
replace ind = 10 if (sic3>=501 & sic3<=505)
replace ind = 11 if (sic3>=451 & sic3<=455)
replace ind = 12 if (sic3>=601 & sic3<=603) | (sic3>=611 & sic3<=612) | (sic3>=621 & sic3<=623) | (sic3>=631 & sic3<=634) | (sic3>=641 & sic3<=642)
replace ind = 13 if (sic3>=651 & sic3<=652) | (sic3==660) | (sic3>=671 & sic3<=672) | (sic3>=701 & sic3<=703) | (sic3>=711 & sic3<=714) | (sic3>=721 & sic3<=726) ///
					| (sic3>=731 & sic3<=732) | (sic3>=741 & sic3<=748)
replace ind = 14 if (sic3>=401 & sic3<=403) | (sic3==410) | (sic3==900) | (sic3>=911 & sic3<=913) | (sic3>=921 & sic3<=927) | (sic3==930) | (sic3==950)
replace ind = 15 if (sic3>=801 & sic3<=804) | (sic3>=851 & sic3<=853)
replace ind = 16 if (sic3>=751 & sic3<=753) | (sic3==990)
lab def ind 1 "Agric, hunting, forestry, fishing" 2 "Energy, mining" 3 "Prod of rubber and plastic products" 4 "Chemical industry" ///
			5 "Metal production and processing" 6 "Automotive, production of data processing" 7 "Consumer goods" 8 "Hospitality industry" ///
			9 "Sales (retail/wholesale)" 10 "Maintenance, repair of motore vehicle" 11 "Building industry" 12 "Transport and communication" ///
			13 "Credit and insurance intermediation" 14 "Public and personal services" 15 "Education, social and health care" 16 "Public admin, social security", modify
lab val ind ind
cap drop sic3

* Define tradables
cap drop tradables
gen tradables = (w93_3>=51 & w93_3<=366)	 /*((bran>=2 & bran<=10) | bran>=12 & bran<=18) */
lab var tradables "Tradable industries"
lab def trade 0 "Non-tradable" 1 "Tradable", modify
lab val tradables trade

save ${data}/masterfile.dta, replace

keep year educ agegroup cens lnrwage fem obs1 ilnrwage icens largefirm fsize fsize2 persnr 
save ${data}/export4imputation.dta, replace

di c(current_date)
di c(current_time)

cap log close






