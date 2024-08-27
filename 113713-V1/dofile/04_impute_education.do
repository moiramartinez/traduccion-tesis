* 	AUTOR: 	Bernd Fitzenberger, Aderonke Osikominu, Robert Voelter
*	INHALT:	Programm zur Verbesserung der Bildungsvariable
*			Zur Verfügung gestellt von Bernd Fitzenberger.
*	STAND: 	30.04.2015

clear all
capture log close
set linesize 255
set more off, perm

log using ${log}\04_impute_education.log, text replace

* Load LIAB Personendaten
use ${data}/ieb.dta, clear

/***********************************************************************************
This program creates the improved education variable IP1 used in the paper
"Imputation Rules to Improve the Education Variable in the IAB Employment Supsample"
 by Bernd Fitzenberger, Aderonke Osikominu and Robert Voelter. The data is the IABS
 regional file 1975-1997.
(c) 2005 Bernd Fitzenberger, Aderonke Osikominu and Robert Voelter
Goethe-Universitaet Frankfurt
************************************************************************************/

/*

This procedure generates the improved education variable IP1. The basic
rule is to impute a spell with the highest degree reported for this person
before this spell if information at this spell is missing or a lower degree is reported.

program structure

Section 1
All employment spells are accepted as basis for the extrapolation rule. Information
from not accepted non employment spells will be treated as missing. At a later stage
information will be extrapolated to these spells. Young persons by assumption have no degree.

Only for persons with all education information missing the program imputes a
vocational training degree if certain employment statuses are reported.

Section 2
The extrapolation rule extrapolates degrees from the accepted spells to later spells if
there is missing information or a lower degree until there is an accepted spell with a
higher degree.

Only if a person's first spells have missing information the program extrapolates
backwards educational information from the persons first accepted spell with non
missing information to the previous spells with missing information.

Section 3
Ensure the same education information at parallel spells.
*/

*clear
*set mem 1000m

capture program drop Educcorrect
program define Educcorrect

rename persnr pnr

* Nur ein Missing Wert für Bildung:
**************************************
bys pnr (spell): replace bild = -9 if bild==.n | bild==.z | bild==.

* Name der korregierten Bildungsvariable
********************************************
gen bildk1 = 0

* Umgekehrter Spellzähler
*******************************
gen spell2 = spell * (-1)

**********************************************************************
* BILDUNGSKORREKTUR
**********************************************************************

*****************************************
* Section 1
* acceptance of spells
*****************************************
* Sonderregelung für junge Leute
* if age below 18 any formal education (2 to 6) is implausible
* impute "no formal education" (1)
bys pnr (spell): replace bildk1 = 1 if age[_n]<18
tab bildk1

* Definiere valide Bildungsinformationen
* --> Nur sozial-versicherungspflichtige Employmentspells
* Fortgeschriebene Informationen bei LE-Bezug und Informationen bei
* geringügiger Beschäftigung werden als invalide gekennzeichnet
******************************************************************
gen byte valid1=(quelle==1 & 1<=bild & bild<=6)

* Valide Informationen werden übernommen:
by pnr (spell), sort: replace bildk1=bild if valid1==1 & age>=18

* For persons with education information missing at all spells:
* Impute "vocational tranining degree" if the employment status (STIB=2,3)
* indicates a qualified job at the respective employment spells.

egen nmis=sum(bild==-9), by(pnr)
generate dmis= (nmis==nspell)
drop nmis

bys pnr (spell): replace bildk1=2 if 2<=stib & stib<=3 & valid1==0 & dmis==1
drop dmis

/*****************************************
*   Section 2
*   Extrapolation
*****************************************
Part 1: extrapolation to following spells

Extrapolate education information to following spells with lower
education information or missing information. Note that persons who have
professional education (2) and high school (3) have both, which
has to be reported as (4). Hence the extrapolation is done stepwise.
1) Extrapolate "no degree" to following missings
2) Extrapolate "professional degree, 2" to following missings and "no degree, 1"
3) Extrapolate "high school, 3" to following missings and "no degree, 1". Do
not extrapolate 3 to 2. 3 is not higher than 2. 2 and 3 mean having both, 4.
4) Impute "high school & professional degree, 4" if "high school, 3" is reported
and "professional degree, 2" has been reported before or vice versa. Extrapolate
"high school & professional degree, 4" to following missings and spells with lower
education.
5) Extrapolate "technical college, 5" to following missings and lower degrees
6) Extrapolate "university, 6" to following missings and lower degrees*/

sort pnr spell

by pnr: replace bildk1 = 1 if _n>1 & bildk1[_n-1]==1 & bildk1[_n]<1

by pnr: replace bildk1=2 if _n>1 & bildk1[_n-1]==2 & bildk1[_n]<2

by pnr: replace bildk1=3 if _n>1 & bildk1[_n-1]==3 & bildk1[_n]<2

by pnr: replace bildk1=4 if _n>1 & (bildk1[_n-1]==4 | (bildk1[_n-1]==2 & bildk1[_n]==3) | (bildk1[_n-1]==3 & bildk1[_n]==2)) & bildk1[_n]<4


by pnr: replace bildk1=5 if _n>1 & bildk1[_n-1]==5 & bildk1[_n]<5

by pnr: replace bildk1=6 if _n>1 & bildk1[_n-1]==6 & bildk1[_n]<6


/**********************************************
*Backwards extrapolation to previous spells

The extrapolation rule above has eliminated all missing values for a person after
the first spell with education information. But possibliy the
first spell(s) still have missing values. Backwards extrapolate information
from the first spell with non missing education information for a person to
previous spells which have no information yet. Do not extrapolate below education
specific age limits:
university: 27 years
technical college: 25 years
both professional education and high school: 23 years
only high school: 21 years
only professional education: 20 years
no education degree: no age limit
*************************************************/
#del;
*gen bildk2 = bildk1;

by pnr (spell2), sort: replace bildk1=6 if _n>1 & (bild[_n]==-9 | bildk1[_n]==0)
                                                & bildk1[_n-1]==6 & age[_n]>=27 ;

by pnr (spell2), sort: replace bildk1=5 if _n>1 & (bild[_n]==-9 | bildk1[_n]==0)
                                                & bildk1[_n-1]==5 & age[_n]>=25 ;

by pnr (spell2), sort: replace bildk1=4 if _n>1 & (bild[_n]==-9 | bildk1[_n]==0)
                                                & bildk1[_n-1]==4 & age[_n]>=23 ;

by pnr (spell2), sort: replace bildk1=3 if _n>1  & (bild[_n]==-9 | bildk1[_n]==0)
                                                & bildk1[_n-1]==3 & age[_n]>=21 ;

by pnr (spell2), sort: replace bildk1=2 if _n>1 & (bild[_n]==-9 | bildk1[_n]==0)
                                                & bildk1[_n-1]==2 & age[_n]>=20;

by pnr (spell2), sort: replace bildk1=1 if _n>1 & (bild[_n]==-9 | bildk1[_n]==0)
                                                & bildk1[_n-1]==1;


#delimit cr

/*******************************
*   Section 3
Require multiple spells (parallel spells at the same time, STYP=2) to have the
same education information.
Identification through the same beginning date.
Impute highest of the parallel education information.
*********************************/


	/* AUSKOMMENTIERT: ICH HABE KEINE PARALLELEN SPELLS MEHR IM DATENSATZ */

*sort pnr adate spell
*by pnr adate (spell): egen maxbild=max(bildk1)
*by pnr adate (spell): replace bildk1=maxbild if bildk1~=maxbild & maxbild>0.5
*drop maxbild
*sort pnr spell

* Missing value

replace bildk1=-9 if bildk1==0

gen high=1 if bildk1==5 | bildk1==6
replace high=0 if high==. & bildk1!=-9

gen medium=1 if bildk1==2 | bildk1==4
replace medium=0 if medium==. & bildk1!=-9

gen low=1 if bildk1==1 | bildk1==3
replace low=0 if low==. & bildk1!=-9

gen educ=.
replace educ=1 if low==1
replace educ=2 if medium==1
replace educ=3 if high==1

rename pnr persnr

lab def bildk1_lb 	1 "no degree" 		///
					2 "vocational degree, no high school" 		///
					3 "no vocational degree, high school"				///
					4 "vocational degree and high school"				///
					5 "technical college / Fachhochschule"				///
					6 "university"
lab val bildk1 bildk1_lb

* Zunächst: Generieren der Variable: POTENTIAL EXPERIENCE
/* Age of entry in the labor force is assumed to be:
	16	for individuals without vocational education
	19	for individuals without A-level with vocational training or with A-level and without vocational education
	21	for individuals with A-level and cvocational education
	24	non-university higher education
	25	workers with university degree
*/
gen experience=0
replace experience = age - 16 if bildk1==1	/*no degree*/
replace experience = age - 19 if bildk1==2	/*vocational degree, no high school*/
replace experience = age - 19 if bildk1==3	/*no vocational degree, high school*/
replace experience = age - 21 if bildk1==4	/*vocational degree, high school*/
replace experience = age - 24 if bildk1==5	/*technical college*/
replace experience = age - 25 if bildk1==6	/*university*/
replace experience = 0 if experience <0

save ${data}/ieb_impeduc.dta, replace

end

* Run program
Educcorrect




