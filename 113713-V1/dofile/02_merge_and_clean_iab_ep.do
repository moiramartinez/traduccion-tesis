* Benjamin Bruns (fdz1213)
* Aufbereitung des LIAB-Mover Models 9308
* 1. Zusammenspielen und Recodings
* 2. ggf. Interpolation/Imputation von fehlenden Werten
* 3. Generieren von zusätzlichen Variablen
* 4. Einige deskriptive Statistiken
* teilw. DoFile vom FDZ des IAB (Stata Panel Syntax)
* 	Aggregation: none; unit of observation is firm.
* I include a vast number of variables though not all will be used in the analysis.
* Some code adapted from Mittag (2015)


cap log close
clear all
log using ${log}\02_merge_and_clean_iab_ep.log, replace

* Zusammenspielen der vorbereiteten Wellen
local y=2008

while `y'>1992 {
append using ${data}\iabbp_`y'_panel_test.dta
local y=`y'-1
}
 
sort idnum jahr

keep idnum jahr bula hrf_quer ges_vor gesamt geschvol invest tarif inhaber teilzeit einstell entlass betrrat betr_and single form ///
eigentum  bran_n00 ges_frau weiterb west grjahr fhire d_grow svb geschart tech ertlag ertrlagv bran_n99 export ict antvorl_vor uebtarif ///
modgewbe sonderz kapbeteil altvers gewbeteil sonstson uebtarif* oeffnklau ausstarif
rename jahr year

* Label variables
label variable  bula		"Bundesland"
label variable  west 		"West/Ost"
label variable  year		"Befragungsjahr"
label variable  ges_vor		"Gesamtzahl Beschäftigter Vorjahr"
label variable  gesamt		"Gesamtzahl Beschäftigter"
label variable  svb			"Sozialversicherungspflichtig Beschäftigte"
label variable  geschart	"Art des Geschäftsvolumen"
label variable  geschvol	"Geschäftsvolumen"
label variable  invest		"Investitionen"
label variable  tech		"Beurteilung des technischen Standes der Anlagen"
label variable  tarif		"Geltung Tarifvertrag"
label variable  inhaber		"Tätige Inhaber insgesamt"
label variable  teilzeit	"Teilzeitbeschäftigte insgesamt"
label variable  einstell	"Neueinstellungen insgesamt"
label variable  entlass		"Anzahl Ausgeschiedene insgesamt"
label variable  betrrat		"Betriebs-/Personalrat"
label variable	betr_and 	"andere betriebsspezifische Form der Mitarbeitervertretung"
label variable  single		"Betrieb/Dienststelle ist ..."
label variable  form		"Rechtsform"
label variable  bran_n99	"Brancheneinteilung bis 1999"
label variable  bran_n00	"Brancheneinteilung ab 2000"
label variable  ges_frau	"Anzahl beschäftigte Frauen insgesamt"
label variable  weiterb		"Finanzierung Weiterbildungsmaßnahmen"
label variable  ertlag		"Ertragslage aktuelles Geschäftsjahr"
label variable  grjahr		"Gründungsjahr ab 1990"
label variable  ertrlagv 	"Ertragslage im letzten Geschäftsjahr"
label variable hrf_quer		"Hochrechnungsgewicht für Querschnittsdaten"
label variable d_grow           "Erwartete Entw. Gesch.vol im aktuellen Jahr"
label variable fhire		"Wollten einstellen, haben aber nicht"
label variable export		"Positiver Exportanteil im vergangenen GJ"
label variable ict 			"ICT-Investment"
label variable antvorl_vor	"Anteil Vorleistungen an Vorjahresumsatz"
label variable uebtarif 	"über Tarif (wenn tarifgebunden)"
label variable modgewbe 	"Modell zur Gewinn-/Kapitalbeteiligung"
label variable sonderz		"Sonderzahlungen"
label variable kapbeteil	"Kapitalbeteiligung"
label variable altvers 		"betriebliche Altersversorgung"
label variable gewbeteil	"Gewinnbeteiligung"
label variable sonstson		"sonstige Sonderzahlungen"
label variable uebtarif_unan "übertarifl. Bez. für unang. MA"
label variable uebtarif_fach "übertarifl. Bez. für Fachangest."
label variable uebtarif_ange "übertarifl. Bez. für Angelernte"
label variable uebtarif_qual "übertarifl. Bez. für qualifizierte MA"
label variable oeffnklau 	"Öffnungsklausel genutzt"
label variable ausstarif	"aussertarifl Angestellte"



* Recode binary variables
recode fhire betrrat weiterb (2=0)

label define fhirelb 0 "kein Bedarf/Einstellungen" 1 "Bedarf, keine Einstellung"
label value 	fhire fhirelb


****** CORRECTIONS / IMPUTATIONS / EDITING OF VARIABLES

*** Missing values:
* Recode missing values
recode geschvol d_grow gesamt ges_vor invest inhaber ges_frau teilzeit fhire einstell entlass weiterb tarif betrrat ///
form single grjahr bran_n99 bran_n00 tech ertlag ertrlagv geschart svb antvorl_vor uebtarif modgewbe sonderz kapbeteil ///
altvers gewbeteil sonstson uebtarif_unan uebtarif_fach uebtarif_ange uebtarif_qual oeffnklau ausstarif (-9 -8=.)
recode d_grow (4=.)
recode eigentum (-9 -8=6)

*** Branch classification:
* Create consistent branch classification
recode bran_n00 (12=10) (21=14) (18=6) (23=22) (40=38)
bysort idnum: egen bran=mode(bran_n00), maxmode
recode bran_n99 (3=7) (4=8) (5=9) (6 7=10) (8=13) (9=14) (10=15) (11=16) (12=17) (13=6) (14=5) (15=4) (16=3) (17=19) (18=20) (19=22) (20=24) (21=26) (22=27) (23=33) (24 28=35) (26=34) (27=37) (29=30) (30 34=32) (32=30) (33=36) (36 37=39) (39 40=41) (25 35=38)
bysort idnum: egen br99=mode(bran_n99), maxmode
replace bran=br99 if bran==.
drop br99 bran_n99 bran_n00

*** State variables:
* Pool West Berlin and Berlin
recode bula (0=11)
* Allocate establishments with bula=18 to either Saarland or Rheinland-Pfalz
gen saar=10 if bula==10
replace saar=7 if bula==7
bysort idnum: egen saar2=mode(saar), minmode
replace bula=saar2 if bula==18 & saar2!=.
replace bula=7 if bula==18
drop saar saar2


/* Redate variables reported for last year */
ren export export_vor
ren geschvol geschvol_vor
ren geschart geschart_vor
bys idnum (year) : gen antvorl=antvorl_vor[_n+1]
bys idnum (year) : gen export=export_vor[_n+1]
bys idnum (year) : gen geschvol=geschvol_vor[_n+1]
bys idnum (year) : gen geschart=geschart_vor[_n+1]

/* Get the 2008 value of antvorl from 2009 sample */
preserve
	use ${orig}\iabbp_2009.dta, clear
	gen antvorl2008=q10/100 if q10>=0
	gen export2008=q08c/100 if q08c>=0
	gen geschvol2008=q07 if q07>=0
	gen geschart2008=q06 if q06>=0
	keep idnum antvorl2008 export2008 geschvol2008 geschart2008
	recode antvorl2008 export2008 geschvol2008 geschart2008 (-9 -8 = .)
	save ${data}/iab_tmp_antvorl.dta, replace
restore

/* Get the 2008 antvorl_vor from the 2009 file (vorjahr) */
merge m:1 idnum using ${data}\iab_tmp_antvorl.dta
replace antvorl=antvorl2008 if year==2008
replace export=export2008 if year==2008
replace geschvol=geschvol2008 if year==2008
replace geschart=geschart2008 if year==2008
cap drop _merge antvorl2008 geschvol2008 export2008 geschart2008

/* Interpolate missings, and fill corners */
bys idnum : ipolate antvorl year , g(i_antvorl) epolate
replace i_antvorl=round(i_antvorl, .0001)
bys idnum : ipolate export year , g(i_export) epolate
replace i_export=round(i_export, .0001)
bys idnum : ipolate geschvol year , g(i_geschvol) epolate
replace i_geschvol=round(i_geschvol, .01)

* Make geschart constant:
egen mgeschart=min(geschart), by(idnum)
tab mgeschart geschart, mis
replace geschart=mgeschart
drop mgeschart

*** Revenues and Investment:
* Change units of geschvol and invest to 10.000s
replace geschvol=geschvol/10000
replace invest=invest/10000


*** Correct missing values if possible and sensible:
* 	Variables: female workers, investment, parttime, and tech
* Help variables for sorting:
bys idnum (year) : gen spID = _n		/* Establishment spell */
gen spID2 = (-1)*spID					/* Cannot use bys idnum (-spID), hence need to construct help var for -spID */
egen tagID = tag(idnum)
tab year tagID						/* Number of new firms in the sample*/

* 1. Female workers (= GES_FRAU)
gen ges_frau2 = ges_frau	/* help variable */
bys idnum (spID) : replace ges_frau2 = round((ges_frau2[_n+1]+ges_frau2[_n-1])/2 ,1) ///	/* Replace gaps by average value of previous and following year. */
	if ges_frau2==. & !missing(ges_frau2[_n+1]) & !missing(ges_frau2[_n-1])
bys idnum (spID) : replace ges_frau2 = ges_frau2[_n-1] ///						// Replace current value by past value if current val.
	if ges_frau2[_n-1]!=. & ges_frau2==.											// is missing and past is available; chronological sorting.
bys idnum (spID2) : replace ges_frau2 = ges_frau2[_n-1] ///						/* As before, but inverse chronological sorting. */
	if ges_frau2[_n-1]!=. & ges_frau2==.	
replace ges_frau2=0 if ges_frau2==.												/* Code any remaining missing val's as 0 */
replace ges_frau = ges_frau2 if ges_frau==.					
drop ges_frau2

* 2. Investment  (= INVEST)
gen invest2 = invest
bys idnum (spID) : replace invest2 = (invest2[_n+1]+invest2[_n-1])/2 	///
	if invest2==. & !missing(invest2[_n+1]) & !missing(invest2[_n-1])	
bys idnum (spID) : replace invest2 = invest2[_n-1]	///				/* Chronological sorting */
	if invest2==. & !missing(invest2[_n-1])
bys idnum (spID2) : replace invest2 = invest2[_n-1]	///				/* Inverse chronological sorting */
	if invest2==. & !missing(invest2[_n-1])
replace invest2 = 0 if invest2==.									/* Code remaining missing vals as 0 (standard assumption) */
replace invest = invest2 if invest==.
drop invest2

* 3. Parttime workers (= TEILZEIT)
gen teilz2 = teilzeit
bys idnum (spID) : replace teilz2 = round((teilz2[_n+1]+teilz2[_n-1])/2, 1) 	///
	if teilz2==. & !missing(teilz2[_n+1]) & !missing(teilz2[_n-1])	
bys idnum (spID) : replace teilz2 = teilz2[_n-1]	///				/* Chronological sorting */
	if teilz2==. & !missing(teilz2[_n-1])
bys idnum (spID2) : replace teilz2 = teilz2[_n-1]	///				/* Inverse chronological sorting */
	if teilz2==. & !missing(teilz2[_n-1])
replace teilz2 = 0 if teilz2==.									/* Code remaining missing vals as 0 (standard assumption) */
replace teilzeit = teilz2 if teilzeit==.
drop teilz2

* 4. Technological state of machines (= TECH); range=[1,5]
gen tech2 = tech
bys idnum (spID) : replace tech2 = int((tech2[_n+1]+tech2[_n-1])/2) ///		// Replace by "weakly better" state of machines:
	if tech2==. & !missing(tech2[_n+1]) & !missing(tech2[_n-1])	&	///		// Use average if earlier stage was 2 integers better (smaller value)
	tech2[_n+1]>=tech2[_n-1]												// than the next periods state. Use the integer value of the average																		
																			// if the gap is of size 1 or 3
																			// (e.g. 1 in 1999 and 4 in 2001 => 2 in 2000)
bys idnum (spID) : replace tech2 = tech2[_n-1] ///							// If future value is smaller, i.e. machines get better,
	if tech2==. & !missing(tech2[_n+1]) & !missing(tech2[_n-1]) & ///		// then I interpolate the past value for the current value
	tech2[_n+1]<tech2[_n-1]													// assuming that the establishment uses its machines until they					
																			// are renewed next period.
bys idnum (spID) : replace tech2 = tech2[_n-1] ///							/* Extrapolate last value by second-to-last */
	if tech2==. & !missing(tech2[_n-1])																										
bys idnum (spID2) : replace tech2 = tech2[_n-1] ///							/* Extrapolate first value by second value */
	if tech2==. & !missing(tech2[_n-1])					

replace tech2 = 6 if tech2==.			/* Additional category for "unknown" */
replace tech = tech2 
lab def techlab 1 "state-of-the-art" 2 "rather new" ///
3 "average" 4 "rather old" 5 "out-of-date" 6 "unknown"
lab val tech techlab
drop tech2

drop spID spID2 tagID


* Dummy if owner(s) work(s) in firm
gen work_owner=inhaber>0
replace work_owner=0 if inhaber==.	// If missing, assume no owner is working

* Make some variables constant within firms
sort idnum
gen export_old=export
gen invest_old=invest
by idnum: egen bran_c=mode(bran), maxmode
by idnum: egen bula_c=mode(bula), maxmode
by idnum: egen form_c=mode(form), minmode
by idnum: egen eigentum_c=mode(eigentum), minmode
by idnum: egen grjahr_c=mode(grjahr), minmode
by idnum: egen single_c=mode(single), minmode
by idnum: egen weiterb_c=mode(weiterb), maxmode
by idnum: egen betrrat_c=mode(betrrat), maxmode
by idnum: egen tarif_c=mode(tarif), minmode
by idnum: egen work_owner_c=mode(work_owner), maxmode
by idnum: egen export_c=mean(export)
by idnum: egen invest_c=mean(invest)
by idnum: egen uebtarif_c=min(uebtarif)
by idnum: egen uebtarif_unan_c=min(uebtarif_unan)
by idnum: egen uebtarif_fach_c=min(uebtarif_fach)
by idnum: egen uebtarif_ange_c=min(uebtarif_ange)
by idnum: egen uebtarif_qual_c=min(uebtarif_qual)
by idnum: egen modgewbe_c=max(modgewbe)
by idnum: egen sonderz_c=max(sonderz)
by idnum: egen kapbeteil_c=max(kapbeteil)
by idnum: egen altvers_c=max(altvers)
by idnum: egen gewbeteil_c=max(gewbeteil)
by idnum: egen sonstson_c=max(sonstson)
by idnum: egen ausstarif_c=min(ausstarif)
by idnum: egen oeffnklau_c=min(oeffnklau)



lab def singlab 1 "Single-enterprise company" 2 "Branch/branch office" ///
3 "Headquarters/headoffice" 4 "Intermediate instance"
lab val single singlab

* Cross tabulations of edited & unedited variable (to make sure there are not "too many" off-diagonal elements)
* There are quite a few, mostly because of eliminated missings.
foreach x in bran bula form eigentum grjahr single weiterb betrrat tarif work_owner {
tab `x' `x'_c, mis
}

* Rename variables and store labels
foreach x in bran bula form eigentum grjahr single weiterb betrrat tarif work_owner export invest uebtarif uebtarif_unan uebtarif_fach uebtarif_ange uebtarif_qual modgewbe sonderz kapbeteil altvers gewbeteil sonstson ausstarif oeffnklau{
local l`x' : variable label `x'
if `"`l`x''"' == "" {
local l`x' "`x'"
}
replace `x'=`x'_c
drop `x'_c
}



/***************************************\
	CREATE ADDITIONAL VARIABLES
\***************************************/

* index of number of variables that indicate scope for firm-level rent-sharing

* first combine a few variables:
gen duebtarif=(uebtarif==1 | uebtarif==3 | uebtarif_unan==1 | uebtarif_fach==1 | uebtarif_ange==1 | uebtarif_qual==1)
gen dgewbet=(modgewbe==1 | gewbeteil==1 | kapbeteil==1)
gen dsonst=(sonstson==1 | sonderz==1 | altvers==1)
recode ausstarif oeffnklau (2=0)

* total of 5 variables; firms are associated with single value
egen nkeyvars=rowtotal(duebtarif dgewbet dsonst ausstarif oeffnklau)
tab nkeyvars, mis 
tab year nkeyvars, mis
tab year nkeyvars, mis nofreq row

* Revenue per worker
gen rev_pw=geschvol/gesamt
* Investment per worker
gen inv_pw=invest/gesamt

* Deflate nominale variables using the Producer Price Index (OECD Stat Extract, accessed 4th Oct 2015)
* Index 2010=100, by main sectors (manufacturing, industrial activities, mining and quarrying activities, manufacture of food products, energy)


* Get counts of missing values for continuous variables
foreach x in hrf_quer gesamt geschvol rev_pw inv_pw {
tab `x' if `x'==., mis
}

* Replace geschvol, rev_pw & inv_pw by categorical variables (get details for now to see if categories below make sense)
bys year: sum geschvol rev_pw inv_pw, d

* Create categorical variables from inv_pw, geschvol, rev_pw
foreach x in geschvol rev_pw inv_pw {
display `x'
xtile d_`x'=`x' if `x'>0 [pw=hrf_quer], nquantiles(10)	/* Missings are ignored */
replace d_`x'=11 if d_`x'==.					/* Assign category 11 to missing */
_pctile `x' if `x'>0 [pw=hrf_quer], nquantiles(10)	/* Missings are ignored*/
return list
local r1=round(r(r1),0.01)
local r9=round(r(r9),0.01)
label def d`x'lb 1 "0< `x' < `r1'" 10 "`r9' < `x'" 11 "`x' missing"
foreach y of numlist 1/8 {
local z=`y'+1
local r1=round(r(r`y'),0.01)
local r2=round(r(r`z'),0.01)
label def d`x'lb `z' "`r1'< `x' <`r2'", add
}
label val d_`x' d`x'lb
tab d_`x', mis
}

* Share of parttime employment
gen share_pt=teilzeit/gesamt

* Dummy if firm uses parttime employment
gen use_pt = (!missing(teilzeit) & teilzeit>0)	/* Dummy if plant uses part-time employment */
lab def use_pt_lab 0 "No" 1 "Yes"
lab val use_pt usept_lab

* Share of female employment
gen share_fem=ges_frau/gesamt
replace share_fem = 1 if share_fem>1

* Employment growth index & dummy if employment is growing
gen DHSindex=2*(gesamt-ges_vor)/(gesamt+ges_vor)
gen emp_grd=(gesamt-ges_vor)>=0
replace emp_grd=. if gesamt==. | ges_vor==.

* Dummies if firm is hiring/firing
gen hiring=einstell>0
replace hiring=0 if einstell==.
gen firing=entlass>0
replace firing=. if entlass==.

* Get counts of missing values for continuous variables
foreach x in hrf_quer gesamt geschvol rev_pw inv_pw share_fem share_pt {
tab `x' if `x'==., mis
}


* Label new variables
label var rev_pw "Geschaeftsvolumen pro Arbeiter"
label var share_pt "Anteil Teilzeitarbeiter"
label var inv_pw "Investitionen pro Arbeiter (in 10000 Euro)"
label var share_fem "Anteil Frauen"
label var work_owner "mind. ein Inhaber arbeitet im Betrieb"
label var emp_grd "Mehr Arbeiter als im Vorjahr"
label var DHSindex "DHS employment growth index"
label var hiring "Hat Arbeiter eingestellt"
label var firing "Hat Arbeiter entlassen"
label var bran "Brancheneinteilung"
label var geschvol "Geschaeftsvolumen in 10000 Euro"
label var d_geschvol "Geschaeftsvolumen, kategorisch"
label var d_rev_pw "Geschaeftsvolumen pro Arbeiter, kategorisch"
label var d_inv_pw "Investitionen pro Arbeiter, kategorisch"

label def dummy 0 "Nein" 1 "Ja"
label val work_owner emp_grd hiring firing export dummy

label define	branlb 	1 "Land- und Forstwirtschaft" 2 "Bergbau/Energie" 3 "Nahrung/Genuss"	 ///
						4 "Bekleidung,Textil" 5 "Papier,Druck" 6 "Holzbearbeitung & Möbel,Schmuck,Spielwaren"  ///
						7 "Chem.Industrie" 8 "Kunststoff,Gummi" 9 "Steine,Erden" 10 "Metallerz./-bearb. & Stahl,Leichtmet"  ///
						11 "Recycling" 13 "Maschinenbau" 14 "Straßenfahrzeugbau & Kfz Handel" 15 "sonst.Fahrzeugbau"  ///
						16 "Elektrotechnik" 17 "Feinmechanik/Optik" 19 "Bauhauptgew." 20 "Ausbau/Bauhilf." 22 "Gross- und Einzelhandel"  ///
						24 "Verkehr" 25 "Nachrichtenübermittlung" 26 "Kredit/Finanz" 27 "Versicherung" 28 "Datenverarbeitung"  ///
						29 "Forschung/Entwicklung" 30 "Rechtsberatung,Werbung" 31 "Grundstücks-/Wohnungswesen" 32 "Vermietung/sonstige Dienstleistung."  ///
						33 "Gaststätten" 34 "Erziehung,Unterricht" 35 "Gesundheits-/Veterinär-/Sozialwesen" 36 "Hygiene"  ///
						37 "Kultur/Sport/Unterhaltung" 38 "Andere Dienstl." 39 "Int.vertr./Org. o. E." 41 "Oeffentliche Verwaltung/Sozialversicherung"
label value	bran branlb 

label define	bulalb 0 "Berlin/West" 1 "Schleswig-Holstein" 2 "Hamburg" 3 "Niedersachsen" 4 "Bremen" 5 "Nordrhein-Westfalen" 6 "Hessen" ///
7 "Rheinland-Pfalz/Saarland" 8 "Baden-Württemberg" 9 "Bayern" 10 "Saarland" 11 "Berlin" 12 "Brandenburg" 13 "Mecklenburg-Vorpommern" 14 "Sachsen" ///
15 "Sachsen-Anhalt" 16 "Thüringen" 18 "Rheinland-Pfalz"
label value 	bula bulalb

label define	west_ost 1 "West" 0 "Ost"
label value	west west_ost

label define	geschalb 1 "Umsatz" 2 "Bilanzsumme" 3 "Beitragssumme" 4 "Haushaltsvolumen" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	geschart geschalb

label define 	ertrllb1 1 "sehr gut" 2 "gut" 3 "befriedigend" 4 "ausreichend" 5 "mangelhaft" 6 "TNz; ÖD u.ä." -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	ertlag ertrllb1 

label define 	ertrllb2 1 "sehr gut" 2 "gut" 3 "befriedigend" 4 "ausreichend" 5 "mangelhaft" 6 "TNz; ÖD u.ä." -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	ertrlagv ertrllb2 

label define	tariflb 1 "Branchentarifvertrag" 2 "Haustarif/Firmentarif" 3 "keine Tarifvertrag" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	tarif tariflb 

label define 	westlb 1 "West" 0 "Ost" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value 	west westlb 

label define	weiterlb 1 "Ja" 2 "Nein" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	weiterb weiterlb 

label define	singlelb 1 "1-Betr.-Untern." 2 "Niederl./Fil." 3 "Zent./Hauptverw." 4 "Mittelinst" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value 	single singlelb 

label define	formlb 1 "Einzelunternehmen" 2 "Personenges." 3 "GmbH" 4 "Kapitalges." 5 "Körperschaft" 6 "sonst. Rechtsform" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	form formlb 

label define	eigentlb 1 "Ostdeutsches Eigentum" 2 "Westdeutsches Eigentum" 3 "Ausländisches Eigentum" 4 "Öffentliches Eigentum" 5 "keine Mehrheitseigentümer" 6 "nicht bekannt" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	eigentum eigentlb 

label define	betrralb 1 "Ja" 2 "Nein" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value 	betrrat betrralb 

label define	betr_andalb 1 "Ja" 2 "Nein" -9 "keine Angabe/weiss nicht"
label value	betr_and betr_andalb

label define	grjahrlb 999 "Gründung vor 1990" -9 "keine Angabe/weiss nicht" -8 "weiss nicht"
label value	grjahr grjahrlb

bys idnum year: gen n = _n
tab n
keep if n==1
qui compress
sort idnum year
save ${data}\2_LIAB_mm_IAB_prepare.dta, replace

cap log close


