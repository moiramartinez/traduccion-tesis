

* Master.do des Projektes fdz1213
* Nutzer: 	Benjamin Bruns

capture cd U:\fdz1213
adopath ++ $prog
set linesize 120


/*---- SECTION: DATA PROCESSING, VARIABLE GENERATION, IMPUTATION ----*/

*do ${prog}/01_extract_iab_ep.do				/* Merge IAB-rawfiles, repeated cross-sections, variables added */
*do ${prog}/02_merge_and_clean_iab_ep.do		/* Construct variables in IAB-BP; clean and impute some firm-level covars; make consistent industryclass */
*do ${prog}/03_prepare_ieb.do				/* Prepare main sample, construct variables, collapse spells, full sample */
*do ${prog}/04_impute_education.do			/* Bildungskorrektur following Fitzenberger et al. (2006), full sample */
*do ${prog}/05_addvars.do					/* Generate variables for imputation, full sample */
*do ${prog}/06_impute_wages.do					/* Allocate upper tail of wages, full sample: gender X age group X education X year = 480 tobits */
*do ${prog}/07_akm_build.do					/* Build estimation samples*/
*do ${prog}/08_estimate_akms.do, nostop		/* Estimate AKMs and match effects models*/
*do ${prog}/09_descriptives.do, nostop		/* Summarise estimation sample*/
*do ${prog}/10_stylised_facts.do, nostop		/* Prepare stylised facts of gender gaps, wage inequality and productivity dispersion*/
*do ${prog}/11_eventstudy.do, nostop			/* Compute baseline event study and extensions for dual-gender moves and quartiles based on firm fixed effects*/
*do ${prog}/12_binning_fe.do, nostop			/* Binning firm effects by mean log value added per worker */
*do ${prog}/13_gridsearch.do, nostop			/* Gridseach algorithm to search for optimal cutoff */
*do ${prog}/14_decompositions.do, nostop		/* Performs baseline decompositions */
*do ${prog}/15_dfl_wagegaps.do, nostop		/* DFL decompositions of gender gap in firm premiums across periods */
*do ${prog}/16_stayermodels.do, nostop			/*Fit stayer models for robustness*/
*do ${prog}/17_rentsharing_models.do, nostop		/*Estimate rent-sharing models ala CCHK */
*do ${prog}/18_unions_rentsharing.do, nostop		/*Analysis of unions and rent-sharing*/
*do ${prog}/19_dfl_unions.do, nostop				/*Estimates of conditional gender gaps and DFL decomposition of AKM firm effect gap*/
*do ${prog}/20_akms_lowhigh.do, nostop				/*Estimate AKMs for low/high union coverage industries (assignment based on Statistisches Jahrbuch 2010*/
*do ${prog}/21_identify_childbirth.do, nostop			/*Identify childbirth spells*/
do ${prog}/22_analyse_childbirth.do, nostop				/*Analyse childbirth effects using event study design*/
*do ${prog}/23_gelbach.do, nostop			/*Gelbach decompositions*/

cap log close
*exit
