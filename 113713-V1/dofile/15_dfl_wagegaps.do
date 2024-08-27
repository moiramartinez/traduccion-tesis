clear all
cap log close
set linesize 120
set more off, perm
log using ${log}/15_dfl_wagegaps.log, replace
adopath ++${prog}
set seed 1000


* This program performs DFL decompositions to examine the sources of growing firm gender wage gaps.

	* Load analysis file, normalise firm effects, and rebuild panel for 1995-2008
		use ${data}/worker_dual_1_all, clear
		su psi10 if VA<=3.25
		replace psi10=psi10-r(mean)
		su psi11 if VA<=3.25
		replace psi11=psi11-r(mean)
		*[DROP YEAR 2000 FROM 1995-2001 PERIOD. THIS WILL LEAD TO SMALL DIFFERENCES IN THE DECOMPOSITION]
		keep if year<=2000
		
		append using ${data}/worker_dual_2_all
		su psi20 if VA<=3.1 & year>=2001
		replace psi20=psi20-r(mean)
		su psi21 if VA<=3.1 & year>=2001
		replace psi21=psi21-r(mean)
		keep persnr betnr idnum age bula educ ln_impwage psi* fem year

	* Rename AKM effects
		gen psi0=psi10 if year<=2001
		replace psi0=psi20 if year>=2001 & psi20!=.
		gen psi1=psi11 if year<=2001
		replace psi1=psi21 if year>=2001 & psi21!=.
		table year, c(mean psi10 mean psi20 mean psi0)
		table year, c(mean psi11 mean psi21 mean psi1)
		gen psigap1 = psi10-psi11
		gen psigap2 = psi20-psi21
		
	* Build quantiles of AKM distribution for each gender and period
		gen int1=(year>=1995&year<=2001)
		gen int2=(year>=2001&year<=2008)
		forval p=1/2 {
			forval g=0/1 {
				fastxtile psi`p'`g'_10_=psi`p'`g' if int`p'==1, nq(10)
				egen psi`p'`g'_10=max(psi`p'`g'_10_), by(betnr)
				drop psi`p'`g'_10_
				fastxtile psi`p'`g'_20_=psi`p'`g' if int`p'==1, nq(20)
				egen psi`p'`g'_20=max(psi`p'`g'_20_), by(betnr)
				drop psi`p'`g'_20_
			}
			fastxtile psigap`p'_10_=psigap`p' if int`p'==1, nq(10)
			egen psigap`p'_10=max(psigap`p'_10_), by(betnr)
			drop psigap`p'_10_
		 }
		drop psi10 psi11 psi20 psi21
		
		* Check that decile variables are defined for all years:
		su fem psigap1_10 psigap2_10 psi10_10 psi11_10 psi20_10 psi21_10 psi10_20 psi11_20 psi20_20 psi21_20

		ren psi0 fe_male
		ren psi1 fe_female
			
	* Combine into one variable [I do not need to assign men and women the AKM effects of the
	* other gender in this exercise]
		gen fe = fe_male if fem==0
		replace fe = fe_female if fem==1
		
	* Base year for job distribution
		gen tp=(year>=1995&year<=2000)
		*gen tp=(year>=2001&year<=2008)
		
* DFL reweighting: Reweight the 2001-2008 distribution of wages and AKM effects to the 
* 1995-2001 by conditioning on where men/women work in 1995-2001 and the relative bargaining
* component. I approximate this by 20 cells of M-/F-firm effects interacted with a dummy for
* female. This measures the share of women in each male/female AKM firm effect decile.

	
	foreach outcome in ln_impwage fe {						/* run decompositions for daily wages and firm premiums */
		
		global contr1 = "i.fem##(i.psi10_10 i.psigap1_10)"	/* three sets of conditioning variables */
		global contr2 = "i.fem##(i.psi11_10 i.psigap1_10)"
		global contr3 = "i.fem##(i.psi10_10 i.psi11_10)"
		
		foreach controlset in 1 2 3{ 	//1 2 
			
			logit tp ${contr`controlset'}
			predict p
			qui su tp
			local pbar=r(mean)											/*share of observations of gender g belonging to target period*/
			gen DFLweight=(1-tp)*(p/(1-p))/(`pbar'/(1-`pbar')) + tp		/*tp=0 for 2001-2008, i.e., this is where the weight is assigned to (correctly!)*/
			drop p
			qui {
				scalar nbins = 50
				qui su `outcome'
				generate x = r(min) if _n==1
				replace x = (r(max)-r(min))/(nbins-1) if _n>1
				replace x = sum(x)
				replace x = . if _n > nbins
				kdensity `outcome' if tp==0, gen(afactual) at(x) nograph					/*target period, actual*/
				kdensity `outcome' if tp==1, gen(factual) at(x) nograph						/*base period, actual*/
				kdensity `outcome' if tp==0 [aw=DFLweight], gen(cfactual) at(x) nograph		/*target period, counterfactual*/
				gen diff = afactual-cfactual
			}
			di "Kernel density estimates of actual and counterfactual densities, and the difference; last column shows number of workers"
			table x, c(mean afactual mean factual mean cfactual mean diff freq)
			qui {
				su `outcome' if tp==1
				local act0 = r(mean)
				su `outcome' if tp==0
				local act1 = r(mean)
				su x [aw=factual]
				local act0kd = r(mean)
				su x [aw=afactual]
				local act1kd = r(mean)
				su x [aw=cfactual]
				local cfact = r(mean)
			}
			mat dist0=J(8,4,.)
			mat dist1=J(8,4,.)
			bys betnr fem year: gen f=(_n==1)
			
			forval g=0/1{
				qui su f if tp==1 & fem==`g'
				mat dist`g'[8,1]=r(sum)
				mat dist`g'[8,2]=r(sum)
				qui su f if tp==0 & fem==`g'
				mat dist`g'[8,3]=r(sum)
				mat dist`g'[8,4]=r(sum)
			}
			
			* Get actual and counterfactual distributions conditional on gender:
			forval g=0/1{
			
				qui su `outcome' if tp==1 & fem==`g', det						/*actual 1995-2001*/
					mat dist`g'[1,1]=r(mean)
					mat dist`g'[2,1]=r(p10)
					mat dist`g'[3,1]=r(p25)
					mat dist`g'[4,1]=r(p50)
					mat dist`g'[5,1]=r(p75)
					mat dist`g'[6,1]=r(p90)
					mat dist`g'[7,1]=r(N)
				qui su `outcome' if tp==1 & fem==`g' [aw=DFLweight], det		/*counterfactual 1995-2001, should be the same as col 1 b/c weight=1*/
					mat dist`g'[1,2]=r(mean)
					mat dist`g'[2,2]=r(p10)
					mat dist`g'[3,2]=r(p25)
					mat dist`g'[4,2]=r(p50)
					mat dist`g'[5,2]=r(p75)
					mat dist`g'[6,2]=r(p90)
					mat dist`g'[7,2]=r(N)
				qui su `outcome' if tp==0 & fem==`g', det						/*actual 2001-2008*/
					mat dist`g'[1,3]=r(mean)
					mat dist`g'[2,3]=r(p10)
					mat dist`g'[3,3]=r(p25)
					mat dist`g'[4,3]=r(p50)
					mat dist`g'[5,3]=r(p75)
					mat dist`g'[6,3]=r(p90)
					mat dist`g'[7,3]=r(N)
				qui su `outcome' if tp==0 & fem==`g' [aw=DFLweight], det		/*counterfactual 2001-2008, reweighted to 1995-2001*/
					mat dist`g'[1,4]=r(mean)
					mat dist`g'[2,4]=r(p10)
					mat dist`g'[3,4]=r(p25)
					mat dist`g'[4,4]=r(p50)
					mat dist`g'[5,4]=r(p75)
					mat dist`g'[6,4]=r(p90)
					mat dist`g'[7,4]=r(N)
				
					mat rown dist`g' = mean p10 p25 p50 p75 p90 npy nfy
					mat coln dist`g' = 9501_act 9501_cfact 0108_act 0108_cfact
					* Display output: last two rows show number of person-years (npy) and number of firm-years (nfy)
					matlist dist`g', tit(Actual and counterfactual wage distributions: gender=`g', dual-connected set)
			}
			drop DFLweight x cfactual afactual factual diff f
		}
	}
