**************************************************************************
* This script computes the Oaxaca estimator of Kline (2011) and reports
* standard errors according to the formulas in Kline (2014).
* Although not used in the paper, it also allows for computation of 
* Conley's (1999) spatial HAC based standard errors.
* Syntax is of the form:
* x_ols y x, bo(D) [cluster(id) ignore]
* Using the ', ignore' switch ignores the sampling variability in the \hat mu1_x
* Warning: do not use this routine with weights (i have not tested it).
**************************************************************************


clear mata
version 10

*******************************************************
*******************************************************
mata:
real matrix distance(string scalar latvar, string scalar longvar, string scalar s, scalar coord) 
 { 
   
   real colvector latitude
   real colvector longitude
   real colvector dlat
   real colvector dlong
   real colvector R
   real colvector V
   real colvector lati
   real colvector latj
   real colvector a
   real colvector d
   real colvector min
   real colvector c
   real colvector D
   
   latitude =st_data(., latvar,s)*pi()/180
   
      
   R=3963 - 13*sin(latitude[1])
   V = J(rows(latitude),rows(latitude),1)
     
   if (coord==1){
		dlat =  latitude:*V-latitude':*V
		a = sin(dlat:/2):^2 
   }
  
   if (coord==2){
		lati = latitude:*V
		latj = latitude':*V
		longitude=st_data(.,longvar,s)*pi()/180
		dlong = longitude:*V-longitude':*V
		a= cos(lati):*cos(latj):*sin(dlong:/2):^2
   }
  
   R=3963 - 13*sin(latitude[1])
   
   d     = sqrt(a)
   min   = (d:<=1):*d + (d:>1)
   c     = 2*asin(min)
   D     = R*c
   
   return(D)
}
end

*******************************************************
*******************************************************

mata:
real matrix window(matrix dis1, matrix dis2, scalar cut1, scalar cut2) 
{
   real colvector window1 
   real colvector window2 
   real colvector window
  
   V = J(rows(dis1),rows(dis1),1)
  
   window1 = V
   window1=window1:*(V-dis1/cut1)
   window1=window1:*(dis1:<cut1)  

   window2=window1:*(V-dis2/cut2)
   window2=window2:*(dis2:<cut2)
   window=window2
   return(window)
}
end

*****************************
*****************************
mata:
real matrix cov(matrix X, matrix W, vector epsilon, vector weight)
{
   	real colvector N 
   	real colvector xreg
   	real colvector XUUk 
   	real colvector XUUX1
   	real colvector XUUX
   	real colvector k
   	real colvector XUUK
   	real colvector x1
	real colvector x2
	real colvector invXX
	real colvector cov
	N = rows(epsilon)
	xreg = rows(X')
	XUUK = J(N,xreg,0)
	XUUX=J(xreg,xreg,0)

	for (k=1; k<=xreg; k++){
		x1 = epsilon*(X[.,k])'
		x2 = (epsilon:*W)'
		XUUk = x1:*x2
		XUUX1 = cross(XUUk,weight,X)/N
		XUUX[k,.] = colsum(XUUX1:*weight)
	}

invXX = invsym(cross(X,weight,X)/N)
cov = invXX*XUUX*invXX'/(N-xreg)
return(cov)
}
end


*************************************************************************************************
*************************************************************************************************

*! matnames 1.01 11july2009 took out eval =
*! matnames 1.0 16apr2009
*! Program to put row and column names in r()
*! posted to Statalist by Austin Nichols 16 April 2009
cap program drop matnames
prog matnames, rclass
 version 9.2
 syntax anything [, *]
 cap conf matrix `anything'
 if _rc {
  di as err "matrix `anything' not found"
  error 198
  }
 forv i=1/`=rowsof(`anything')' {
  mata: getRNames("`anything'",`i')
  loc r `"`r' `"`r1':`r2'"'"'
  }
 forv i=1/`=colsof(`anything')' {
  mata: getCNames("`anything'",`i')
  loc c `"`c' `"`r1':`r2'"'"'
  }
 return local r `"`r'"'
 return local c `"`c'"'
end
version 9.2
mata:
 void getRNames(string scalar b, real scalar i)
 {
   r=st_matrixrowstripe(b)
   st_local("r1", r[i,1])
   st_local("r2", r[i,2])
 }
 void getCNames(string scalar b, real scalar i)
 {
   c=st_matrixcolstripe(b)
   st_local("r1", c[i,1])
   st_local("r2", c[i,2])
 }
end

*****************************
*****************************
capture program drop x_ols_JP
program x_ols_JP, eclass
version  10.0

syntax varlist(min=1) [if] [aweight pweight fweight], lat(varname) long(varname) cut1(real) cut2(real) [NOCONStant]
 
local i = 0
tempvar _sample
qui gen `_sample' = 1 `if'
qui replace `_sample'= 0 if `_sample'==. 

** noconstant option 
if ("`noconstant'"!="") {
	local noconstant noconstant
}

*weights

if "`weight'"==""{
	tempvar _ones
	gen `_ones'=1
	local weight="aweight"
	local exp = "= `_ones'"
}

local weightvar = substr("`exp'",3,.)

* Save coeff estimates - assume always there's a constant

qui regress `varlist' `if' [`weight' `exp'], `noconstant'
matrix b = e(b)

* Take the missing values out of the sample
replace `_sample'=0 if e(sample)==0

qui count if `_sample'==1
local N = r(N)

* weights - normalize so as they always sum 1.
tempvar _weight
qui gen `_weight' = 1/`N'

if "`weight'"!="" {
qui sum `weightvar' if `_sample'==1
qui replace `_weight'= `weightvar'/r(sum)
}

tempvar epsilon
predict `epsilon', residuals

* Start regressors as constant

foreach v of local varlist {
	if `i' == 0{
		tempvar Y 
		local Y	= "`v'"
		local X =""
	}	 	 
	else if `i'==1{
		mata: X = st_data(.,"`v'","`_sample'")
		local X_`i'="`v'"
		local X = "`X' `v'"
	}
	else{
		mata: Xk = st_data(.,"`v'","`_sample'")
		mata: X = (X,Xk)
		local X_`i'="`v'"
		local X = "`X' `v'"
	}
	local i = `i'+1
}


if ("`noconstant'"=="") {
	mata: C = J(`N',1,1)
	mata: X = (X,C)
}

mata: epsilon = st_data(.,"`epsilon'","`_sample'")
mata: weight = st_data(.,"`_weight'","`_sample'")
*distance in each coordinate:
mata: D1 = distance("`lat'","`long'","`_sample'",1)
mata: D2 = distance("`lat'","`long'","`_sample'",2)
mata: W = window(D1,D2,`cut1',`cut2')
mata: Vs = cov(X,W,epsilon,weight)
mata: st_matrix("e(Vs)",Vs)
mat Vs=e(Vs)
matnames b
matrix rownames Vs = `r(c)'
matrix colnames Vs = `r(c)'
ereturn repost V=Vs
reg

end


*************************************************************************************************
** Based on Matias Busso's BO routine 
*************************************************************************************************
cap program drop bo_x
program define bo_x, eclass
syntax varlist(ts) [if/] [aweight pweight fweight], Treatment(varname) [ignore lat(varname)] [long(varname)] [cut1(real 1)] [cut2(real 1)] [cluster(varname)]

 ***-control of varlist
   tokenize `varlist'
   local Y = "`1'"
   macro shift
   local X "`*'"
 ***-control of option if
   if "`if'"=="" local if = ""
   if "`if'"!="" local andif = "& `if'"
   if "`if'"!="" local if = "if `if'"
 ***-control of treatment
   local T = "`treatment'"

 ***-Interact Xs
  cap drop _C_*
    foreach x in `X'{
      qui gen _C_`x' = `x'*(1-`T') `if'
   }
 
  
  gen _C_=1-`T'


  tempvar xb ystar
  if "`ignore'"==""{
  qui reg `Y' `X' if `T'==0 [`weight' `exp']
  qui predict `xb'
  gen `ystar'=`Y'
  replace `ystar'=`Y'-`xb' if `T'==1
  }
  else{
  gen `ystar'=`Y'
  }
  
  if "`cluster'"==""{
  qui x_ols_JP `ystar' _C_* `T' `if' [`weight' `exp'], lat(`lat') long(`long') cut1(`cut1') cut2(`cut2') nocons 
  }
  else{  
  qui reg `ystar' _C_* `T' [`weight' `exp'], cluster(`cluster') nocons
  }

  local v1=_se[`T']^2
  mata: v1=`v1'

  mat b=e(b)
  mat V=e(V)
  mat list b
  mat list V

  order `X'
  local K: word count `X'

  mata: x=st_data(.,1..`K',"`T'")
  mata: D=J(rows(x),1,1)
  mata: X=(x, D)

  mata: b=st_matrix("b")
  mata: b0=b[1..`K'+1] 
  
  qui sum `Y' if `T'==1
  scalar mu1=r(mean)
  mata: mu1=st_numscalar("mu1")
    
  mata: mu0=mean(X)*b0'
  mata: delt=mu1-mu0

  mata: V=st_matrix("V")
  mata: V0=V[1..`K'+1,1..`K'+1]
  mata: v0=mean(X)*V0*mean(X)'
  mata: C=mean(X)*V[`K'+2,1..`K'+1]'
  mata: se=sqrt(v1+v0-2*C)
  
  mata: st_numscalar("delt",delt)
  mata: st_numscalar("serr",se)

  qui reg `Y' `T' if e(sample), nocons
  mat bnew=delt
  mat Vnew=serr^2
  ereturn repost b=bnew V=Vnew
  reg

end

 
*****************************
*****************************
capture program drop x_ols
program x_ols, eclass
version  10.0

syntax varlist(min=1) [if] [aweight pweight fweight] [,ignore lat(varname) long(varname) cut1(real 999999999999) cut2(real 999999999999) bo(varname) cluster(varname)]

	
if "`bo'"==""{
	if "`cluster'"==""{
		if ("`lat'"==""){ 
		di as err "lat option needed if not using bo"
		exit 198
		}
		
		if ("`long'"==""){ 
		di as err "long option needed if not using bo"
		exit 198
		}

		if ("`cut1'"=="999999999999"){ 
		di as err "cut1 option needed if not using bo"
		exit 198
		}
	
		if ("`cut2'"=="999999999999"){ 
		di as err "cut2 option needed if not using bo"
		exit 198
		}
		x_ols_JP `varlist' `if' [`weight' `exp'], lat(`lat') long(`long') cut1(`cut1') cut2(`cut2')	
	
	}
	else {
		di as err "cluster option is not valid"
		exit 198
		}
	
}

else{
	if "`cluster'"==""{
		if ("`lat'"==""){ 
		di as err "lat option needed if not using cluster"
		exit 198
		}
		
		if ("`long'"==""){ 
		di as err "long option needed if not using cluster"
		exit 198
		}

		if ("`cut1'"=="999999999999"){ 
		di as err "cut1 option needed if not using cluster"
		exit 198
		}
	
		if ("`cut2'"=="999999999999"){ 
		di as err "cut2 option needed if not using cluster"
		exit 198
		}	
	
		bo_x `varlist' `if' [`weight' `exp'], `ignore' treatment(`bo') lat(`lat') long(`long') cut1(`cut1') cut2(`cut2')
	}
	
	else{
		if ("`lat'"!=""){ 
		di as err "lat option not allowed if using cluster"
		exit 198
		}
		
		if ("`long'"!=""){ 
		di as err "long option not allowed if using cluster"
		exit 198
		}

		if ("`cut1'"!="999999999999"){ 
		di as err "cut1 option not allowed if using cluster"
		exit 198
		}
	
		if ("`cut2'"!="999999999999"){ 
		di as err "cut2 option not allowed if using cluster"
		exit 198
		}
		bo_x `varlist' `if' [`weight' `exp'], `ignore' treatment(`bo') cluster(`cluster')
	}
}

end

	
