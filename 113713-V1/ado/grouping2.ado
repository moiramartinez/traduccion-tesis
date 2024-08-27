*Based on grouping-ado of Andrews, Schank, and Upward (2006).

cap program drop grouping2
program grouping2
	version 8.2
	syntax newvarname(max=1), Ivar(varname) Jvar(varname) lowbound(real)

	tempvar gmax jmin 

	di in green _newline "New variable `varlist' contains grouping indicator" _newline
	qui gen `varlist' = 1 if `jvar'==1

	local stop1 = 0
	while `stop1' == 0 {

	      local stop2 = 0
	      while `stop2' == 0 {
	      
			quietly {
				egen `gmax' = max(`varlist'), by(`ivar')
				replace `varlist' = `gmax'          
				drop `gmax'
				egen `gmax' = max(`varlist'), by(`jvar') 
				count if `varlist'!=`gmax'          
				local stop2 = r(N)==0	
				replace `varlist' = `gmax'          
				drop `gmax'
				}            
			}

	
	       quietly {
		       egen `jmin' = min(`jvar') if `varlist'==.        
		       count if `jmin'<.                   
		       local stop1 = r(N)==0			
		       egen `gmax' = max(`varlist')                
		       count if `jmin'==.
		       local alloc = r(N)
			   count if `ivar'!=.
			   local size = r(N)
			   local frac = `alloc'/`size'*100
			   noi di "Group " `gmax' ": `alloc' person-years allocated to groups (`frac')" 
			   replace `varlist' = `gmax'+1 if `jvar'==`jmin'
		       drop `gmax' `jmin' 
			   if "`frac'">="`lowbound'" {
					local stop1 = 1
				}
		       }
	       }


end
