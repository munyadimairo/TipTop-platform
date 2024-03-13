cap prog drop sim_ifh
cap prog define sim_ifh, rclass
version 18
*************************** notes***********************************************
* Author 			:		Munya Dimairo (mdimairo@gmail.com/m.dimairo@sheffield.ac.uk
* Program details		:		a) binary co-primary outcomes with only futility early stopping 
* 				:		b) reduction in event rate is a better patient outcome
* 				:		c) both co-primary outcomes must be statistically significant to claim evidence of benefit
*				:		d) assumes random correlation between co-primary outcomes  
********************************************************************************
****************************program dependencies********************************
* none!!  

****************************program syntax**************************************
syntax [, NSIMS(int 10) N_c(int 1) N_t(int 1) 						///
		PC_y1(real 0.5) PC_y2(real 0.5) 					///
		RD_y1(real 0.05) RD_y2(real 0.05)					///
		SIM_rd_y1(real 0) SIM_rd_y2(real 0)					///
		INTERIMS(int 1) INTERIM_if(string) 					///
		FUT_criteria_y1(string) FUT_criteria_y2(string) 			///
		FUT_spec_method(string) ALPHAF_y1(real 0.05) ALPHAF_y2(real 0.05) 	///
		SEEDSIM(int 12356) target_power(real 0.9)]

****************************syntax arguments *********************************** 
* nsims 		: number of simulations
* n_c, n_t		: total number of participants per treatment group assuming no early stopping (control, treatment)
* pc_y(1/2)		: control event rates for the co-primary outcomes
* rd_y(1/2)		: targeted risk differences for the co-primary outcomes
* sim_rd_y(1/2)		: assumed risk differences for the co-primary outcomes used for simulating trial data
* interims		: number of interim analyses including final analysis
* interim_if		: interim analyses information fraction including the final analysis
* fut_spec_method	: futility specification method: only allows pvalue, critvalue, or riskdiff
* fut_criteria_y(1/2)	: futility stopping criteria at each interim analysis as a vector list (exclude final analysis) 
* alpha_y(1/2)		: threshold for declaring statistical significance for the co-primary outcomes (on p-value scale)
* seedsim		: simulation seed to produce reproducible operating characteristics (OC)
* target_power 		: target power - just for reference when reviewing OCs

*****************************arguments checks***********************************

* check the number of interims and final analysis matches the information fractions
if (`interims' != wordcount("`interim_if'")) {
 	di in err "Number of interims (interims) must match specified corresponding information fractions (interim_if)."
	ex 198
 }
 
* check the number of interims (excluding the final analysis) matches futility criteria
local w1 = wordcount("`fut_criteria_y1'") + 1
local w2 = wordcount("`fut_criteria_y2'") + 1
if ((`interims' != `w1') | (`interims' != `w2')) {
	di in err "Number of interims (interims) excluding final analysis must match specified corresponding futility criteria for each primary outcome."
	ex 198
} 

* check futility and interim information fraction arguments parsed are not (technically) strings (can also add another check of incremental logic)
tokenize `interim_if', parse("")
if ("`1'" == "") {
	di in err "Interim_if must not be empty!"
	ex 198
}
local j 1
while ("`1'" != "") {
	local interimif`j' = `1'
	mac shift 
	qui cap assert (`interimif`j'' > 0) & (`interimif`j'' <= 1), fast
	if (_rc != 0) {
		di in err "Interim information fractions list in interim_if() must be in interval (0, 1]."
		ex 198
	}
	* check that the last specified information fraction is 1 at final analysis
	if (`j' == `interims') {
		qui cap assert (`interimif`j'' == 1)
		if (_rc != 0) {
			di in err "Information fraction at final analysis in list interim_if() must be 1."
			ex 198
		}
	}
	local ++j
}

* check that method for specifying futility criteria is recognised (as a start)
if (strlower("`fut_spec_method'") != "pvalue") & (strlower("`fut_spec_method'") != "critvalue") & (strlower("`fut_spec_method'") != "riskdiff") {
	di in err "Futility specification method in fut_spec_method() must be one of these: pvalue, critvalue, or riskdiff."
	ex 198
}

* check futility arguments parsed for each primary outcome are not (technically) strings
tokenize `fut_criteria_y1', parse("")
if ("`1'" == "") {
	di in err "Futility criteria must not be empty."
	ex 198
}
local j 1
while ("`1'" != "") {
	local fut_criteria_y1`j' = `1'
	mac shift 
	if ("`fut_spec_method'" == "pvalue") {
		cap assert ((`fut_criteria_y1`j'' > `alphaf_y1') & (`fut_criteria_y1`j'' < 1.0)), fast
		if (_rc != 0) {
			di in err "Futility criteria for the co-primary outcome (Y1) must be in (`alphaf_y1', 1) on pvalue scale (one-sided)."
			ex 198
		}
	} 
	if ("`fut_spec_method'" == "riskdiff") {
		cap assert ((`fut_criteria_y1`j'' >=0) & (`fut_criteria_y1`j'' < `pc_y1')), fast
		if (_rc != 0) {
			di in err "Futility criteria for the co-primary outcome (Y1) must be in (0, `pc_y1') on riskdiff scale."
			ex 198
		}
	}
	local ++j
}

tokenize `fut_criteria_y2', parse("")
if ("`1'" == "") {
	di in err "Futility criteria must not be empty."
	ex 198
}
local j 1
while ("`1'" != "") {
	local fut_criteria_y2`j' = `1'
	mac shift 
	if "`fut_spec_method'" == "pvalue" {
		qui cap assert ((`fut_criteria_y2`j'' > `alphaf_y2') & (`fut_criteria_y2`j'' < 1.0)), fast
		if (_rc != 0) {
			di in err "Futility criteria for the co-primary outcome (Y2) must be in (`alphaf_y2', 1) on pvalue scale (one-sided)."
			ex 198
		}
	}
	if "`fut_spec_method'" == "riskdiff" {
		cap assert ((`fut_criteria_y2`j'' >=0) & (`fut_criteria_y2`j'' < `pc_y2')), fast
		if (_rc != 0) {
			di in err "Futility criteria for the co-primary outcome (Y2) must be in (0, `pc_y2') on riskdiff scale"
			ex 198
		}
	}
	local ++j
}

* check the number of simulations 
cap assert (`nsims'>=1)
if (_rc != 0) {
	di in err "The number of simulations must be a positive integer." 
	ex 198
}

* check the sample sizes
cap assert ((`n_c'>=1) & (`n_t'>=1)), fast
if _rc != 0 {
	di in err "Sample size for each treatment group must be a positive integer" 
	ex 198
}
********************************************************************************
* total sample size 
local N = `n_c' +  `n_t'

* allication ratio to the treatment group
local aratio = (`n_t'/`N')

* set event rates in the treatment group for the two primary outcomes: for parsing mean vector in simulations 
local pt_y1_sim 	= 	`pc_y1' + `sim_rd_y1'
local pt_y2_sim 	= 	`pc_y2' + `sim_rd_y2'

* set event rate in the treatment group for the two primary outcomes under H1: for use when converting futility boundaries from one scale to another
local pt_y1_h1 		= 	`pc_y1' + `rd_y1'
local pt_y2_h1 		= 	`pc_y2' + `rd_y2'

* set local macros for parsed design parameters relating to interim analyses for each outcome (for use in simulations)
local futstages 	= 	`interims' - 1
forvalues j = 1(1)2 { 
	tokenize `fut_criteria_y`j''   
	local k 1
	while (`k' <= `futstages') {
		local fut`k'_y`j' 	=  	``k''
		local ++k
	}
	*derive thresholds for declaring futility at the end (assuming trial is not stopped early) for different configuration of futility specification
	if (("`fut_spec_method'" == "critvalue") & (`k' == `interims')) { 
		local fut`k'_y`j' 	= 	invnormal(1 - (`alphaf_y`j''/2))
	}
	if ((("`fut_spec_method'" == "pvalue") | ("`fut_spec_method'" == "riskdiff")) & (`k' == `interims')) { /* just use p-value scale here */
		local fut`k'_y`j' 	= 	`alphaf_y`j''
	}
	*di as text "fut`a'_y`j' = `fut`a'_y`j''"
}

* cumulative interim information fraction and sample sizes common for both primary outcomes (for use in simulations)
tokenize `interim_if'   
local k 1
while (`k' <= `interims') {
	local iaf`k' 	= 	``k'' 
	local nc`k' 	= 	ceil(`iaf`k'' * `n_c')
	local nt`k' 	= 	ceil(`iaf`k'' * `n_t')
	local ++k
}

* convert futility boundaries to different scale at each interim to aid communication with different stakeholders
local listmat "rd_futthresh zcrit_futthresh pval_futthresh"
foreach mat_name in `listmat'	{
	mat def `mat_name' 	= 	J(`futstages', 2, .)
	mat colnames `mat_name'	= 	y1 y2 
	local rownames
	forvalues k = 1(1)`futstages'{
		local rownames `rownames'  stage_`k'
	}
	matrix rownames `mat_name' = `rownames'
}

forvalues j = 1(1)2 {
	tokenize `fut_criteria_y`j''
	local k 1
	while (`k' <= `futstages') {
		local stderr_rd 	=  sqrt( ((1 -`pc_y`j'')/ `nc`k'') + ((1 -`pt_y`j'_h1')/ `nt`k''))
		if ("`fut_spec_method'" == "critvalue") {
			local rd`k'_y`j' 		=  ``k'' * `stderr_rd'
			mat rd_futthresh[`k', `j'] 	= 	`rd`k'_y`j'' 
			mat zcrit_futthresh[`k', `j']	=	``k''
			mat pval_futthresh[`k', `j']	= 	normal(``k'')
		}
		if ("`fut_spec_method'" == "pvalue") {
			mat pval_futthresh[`k', `j']	= ``k''
			mat rd_futthresh[`k', `j'] 	= invnormal(``k'')* `stderr_rd'
			mat zcrit_futthresh[`k', `j']	= -invnormal(``k'')
		}
		if ("`fut_spec_method'" == "riskdiff") {
			mat rd_futthresh[`k', `j']	= ``k''
			mat pval_futthresh[`k', `j'] 	= normal((``k'') / `stderr_rd')
			mat zcrit_futthresh[`k', `j']	= ``k'' / `stderr_rd'
		}
	local ++k
	}
}

foreach mat_name in `listmat' {
	ret mat `mat_name' 	= 	`mat_name'
}

* initialise metrics to capture at each interim and final analyses 
forvalues k = 1(1)`interims' {
	local stopfut`k' 		0  	/* number of trials stopping early for futility each stage of analysis including finalysis (i.e., futility claim at final stage k) */
	local success`k' 		0	/* number of trials with significant result at each stage: note that stage k-1 are redudant when there is no efficacy early stopping */
	if (`k' <= `futstages') {
		local proceed`k' 	0 	/* number of trials that progresses at each interim stage*/
	}
}

* temporary file 
tempfile control
tempfile fulldata

* initialise and start simulations
local sim 1
*set seed `seedsim'
local seed `seedsim'
while (`sim' <= `nsims') {
	if ((`sim'/50) == int(`sim'/50)) {
		di as txt ("`sim' ... ") _cont
	}
		
	* apply futility analyses across all interims, where applicable
	local k 1
	while (`k' <= `interims') {
		
		local nk = `nc`k'' + `nt`k''
		
		clear
		* simulate control group data for the full trial
		set seed `seed'
		qui set obs `nk'
		qui gen id = _n
		qui gen trt = rbinomial(1, `aratio')
		qui gen y1 = rbinomial(1, `pc_y1') if (trt == 0)
		qui gen y2 = rbinomial(1, `pc_y2') if (trt == 0) 

		qui replace y1 = rbinomial(1, `pt_y1_sim') if (trt == 1)
		qui replace y2 = rbinomial(1, `pt_y2_sim') if (trt == 1)
		
		qui save `fulldata', replace
		
		*di in red "[Sim; k] = [`sim', `k']"
		* mark records for inclusion in interim analysis (kth interim)
										
		* run interim analysis (difference in proportions) and apply futility rules
		forvalues j = 1(1)2 {
		 	local level_y`j' 	= (1 - (`alphaf_y`j''))*100 	/* two-sided sig level */
			* test for difference between proportions with normal approximation and extract results 
			qui glm y`j' i.trt, family(bin) link(identity) level(`level_y`j'')
			local diff_y`j' 	= 	r(table)[1,2] 		/* difference in proportions */
			local zcrit_y`j' 	= 	r(table)[3,2]		/* z critical value */
			local pval_y`j' 	=   normal(r(table)[3,2]) 	/* lower one-sided p-value */
			local p_y`j'		= 	r(table)[4,2]		/* two-sided p-value */
			local lb_y`j' 		= 	r(table)[5, 2]		/* lower confidence limit */
			local ub_y`j' 		= 	r(table)[6, 2]		/* upper confidence limit */
		 }
		 if (`k' < `interims') {
				if ("`fut_spec_method'" == "riskdiff") {
					if ((`diff_y1' >= `fut`k'_y1') & (`diff_y2' >= `fut`k'_y2')) {
						local ++stopfut`k'
						continue, break	 	/* stop execution and exit/terminates while loop (no further interim analysis) to start continue simulation */
					}
					else {
						local ++proceed`k' 	/* proceed to the next interim analysis */
					}
				}
				if ("`fut_spec_method'" == "pvalue") {
					if ((`pval_y1' >= `fut`k'_y1') & (`pval_y2' >= `fut`k'_y2')) {
						local ++stopfut`k'				
						continue, break		/* stop execution and exit/terminates while loop (no further interim analysis) to start continue simulation */
					}
					else {
						local ++proceed`k'	/* proceed to the next interim analysis */
					}
				}
				if ("`fut_spec_method'" == "critvalue") {
					if ((`zcrit_y1' >= `fut`k'_y1') & (`zcrit_y2' >= `fut`k'_y2')) {
						local ++stopfut`k'
						continue, break		/* stop execution and exit/terminates while loop (no further interim analysis) to start continue simulation */
					}
					else {
						local ++proceed`k'	/* proceed to the next interim analysis */
					}
				}
		 }
		 else if (`k' == `interims') {				/* final stage at maximum sample size */
		 		if (((`pval_y1' < `alphaf_y1'/2) & (`rd_y1' > `lb_y1')) & ((`pval_y2' < `alphaf_y2'/2) & (`rd_y2' > `lb_y2'))) { 		/* based on a two-sided test at final as direction of test is irrelevant except for early stopping */
					local ++success`k'
				}
				else {
					local ++stopfut`k'
				}
		 }
		local ++k 						/* interim increment */
	}
	local ++seed							/* seed increment so that each simulation will be different and reproducible */
	local ++sim							/* number of simulations  increment */
}

* return scalars for use post running this program 
ret scalar NSims 			= 	`nsims'
forvalues j=1(1)2{
	ret scalar alpha_y`j' 		=	`alphaf_y`j''
	ret scalar pc_y`j'		= 	`pc_y`j''
	ret scalar rd_y`j'		=	`rd_y`j'' 
	ret scalar sim_rd_y`j'		=	`sim_rd_y`j''
}
ret scalar N_C				= 	`n_c'
ret scalar N_T				= 	`n_t'
ret local fut_spec 			= 	strlower("`fut_spec_method'")
ret scalar target_power			=	`target_power'

* initialise matrices to save results and important stats 
foreach mat_name in futstop proceed success stage_N  {
	if ("`mat_name'" != "stage_N") {
		mat def `mat_name'		  	=  J(`interims', 1, .) 	/* prob of futility stopping and efficacy (success) at each interim (note that success here is only relevant at the final stage as there is no futility early stopping) */
		mat colnames `mat_name'			= 	`mat_name'
	}
	else if ("`mat_name'" == "stage_N") {
		mat def `mat_name'			=  J(`interims', 3, .)  /* initialise matrix to save information fraction and actual sample sizes at each interim */
		mat colnames `mat_name'			= 	frac n_c n_t
	} 
}

* change rownames of matrices to reflect stage_1, stage_2, ..., stage_k-1 or stage_k, where appropriate */
foreach mat_name in futstop proceed success stage_N {
	local rownames
	forvalues k = 1(1)`interims' {
		local rownames `rownames'  stage_`k'
	}
	mat rownames `mat_name' = `rownames'
}

* Populating matrices initialised above
local k 1
while (`k' <= `interims') {
	ret scalar interim_`k'		= 	`iaf`k''
	mat stage_N[`k', 1] 		= 	`iaf`k''
	mat stage_N[`k', 2]		=   `nc`k''
	mat stage_N[`k', 3]		=   `nt`k''
		
	mat futstop[`k', 1] 		= 	round((`stopfut`k''/ `nsims'), 0.0001)
	ret scalar stopfut_`k' 		= 	round((`stopfut`k''/ `nsims'), 0.0001)
		
	mat success[`k', 1] 		= 	round((`success`k''/ `nsims'), 0.0001)
		
	if (`k' == `interims') {
		ret scalar success 	= 	round((`success`k''/ `nsims'), 0.0001) /* for convenience return success at the end as the design as no efficacy early stopping */
	}
	local ++k
}	

local k 1
while (`k' <= `futstages') {
	mat proceed[`k', 1] 		= 	round((`proceed`k''/ `nsims'), 0.0001)
	ret scalar proceed`k' 		= 	round((`proceed`k''/ `nsims'), 0.0001)
	local ++k
}

foreach mat_name in futstop proceed success stage_N {
	ret mat `mat_name'		= 	`mat_name'
}


end 		/* end of program */

ex

* example on how to run the program assuming 4 interim analyses and display results stored in matrices
sim_ifh, nsims(5000) n_c(912) n_t(912) 					///
		pc_y1(0.12) pc_y2(0.25)					///
		rd_y1(-0.045) rd_y2(-0.09375) 				///
		sim_rd_y1(-0.045) sim_rd_y2(-0.09375) 			///
		interims(5) interim_if(0.35 0.50 0.70 0.85 1) 		///
		fut_criteria_y1(0 0 0 0) fut_criteria_y2(0 0 0 0)	///
		fut_spec_method(riskdiff)				///
		alphaf_y1(0.05) alphaf_y2(0.05) 			///
		seedsim(2504879) target_power(0.9) 

return list
foreach mat_name in futstop proceed success stage_N pval_futthresh zcrit_futthresh rd_futthresh {
	mat list r(`mat_name')
}

ex
