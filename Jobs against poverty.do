**************************************
* Jobs against poverty – a working strategy? A fixed-effects analysis on the link between gaining employment and exiting poverty in Europe
* Vaalavuo M. & Sirniö O.
* Created on [Date]
* Dataset used: EU-SILC longidutinal data 2014-2018
**************************************

*- use data including only cases who are in 4 waves 

*- restrict analysis to those aged 25 to 64
rename RX010 AGE
keep if inrange(AGE,25,64)  

rename RB064 = weight4w 

egen PERCO_ID = group(COUNTRY PER_ID)
xtset PERCO_ID YEAR, yearly


	*** VARIABLES **
	
*pov_thres = poverty thresholds by country and by calendar year
capture drop if COUNTRY=="RS" // no pov_thres for Serbia
*DATAYEAR = year of data collection
	
**Poverty variables
*poverty status
	gen disp_income=HX090
	replace disp_income=0 if HX090<0 & DATAYEAR
	gen poor=0 if disp_income!=.
	replace poor=1 if disp_income<pov_thres & disp_income!=.
	
**Employment variables
*Total recorded months
	egen tot_month = rownonmiss(PL211A PL211B PL211C PL211D PL211E PL211F PL211G PL211H PL211I PL211J PL211K PL211L)
	label var tot_month "Total recorded monthly status"
*total number of months in each status 
	foreach month in PL211A PL211B PL211C PL211D PL211E PL211F PL211G PL211H PL211I PL211J PL211K PL211L {
		recode `month' (1/4=1 "work") (5/11=0 "not work") (.=.), gen(work_`month') // any work
		recode `month' (1 3=1 "work_full") (2 4/11=0 "other") (.=.), gen(work_full_`month') // full-time work
		recode `month' (1=1 "work_fullempl") (2/11=0 "other") (.=.), gen(work_fullempl_`month') // full-time employee
		recode `month' (2 4=1 "work_part") (1 3 5/11=0 "other") (.=.), gen(work_part_`month') // part-time work
		recode `month' (2=1 "work_partempl") (1 3/11=0 "other") (.=.), gen(work_partempl_`month') // part-time employee
	}
	egen work_tot=rsum(work_PL211A work_PL211B work_PL211C work_PL211D work_PL211E work_PL211F work_PL211G work_PL211H work_PL211I work_PL211J work_PL211K work_PL211L)
	egen work_full_tot=rsum(work_full_PL211A work_full_PL211B work_full_PL211C work_full_PL211D work_full_PL211E work_full_PL211F work_full_PL211G work_full_PL211H work_full_PL211I work_full_PL211J work_full_PL211K work_full_PL211L)
	egen work_fullempl_tot=rsum(work_fullempl_PL211A work_fullempl_PL211B work_fullempl_PL211C work_fullempl_PL211D work_fullempl_PL211E work_fullempl_PL211F work_fullempl_PL211G work_fullempl_PL211H work_fullempl_PL211I work_fullempl_PL211J work_fullempl_PL211K work_fullempl_PL211L)
	egen work_part_tot=rsum(work_part_PL211A work_part_PL211B work_part_PL211C work_part_PL211D work_part_PL211E work_part_PL211F work_part_PL211G work_part_PL211H work_part_PL211I work_part_PL211J work_part_PL211K work_part_PL211L)
	egen work_partempl_tot=rsum(work_partempl_PL211A work_partempl_PL211B work_partempl_PL211C work_partempl_PL211D work_partempl_PL211E work_partempl_PL211F work_partempl_PL211G work_partempl_PL211H work_partempl_PL211I work_partempl_PL211J work_partempl_PL211K work_partempl_PL211L)
	drop work_PL211A-inactive_PL211L
	gen work_fullself_tot=work_full_tot - work_fullempl_tot	// full-time self-employed
	gen work_partself_tot=work_part_tot - work_partempl_tot // part-time self-employed
* Total months working
	gen WI=work_tot/tot_month*12
	replace WI=round(WI)
	label var WI "months working"
* Indicator for any employment (employed for at least 1 month per year)
	gen anywork=0 if WI!=.
	replace anywork=1 if inrange(WI,1,12)
	label var anywork "Employment"
* Employment type, annual
	gen empl_type=0 // not-employed
	replace empl_type=1 if inrange(WI,1,12) & work_full_tot>=work_part_tot & work_fullempl_tot>=work_fullself_tot // full-time employee
	replace empl_type=2 if inrange(WI,1,12) & work_full_tot>=work_part_tot & work_fullempl_tot<work_fullself_tot // full-time self-employed
	replace empl_type=3 if inrange(WI,1,12) & work_part_tot>work_full_tot & work_partempl_tot>=work_partself_tot // part-time employee
	replace empl_type=4 if inrange(WI,1,12) & work_part_tot>work_full_tot & work_partempl_tot<work_partself_tot // part-time self-employed
	lab define empl_type 1 "full-time employee" 2 "full-time self-employed" 3 "part-time employee" 4 "part-time self-employed"
	lab val empl_type empl_type
* Employment months in categories  
	recode WI (0=0) (1/5=1) (6/9=2) (10/12=3) (.=.), gen(WI_3cat)	  
	lab define WI3cat 0 "not employed" 1 "less than 6 months empl" 2 "6-9 months empl" 3 "10-12 months empl" 
	lab val WI_3cat WI3cat	

**Poverty/employment categories
	gen group=0 
	replace group=1 if anywork==0 & poor==1
	replace group=2 if anywork==0 & poor==0
	replace group=3 if anywork==1 & poor==1
	replace group=4 if anywork==1 & poor==0
	label define group 1 "unemployed/inactive poor" 2 "unemployed/inactive non-poor" 3 "employed poor" 4 "employed non-poor" 0 "retired etc"
	label val group group
	
** Outcome
*- outcome only for those who unemployed/inactive poor in t-1
*- censored if AGE>65 or retired
gen exitpoor=.
replace exitpoor=0 if group==1 
bys PERCO_ID: replace exitpoor=0 if group==3 & l.exitpoor==0
bys PERCO_ID: replace exitpoor=1 if inlist(group,2,4) & l.exitpoor==0

*- indicator for last and first year in the follow-up
by PERCO_ID: egen lastexitpoor_year= max(cond(exitpoor!=., YEAR, .))
gen last_exitpoor = YEAR == lastexitpoor_year
drop lastexitpoor_year
bysort PERCO_ID: egen firstexitpoor_year= min(cond(exitpoor!=., YEAR, .))
gen first_exitpoor = YEAR == firstexitpoor_year
drop firstexitpoor_year


** Individual-level control variables 
*- HH_N_WA1864 = number of working-age (18 to 64 yo) household members
*- HH_N_child = number of children (<18 yo) household members
*- HH_N_senior = number of senior (>64 yo) household members
*- earnings_gross_other1000 = other household member's gross earnings * 1000
*- benefits_gross_other1000 = other household member's gross benefits * 1000
*- YEAR_inc = income reference year
*- educ = education (0 primary/lower secondary; 1 (upper) secondary; 2 tertiary)
*- AGEcat = age groups (1 18-29 year olds; 2 30-49 year olds; 3 50-59 year olds; 4 60-64 year olds)	

	
** Macro variables
preserve
use "X:\EU-SILC\Macro vars.dta", clear
xtset COUNTRY YEAR, yearly
foreach var in unemplrate preca almp {
	gen L2`var' = l2.`var'
}
save "X:\EU-SILC\Macro vars.dta", replace
restore


merge m:1 COUNTRY YEAR using "X:\EU-SILC\Macro vars.dta", nogen keepusing(L2unemplrate L2preca L2almp) // L2 variables are lagged for two years, i.e. measures the calendar year before income reference period

* Standardized macros
foreach macro in L2unemplrate L2preca L2almp {
	sum `macro' 
	gen `macro'_s = (`macro'-r(mean))/r(sd)
}
	
	
	*** DESCRIPTIVES ***

*analysis for those with no missing only
local controlsHHsize "HH_N_WA1864 HH_N_child HH_N_senior" 
local controlsHHinc "earnings_net_other1000 benefits_net_other1000"
local controlsIND "AGE educ YEAR"
mark nomiss if exitpoor!=.
markout nomiss `controlsHHsize' `controlsHHinc' `controlsIND' anywork WI_3cat empl_type 	
	

* TABLE 1 *
*% of women by country for sample ppl in the end of the follow-up
tab COUNTRY SEX if last_exitpoor==1 & nomiss [aw=weight4w], row nofre 

* FIGURE 1* 
* empl and pov status in the end of the follow-up, by gender
bysort SEX: tab COUNTRY group if last_exitpoor==1 & nomiss [aw=weight4w], row nofre  
 
* TABLE 3 *
* work intensity and empl type for those gaining employment, by gender and country
bysort SEX: tab COUNTRY WI_3cat if anywork==1 & exitpoor!=. & nomiss [aw=weight4w], row nofre 
bysort SEX: tab COUNTRY empl_type if anywork==1 & exitpoor!=. & nomiss [aw=weight4w], row nofre 
 
	
	
	
		*** FIXED EFFECTS MODELS **
		
xtset PERCO_ID YEAR, yearly

* TABLE 2 *	
*** by gender, countries combined 
local controls "HH_N_WA1864 HH_N_child HH_N_senior c.earnings_gross_other1000 c.benefits_gross_other1000 AGE i.YEAR_inc i.educ"
forvalues gender = 1/2 {
*MODEL 1: All controls + anywork 
	xtreg exitpoor `controls' anywork if nomiss==1 & SEX==`gender' [aweight=weight4w], fe vce(cluster PERCO_ID country) 
*MODEL 2: All controls + WI3cat 
	xtreg exitpoor `controls' i.WI_3cat  if nomiss==1 & SEX==`gender' [aweight=weight4w], fe vce(cluster PERCO_ID country)  
*MODEL 3: All controls + empl_type 
	xtreg exitpoor `controls' i.empl_type  if nomiss==1 & SEX==`gender' [aweight=weight4w], fe vce(cluster PERCO_ID country) 
*MODEL 4: All controls + anywork#edu 
	xtreg exitpoor `controls' i.anywork##i.educ if nomiss==1 & SEX==`gender' [aweight=weight4w], fe vce(cluster PERCO_ID country)
	testparm i.anywork#i.educ
	margins, dydx(anywork) at(educ=(0(1)2)) post vsquish
*MODEL 5: All controls + anywork#age
	xtreg exitpoor `controls' i.anywork##i.AGEcat if nomiss==1 & SEX==`gender' [aweight=weight4w], fe vce(cluster PERCO_ID country)
	testparm i.anywork#c.AGEcat
	margins, dydx(anywork) at(AGEcat=(1(1)4)) post vsquish
}

* FIGURE 2 *
***by gender, country interaction 
*MODEL 6: All controls + anywork#country
	xtreg exitpoor `controls' i.anywork##i.country if nomiss==1 & SEX==`gender' [aweight=paino4w], fe vce(cluster PERCO_ID country)
	margins, dydx(anywork) at(country=(1(1)24) country=(26(1)31)) post vsquish noestimcheck
	}


* TABLE 4 *
forvalues gender = 1/2 {	
    foreach macro in unemplrate preca almp { 
	xtreg exitpoor `controls' anywork##c.L2`macro'_s if nomiss==1 & SEX==`gender' [aweight=paino4w], fe vce(cluster PERCO_ID country)
	}
	
	
		*** SENSITIVITY TESTS ***
		
** Annex Table 1: Employment in t-1
*MODEL 1: All controls + anywork 
forvalues gender = 1/2 {
	xtreg exitpoor `controls' anywork L.anywork if nomiss==1 & SEX==`gender' [aweight=paino4w], fe vce(cluster PERCO_ID country)
	xtreg exitpoor `controls' i.anywork##i.L.anywork if nomiss==1 & SEX==`gender' [aweight=paino4w], fe vce(cluster PERCO_ID country)
}	
	
** Annex Table 3: Previous employment
rename PL200 prevempl
*MODEL 1: All controls + anywork 
forvalues gender = 1/2 {
	xtreg exitpoor `controls' anywork prevempl if nomiss==1 & SEX==`gender' [aweight=paino4w], fe vce(cluster PERCO_ID country)
	xtreg exitpoor `controls' i.anywork##c.prevempl if nomiss==1 & SEX==`gender' [aweight=paino4w], fe vce(cluster PERCO_ID country)
}

** Annex Table 4: Model 1 as random-effects model
forvalues gender = 1/2 {
*MODEL 1: All controls + anywork 
	xtreg exitpoor `controls' anywork if nomiss==1 & SEX==`gender' [iweight=weight4w], mle 
	}