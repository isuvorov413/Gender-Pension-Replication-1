version 10
capture log close
set more off
global createdata=1
global imputesp=0
global T0=36

glo tablepath "	${rootdir}\Outputs"

**********************************************************************************************
*Chilean Pension System: GenderReformdata
*****************************************
*by Clement Joubert
*March 12th 2009
*Revised February 2011
*Revised December 22 2015

*Project: Gender Equity 

*Object: 
* This dofile selects the estimation sample (exclusions described in the corresponding paragraphs,
* performs inputation of missing spouse information (see below)
* and extracts initial conditions to be used in the model simulations. 
*
* Inputation of spousal information:
* Missing information on spouses' pension balances and past work experiences is inputed by drawing
* from respondents of same gender, schooling, age and employement status 
*
* The dofile also merges in the pension system balances variables, 
* and adds information about the spouses
* of respondents who were missing in 2004 and "rescued" in 2006.

* NOTE: 
* For now there are a few missing values in 2005,6,7,8 => should be imputed
* Wealth is bottom coded at 0
* Topcode earnings at p99
*
* INPUTS:

*use .\masterdataHW.dta", clear
*

***********************************************************************************************

clear
clear matrix
clear mata
capture drop _merge
set memory 1500000

if ${createdata}==1 {

	use "${workdata}\masterdataHW.dta", clear
	***
	
	drop if year==12 //XXX only 28 observation, figure out reason

	xtset folio year


	***********************************************************************************************
	* Add CPI
	***********************************************************************************************
	sort year
	merge year using "${rawdata}\AFPreturns\AnPriceadj0601.dta",_merge(minflation)

	ta minflation
	keep if minflation==3

	***********************************************************************************************

	* Add pension balances
	* XXX Move to masterdata
	***********************************************************************************************

	sort folio year
	merge folio year using "${workdata}\DAbalances\anDAbal.dta", _merge(inanDAbal)
	ta inanDAbal
	keep if inanDAbal==1|inanDAbal==3 /*keep only if in EPS sample*/

	***********************************************************************************************

	* Add formal admin salaries
	* XXX Move to masterdata
	***********************************************************************************************
	sort folio year
	merge folio year using "${rawdata}\Pegado_AFP10\analysis\IngresoImponible.dta", _merge(ining_imp)
	ta ining_imp
	keep if ining_imp==1|ining_imp==3
	recode IngresoImponible (.=0)
	g H_TaxableIncome=IngresoImponible if sexsampled==1
	g W_TaxableIncome=IngresoImponible if sexsampled==2

	**********************************************************************************************
	*
	* Add bono de reconocimiento
	* XXX Move to masterdata
	*
	*********************************************************************************************

	sort folio
	merge folio using "${rawdata}\Bono\Bono2004.dta", _merge(inbono)
	ta inbono
	keep if inbono==1|inbono==3


	************************************************************************************************
	*
	* For people who were not interviewed in 2004, get spousal info from historia individual in 2006
	* XXX move to master data
	*
	**********************************************************************************************

	sort folio year
	capture drop _merge
	merge folio using "${workdata}\EPSspouse\spouseNE04info.dta", _merge(inspouse)
	ta inspouse
	keep if inspouse==1|inspouse==3

	************************************************************************************************
	*
	* Merge in sampling weights
	* XXX move to master data
	*
	**********************************************************************************************

	sort folio year
	capture drop _merge
	merge folio using "${workdata}\EPSweights.dta", _merge(inweights)
	ta inweights
	keep if inweights==1|inweights==3



	***********************************************************************************************
	*
	* Rename and redefine some variables
	*
	***********************************************************************************************

	do "${dodir}\Dataconstruction\upstream\variablesdef_GRdata.do"

	***********************************************************************************************
	*
	* Impute  missing variables for the spouses
	*
	**********************************************************************************************

	if ${imputesp}==1 {
	do "${dodir}\Dataconstruction\upstream\imputations_spouse.do"
	}

save Genderdata2, replace
}


**********************************************************************************************

*1. Sample restrictions

**********************************************************************************************

use Genderdata2, clear	
keep folio year kids_new kidsnodrop married married04 married06 married09 H_working W_working H_inactive_dum W_inactive_dum H_XP W_XP H_Xformal H_Xinformal W_Xformal W_Xinformal ///
H_death W_death H_dead W_dead separation marriageduration ///
H_formal_dum W_formal_dum H_yrsed W_yrsed H_age W_age sexsampled wealth  H_wage W_wage H_TaxableIncome W_TaxableIncome ///
H_working_lag W_working_lag  W_balance H_balance H_formal_lag W_formal_lag H_Xformal H_Xinformal ///
W_Xformal W_Xinformal singlespl female adj0601 otherpensionsystem sp_age dead inEPS06 weights06


***********************************************************************************************
*XXX temp: fix inconsistencies in marital status
***********************************************************************************************


foreach round in 04 06 09 {        // XXX fix some inconsistencies in coding of married variable
	replace married=married`round' if year==20`round'
}

replace married=married04 if year==2005			// XXX keep marital status of previous survey round for intermediate years
replace married=married06 if year==2007 | year==2008

***********************************************************************************************
*Drop individuals who contribute to INP (old pension system)
***********************************************************************************************

bysort folio: egen notAFP=sum(otherpensionsystem)
g Not_in_AFP_system=(notAFP>0 & notAFP!=.)

************************************************************************************************
*Drop individuals who are younger than 35 in 2004
************************************************************************************************

g H_var04=.
replace H_var04=H_age if year==2004

g W_var04=.
replace W_var04=W_age if year==2004
bysort folio : egen H_age04=max(H_var04)
bysort folio : egen W_age04=max(W_var04)
drop H_var04 W_var04

g tooyoung = (W_age04<$T0 |H_age04<$T0)

save "${workdata}\temp", replace



use "${workdata}\temp", clear

***********************************************************************************************
*Drop individuals who were single in 2004 and remarried after 2004
***********************************************************************************************
/*
*g remarry=(H_agelastmarriage>35 & H_agelastmarriage<999|W_agelastmarriage>35 & W_agelastmarriage<999)

g agesingle=H_age if married==0 & sexsampled==1 & H_age>34 & year>2003 & W_dead!=1
replace agesingle=W_age if married==0 & sexsampled==2 & W_age>34 & year>2003 & H_dead!=1
bysort folio: egen minagesingle=min(agesingle) 

g agemarried=H_age if married==1 & sexsampled==1 & H_age>34 & year>2003 & W_dead!=1
replace agemarried=W_age if married==1 & sexsampled==2 & W_age>34 & year>2003 & H_dead!=1
bysort folio: egen maxagemarried=max(agemarried)

g remarry=(minagesingle<maxagemarried) & maxagemarried!=. 
*/

g temp=(singlespl==1 & married==1 & year>2004)
bysort folio: egen remarry=max(temp)


************************************************************************************************
* Trim wealth variables
************************************************************************************************

g trimmed=0

replace H_balance=. if H_balance>600000000 & H_balance!=.
replace W_balance=. if W_balance>347000000 & W_balance!=.

************************************************************************************************
* Drop if big inconsistency. Check the consistency of spouse's age accross surveys. 
************************************************************************************************


g asp_age02=.
g asp_age04=.
g asp_age06=.
g asp_age09=.

replace asp_age02 = sp_age if year==2002
replace asp_age04 = sp_age if year==2004
replace asp_age06 = sp_age if year==2006
replace asp_age09 = sp_age if year==2009

bysort folio: egen sp_age02=max(asp_age02)
bysort folio: egen sp_age04=max(asp_age04)
bysort folio: egen sp_age06=max(asp_age06)
bysort folio: egen sp_age09=max(asp_age09)

g diffsp_age0204=sp_age04-sp_age02-2
g diffsp_age0406=sp_age06-sp_age04-2
g diffsp_age0609=sp_age09-sp_age06-3

codebook folio

g flagsp_age=((abs(diffsp_age0204)>9 & diffsp_age0204!=.) | (abs(diffsp_age0406)>9 & diffsp_age0406!=.)| (abs(diffsp_age0609)>9 & diffsp_age0609!=.))

replace W_age=sp_age06-2006+year if sexsampled==1 & sp_age06!=.
replace W_age=sp_age04-2004+year if sexsampled==1 & sp_age04!=. & sp_age06==.
replace W_age=sp_age02-2002+year if sexsampled==1 & sp_age02!=. & sp_age06==. & sp_age04==.
replace W_age=sp_age09-2009+year if sexsampled==1 & sp_age09!=. & sp_age06==. & sp_age04==. & sp_age02==.

replace H_age=sp_age06-2006+year if sexsampled==2 & sp_age06!=.
replace H_age=sp_age04-2004+year if sexsampled==2 & sp_age04!=. & sp_age06==.
replace H_age=sp_age02-2002+year if sexsampled==2 & sp_age02!=. & sp_age06==. & sp_age04==.
replace H_age=sp_age09-2009+year if sexsampled==2 & sp_age09!=. & sp_age06==. & sp_age04==. & sp_age02==.

codebook folio

g flagyoungspouse=(sp_age06<35 & sexsampled==2)

codebook folio

bysort folio: egen H_meanyrsed=mean(H_yrsed)
bysort folio: egen W_meanyrsed=mean(W_yrsed)
replace H_yrsed=round(H_meanyrsed)
replace W_yrsed=round(W_meanyrsed)


***********************************************************************************************
* Check for missing initial conditions
* Code as -99 observations that should be missing given survey design, to distinguish from other
* missing vars
***********************************************************************************************


replace H_age=-99 if sexsampled==2 & married==0 & separation==0 & dead==0
replace W_age=-99 if sexsampled==1 & married==0 & separation==0 & dead==0

replace H_working=-99 if sexsampled==2 & married==0
replace W_working=-99 if sexsampled==1 & married==0

replace H_yrsed=-99 if sexsampled==2 & married==0 & separation==0 & dead==0
replace W_yrsed=-99 if sexsampled==1 & married==0 & separation==0 & dead==0

replace H_formal_dum=-99 if sexsampled==2 & married==0
replace W_formal_dum=-99 if sexsampled==1 & married==0

replace  marriageduration=-99 if  married==0 & separation==0 & dead==0

replace H_working=-99 if sexsampled==2 & married==0
replace W_working=-99 if sexsampled==1 & married==0

replace H_wage=-99 if sexsampled==2 & married==0
replace W_wage=-99 if sexsampled==1 & married==0

replace H_formal_lag=-99 if sexsampled==2 & married==0
replace W_formal_lag=-99 if sexsampled==1 & married==0

replace H_working_lag=-99 if sexsampled==2 & married==0
replace W_working_lag=-99 if sexsampled==1 & married==0

replace H_XP=-99 if sexsampled==2 & married==0
replace W_XP=-99 if sexsampled==1 & married==0

replace H_Xformal=-99 if sexsampled==2 & married==0
replace W_Xformal=-99 if sexsampled==1 & married==0

replace H_Xinformal=-99 if sexsampled==2 & married==0
replace W_Xinformal=-99 if sexsampled==1 & married==0

replace H_wage=-99 if H_working==0 | sexsampled==2
replace W_wage=-99 if W_working==0 | sexsampled==1

replace H_balance=-99 if sexsampled==2 & married==0
replace W_balance=-99 if sexsampled==1 & married==0




g missing04=0
replace missing04=1 if year==2004 & (folio==. | year==. | married==. | H_working==. | W_working==. | H_XP==. | W_XP==.| ///
H_dead==. | W_dead==. | separation==. | marriageduration==. | H_formal_dum==. | W_formal_dum==. | H_yrsed==. | W_yrsed==. ///
| H_age==. | W_age==. | sexsampled==. | wealth==. | H_balance==. | W_balance==. | ///
H_working_lag==. | W_working_lag==. | singlespl==. |  H_formal_lag==. | W_formal_lag==.| H_Xformal==. | H_Xinformal==. | ///
 W_Xformal==. | W_Xinformal==. )
 
bysort folio: egen missinginit=max(missing04)


**************************************************************************************************
*
* Document effect of sample restrictions
*
**************************************************************************************************
g notsample = (inEPS06==0|tooyoung==1)
keep if notsample==0

save "${workdata}\PreExclusions.dta", replace

use  "${workdata}\PreExclusions.dta", replace

g exclude = (Not_in_AFP_system==1|remarry==1|missinginit==1 |flagsp_age==1|flagyoungspouse==1)




global varlist1 marriedperc sglfemale sglmale WLFP HLFP WFormperc HFormperc W_age H_age W_yrsed H_yrsed kidsnodrop  
global varlist2  W_wagemil H_wagemil wealthmil W_balmil H_balmil 


local i=4
preserve
keep if year==2004
recode _all (-99=.)

*generate tables describing effect of sample exclusions

g wealthmil=wealth/1000000
g H_balmil=H_balance/1000000
g W_balmil=W_balance/1000000

g H_wagemil=H_wage/1000000
replace H_wagemil=. if H_working==0 | sexsampled==2
g W_wagemil=W_wage/1000000
replace W_wagemil=. if W_working==0 | sexsampled==1
g marriedperc=married*100
g femaleperc=(sexsampled==2)*100
g sglfemale=(1-married)*(femaleperc)
g sglmale=(1-married)*(100-femaleperc)

g HLFP= (1-H_inactive_dum)*100
g WLFP=(1-W_inactive_dum)*100
g HFormperc=H_formal_dum*100
g WFormperc=W_formal_dum*100
replace HFormperc=. if H_working==0
replace WFormperc=. if W_working==0


label var marriedperc "Married (%)"
label var sglfemale "Single Women (%)"
label var sglmale "Single Men (%)"
label var WLFP "Working - Women (%)"
label var HLFP "Working - Men (%)"
label var WFormperc "Formal sector - Working Women (%)"
label var HFormperc "Formal sector - working Men (%)"
label var H_age "Age - Men"
label var W_age "Age - Women"
label var femaleperc "Women (%)"
label var H_yrsed "Yrs of School. - Men"
label var W_yrsed "Yrs of School. - Women"
label var wealthmil "Household non-pens. assets (MM CPL)"
label var H_wagemil "Annual Earnings - Working Men (MM CPL)"
label var W_wagemil "Annual Earnings - Working Women (MM CPL)"
label var H_balmil "Pension assets - Men (MM CPL)"
label var W_balmil "Pension assets - Women (MM CPL)"
label var kidsnodrop "Number of children"
/*
putexcel set "$tablepath\TablesFigures", modify sheet(Exclusions1)

sleep 1000

foreach X of varlist   $varlist1  {

local label : variable label `X'
putexcel A`i' = "`label'"

su `X',d 

quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(n) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel B`i' = matrix(A')

quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(mean) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel C`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(n) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel D`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(mean) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel E`i' = matrix(A')

local i=`i'+1

}



local i=4
putexcel set "$tablepath/TablesFigures", modify sheet(Exclusions2)

foreach X of varlist   $varlist2 {

local label : variable label `X'
putexcel A`i' = "`label'"

su `X',d 

quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(n) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel B`i' = matrix(A')

quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(p10) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel C`i' = matrix(A')

quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(p25) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel D`i' = matrix(A')

quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(median) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel E`i' = matrix(A')
	
quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(p75) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel F`i' = matrix(A')

quietly tabstat `X' if year==2004 & `X'!=. [aw=weights06], s(p90) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel G`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(n) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel H`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(p10) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel I`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(p25) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel J`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(median) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel K`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(p75) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel L`i' = matrix(A')

quietly tabstat `X' if year==2004 & exclude==0 & `X'!=. [aw=weights06], s(p90) f(%5.1f)   c(statistics) save
tabstatmat A
putexcel M`i' = matrix(A')

local i=`i'+1

}
*/
restore
**************************************************************************************************
* Output datasets
**************************************************************************************************


keep if inEPS06==1
drop inEPS06
drop if Not_in_AFP_system==1
drop Not_in_AFP_system
drop if tooyoung==1
drop tooyoung
drop if remarry==1
drop remarry
drop if missinginit==1 
drop missinginit
drop if flagsp_age==1 
drop flagsp_age
drop if flagyoungspouse==1 
drop flagyoungspouse


keep folio year kids_new kidsnodrop married H_working W_working H_XP W_XP H_Xformal H_Xinformal W_Xformal W_Xinformal ///
H_death W_death H_dead W_dead separation marriageduration ///
H_formal_dum W_formal_dum H_yrsed W_yrsed H_age W_age sexsampled wealth  H_wage W_wage H_TaxableIncome W_TaxableIncome ///
H_working_lag W_working_lag  W_balance H_balance H_formal_lag W_formal_lag H_Xformal H_Xinformal ///
W_Xformal W_Xinformal singlespl female adj0601

save "${workdata}\GenderReformdata3", replace

use "${workdata}\GenderReformdata3", replace

keep if year>2003

sort folio year

*codebook 

*convert to 2006 pesos
replace wealth=wealth/adj0601 if wealth!=-99
replace H_wage=H_wage/adj0601 if H_wage!=-99
replace W_wage=W_wage/adj0601 if W_wage!=-99
replace H_balance=H_balance/adj0601 if H_balance!=-99
replace W_balance=W_balance/adj0601 if W_balance!=-99

replace wealth=0 if wealth<0 & wealth!=. & wealth!=-99 /*XXX*/
su H_wage if H_wage!=. & H_wage!=-99, d
replace H_wage=r(p99) if H_wage>r(p99) & H_wage!=.
su W_wage if W_wage!=. & W_wage!=-99, d
replace W_wage=r(p99) if W_wage>r(p99) & W_wage!=.
recode _all (.=-99)

outsheet folio year kidsnodrop kids_new married H_working W_working H_XP W_XP H_Xformal H_Xinformal W_Xformal W_Xinformal H_death ///
W_death H_dead W_dead separation marriageduration ///
H_formal_dum W_formal_dum H_yrsed W_yrsed H_age W_age sexsampled wealth H_balance W_balance H_wage W_wage H_formal_lag W_formal_lag ///
H_working_lag W_working_lag singlespl  if year==2004 | year==2006 using Initialconditions.txt, nol replace

g truc = 1

outsheet  folio year married female W_age H_age H_working W_working H_formal_dum W_formal_dum singlespl W_balance ///
H_balance wealth kidsnodrop kids_new H_XP H_Xformal W_XP W_Xformal H_wage W_wage truc truc H_yrsed W_yrsed  separation H_TaxableIncome W_TaxableIncome ///
if year>2003 using dataformoments.txt, replace

save "${workdata}\GenderReformdata.dta", replace
