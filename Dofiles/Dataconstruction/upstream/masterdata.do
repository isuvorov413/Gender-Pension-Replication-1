
version 10
capture log close
set more off

**********************************************************************************************
*Employment Protection Survey (EPS): masterdata
***********************************************
*by Clement Joubert
*First: December 06th 2008
*Last: August 2011

*Object: 
*This dofile combines labor histories from HLLS02, EPS04 and EPS06 with wealth and 
*spouse characteristics from EPS, balances from administrative data and sociodemographics
*into a 1980-2009 panel, creates some useful variables and performs some basic data cleaning described below.
*
*Data cleaning:
*Drop respondents that report different genders or very different ages (+ than 9 year difference)
*or schooling (2 schooling groups appart, where schooling groups are No HS, some HS, HS grad, college grad)
*accross waves.
*Impute an age and schooling consistent with the EPS2006 response to other waves when small discrepancies occur

*IN: .\EPSlabor\laborhistorypre1980.dta
*IN: .\EPSsamples\EPSsamples.dta
*IN: .\EPSlabor\anlaborhistory.dta
*IN: .\DAbalances\animpbal.dta
*IN: .\EPSWealth\EPSwealth.dta
*IN: .\EPSsociodemo\sociodemo.dta
*IN: .\EPSlabor\spouse\spousestatus.dta
*IN: .\EPSdead\EPSdead.dta
*IN: .\EPSkids\kidspanel.dta
*IN: .\EPScivilstatus\agelastmarriage.dta

*OUT: .\masterdata.dta
***********************************************************************************************


clear
set memory 900000

*0. Start with an empty panel 1980-2009 with all folios ever in EPS
********************************************************************
use "${workdata}\EPSsamples\EPSsample.dta", clear
expand 30
bysort folio: egen incr=seq(), from (0)
g year = 1980+incr
drop incr
sort folio year

*1.Merge income, asset, demographic characteristics etc.
********************************************************
merge folio year using "${workdata}\EPSlabor\anlaborhistory.dta", _merge(inlaborhist)
sort folio year
merge folio year using "${workdata}\DAbalances\anDAbal.dta", _merge(inimpbal)
sort folio year
merge folio year using "${workdata}\EPSwealth\EPSwealth.dta",_merge(inwealth)
sort folio year
merge folio year using "${workdata}\EPSsociodemo\sociodemo.dta",_merge(insociodemo)
sort folio year
merge folio year using "${workdata}\EPSlabor\spouse\spousestatus.dta", _merge(inspousestatus)
sort folio year
merge folio using "${workdata}\EPScivilstatus\agelastmarriage.dta", _merge(inagelast)
sort folio year
merge folio using "${workdata}\EPSlabor\laborhistorypre1980.dta", _merge(inxppre1980)
sort folio year
merge folio year using "${workdata}\EPSkids\kidspanel.dta", _merge(inkids)
sort folio year
merge folio using "${workdata}\EPSdead\EPSdead.dta", _merge(indead)
sort folio year
merge folio year using "${workdata}\EPSsocialsecurity\EPSsocialsecurity.dta", _merge(inSS)
sort folio year

xtset folio year

*2. Check for discrepancies in sex and age and drop the corresponding folios
****************************************************************************

g ssex02=.
g ssex04=.
g ssex06=.
g ssex09=.

replace ssex02 = sex if year==2002
replace ssex04 = sex if year==2004
replace ssex06 = sex if year==2006
replace ssex09 = sex if year==2009

bysort folio: egen sex02=max(ssex02)
bysort folio: egen sex04=max(ssex04)
bysort folio: egen sex06=max(ssex06)
bysort folio: egen sex09=max(ssex09)


g imputedage=0

g aage02=.
g aage04=.
g aage06=.
g aage09=.

replace aage02 = age if year==2002
replace aage04 = age if year==2004
replace aage06 = age if year==2006
replace aage09 = age if year==2009

bysort folio: egen age02=max(aage02)
bysort folio: egen age04=max(aage04)
bysort folio: egen age06=max(aage06)
bysort folio: egen age09=max(aage09)


g eedugrp02=.
g eedugrp04=.
g eedugrp06=.
g eedugrp09=.

replace eedugrp02 = edugrp if year==2002
replace eedugrp04 = edugrp if year==2004
replace eedugrp06 = edugrp if year==2006
replace eedugrp09 = edugrp if year==2009

bysort folio: egen edugrp02=max(eedugrp02)
bysort folio: egen edugrp04=max(eedugrp04)
bysort folio: egen edugrp06=max(eedugrp06)
bysort folio: egen edugrp09=max(eedugrp09)


g diffage0204=age04-age02-2
g diffage0406=age06-age04-2
g diffage0609=age09-age06-3

g diffsex0204=sex04-sex02
g diffsex0406=sex06-sex04
g diffsex0609=sex09-sex06

g diffedugrp0204=edugrp04-edugrp02
g diffedugrp0406=edugrp06-edugrp04
g diffedugrp0609=edugrp09-edugrp06

*
g flagsex=((diffsex0204!=0 & diffsex0204!=.) | (diffsex0406!=0 & diffsex0406!=.) | (diffsex0406!=0 & diffsex0406!=.))
g flagedu=((abs(diffedugrp0204)>2 & diffedugrp0204!=.) | (abs(diffedugrp0406)>2 & diffedugrp0406!=.)| (abs(diffedugrp0609)>2 & diffedugrp0609!=.))
g flagage=((abs(diffage0204)>9 & diffage0204!=.) | (abs(diffage0406)>9 & diffage0406!=.)| (abs(diffage0609)>9 & diffage0609!=.))

drop if flagsex==1
****
drop if flagedu==1
****
drop if flagage==1
****

*3. Fill in variables in between rounds
***************************************

replace sex=sex06
replace sex=sex04 if sex==.
replace sex=sex02 if sex==.
replace sex=sex09 if sex==.

replace edugrp=edugrp06
replace edugrp=edugrp04 if edugrp==.
replace edugrp=edugrp02 if edugrp==.
replace edugrp=edugrp09 if edugrp==.

replace imputedage=age06-2006+year
replace imputedage=age04-2004+year if imputedage==.
replace imputedage=age02-2002+year if imputedage==.
replace imputedage=age09-2009+year if imputedage==.
replace age=imputedage


replace agegrp=int(imputedage/5)*5
label values agegrp agegrp


bysort folio: egen iedugrp=max(edugrp)
replace edugrp=iedugrp
drop iedugrp

*4. Create some additional variables
************************************

g birthyear=year-age
g sp_birthyear=year-sp_age
g cohort=5*int((birthyear)/5)
g sp_cohort=5*int((sp_birthyear)/5)

g married=.
replace married=1 if civilstatus==1|civilstatus==2
replace married=0 if civilstatus>2 & civilstatus!=.

g temp02=married if year==2002
g temp04=married if year==2004
g temp06=married if year==2006
g temp09=married if year==2009

bysort folio: egen married02=max(temp02)
bysort folio: egen married04=max(temp04)
bysort folio: egen married06=max(temp06)
bysort folio: egen married09=max(temp09)


g death=.
g dead=0
forvalues i=1(1)26 {
replace death=1 if year==1981+`i' & death`i'==1
replace dead=1 if year>=1981+`i' & death`i'==1
}

g sp_death=.
replace sp_death=1 if (sp_dead04==0 | sp_dead04==.) & sp_dead06==1 & year==2006
replace sp_death=1 if (sp_dead06==0 | sp_dead06==.) & sp_dead09==1 & year==2009

g sp_dead=.
replace sp_dead=(sp_dead04==1) if year==2004
replace sp_dead=(sp_dead06==1) if year==2006
replace sp_dead=(sp_dead09==1) if year==2009


g sp_mummy=0
replace sp_mummy=1 if sp_dead04==0 & sp_dead06==1 & year==2006
replace sp_mummy=1 if sp_dead06==0 & sp_dead09==1 & year==2009

g separation=0 
replace separation=1 if married02==1 & married04==0 & year==2004 & sp_dead04==0
replace separation=1 if married04==1 & married06==0 & year==2006 & sp_dead06==0
replace separation=1 if married06==1 & married09==0 & year==2009 & sp_dead09==0


rename  baltot imp_bal

*5. crop beginning of life
**************************
drop if age<16

*for the young and old individuals, classify as inactive if employment status is missing

keep folio year birthyear cohort sex age agegrp edugrp yrsed kinship region civilstatus agelastmarriage sp_agelastmarriage health ///
student unemployed inactive boss selfemployed salariedformal salariedinformal parttime AFPcontribution otherpensionsystem ///
ocupation activity firmsizegr position term weeklyhours wealth kids_new kids kidsnodrop dead death married* separation ///
wage avwage imp_bal in*  sp_sex sp_age sp_agegrp sp_health sp_inactive sp_formal sp_informal sp_avwage sp_edugrp Xformal_pre80 Xinformal_pre80 ///
sp_cohort sp_birthyear sp_dead* sp_death sp_dum sp_yrsed sp_parttime


save "${workdata}\masterdata.dta", replace


