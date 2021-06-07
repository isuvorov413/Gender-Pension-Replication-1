version 10
capture log close
set more off


**********************************************************************************************
*Employment Protection Survey (EPS): masterdataHW
***********************************************
*by Clement Joubert
*March 12th 2009
*Revised January 2011

*Object: 
* This dofile transforms the EPS data grouped in "masterdata.dta" from a Respondent/Spouse
* to a Husband/Wife format. It also creates some new variables. 

*NOTE: "married" means married or cohabitating here
*NOTE: assumes reported formal sector wages are net of contribution and adds those back 
*NOTE: annual spousal earnings are computed multiplying the reported average monthly wage by 
* the number of worked months in the year

*IN: C:\DATA\JLSdata\masterdata\masterdata.dta
*OUT: C:\DATA\JLSdata\masterdata\masterdataHW.dta
***********************************************************************************************


clear
set memory 1500000
use "${workdata}\masterdata.dta",clear
***

rename sex sexsampled

************************************************************************************************

**** 1. Convert the data set to a husband/wife format rather than interviewee/spouse

***********************************************************************************************
*Create H_var and W_var variables
***********************************************************************************************


g H_agelastmarriage=.
g W_agelastmarriage=.
g H_age=.
g W_age=.
g H_edugrp=.
g W_edugrp=.
g H_health=.
g W_health=.
g H_inactive=.
g W_inactive=.
g H_formal=.
g W_formal=.
g H_informal=.
g W_informal=.
g H_avwage=.
g W_avwage=.
g H_wage=.
g W_wage=.
g H_impbal=.
g W_impbal=.
g H_agegrp=.
g W_agegrp=.
g H_ocupation=.
g W_ocupation=.
g H_activity=.
g W_activity=.
g H_cohort=.
g W_cohort=.
g H_parttime=.
g W_parttime=.
g H_dead=.
g W_dead=.
g H_death=.
g W_death=.
g H_yrsed=.
g W_yrsed=.

replace H_agelastmarriage=agelastmarriage if sexsampled==1
replace W_agelastmarriage=agelastmarriage if sexsampled==2
replace H_age=age if sexsampled==1
replace W_age=age if sexsampled==2
replace H_edugrp=edugrp if sexsampled==1
replace W_edugrp=edugrp if sexsampled==2
replace H_health=. if sexsampled==1   /*var health not in dataset at this point*/
replace W_health=. if sexsampled==2
replace H_inactive=inactive+unemployed if sexsampled==1
replace W_inactive=inactive+unemployed if sexsampled==2
replace H_formal=salariedformal if sexsampled==1
replace W_formal=salariedformal if sexsampled==2
replace H_informal=salariedinformal + selfemployed + boss if sexsampled==1
replace W_informal=salariedinformal + selfemployed + boss if sexsampled==2
replace H_avwage=avwage if sexsampled==1
replace W_avwage=avwage if sexsampled==2
replace H_wage=wage if sexsampled==1
replace W_wage=wage if sexsampled==2
replace H_impbal=imp_bal if sexsampled==1
replace W_impbal=imp_bal if sexsampled==2
replace H_agegrp=agegrp if sexsampled==1
replace W_agegrp=agegrp if sexsampled==2
replace H_ocupation=ocupation if sexsampled==1
replace W_ocupation=ocupation if sexsampled==2
replace H_activity=activity if sexsampled==1
replace W_activity=activity if sexsampled==2
replace H_cohort=cohort if sexsampled==1
replace W_cohort=cohort if sexsampled==2
replace H_parttime=parttime if sexsampled==1
replace W_parttime=parttime if sexsampled==2
replace H_dead=dead if sexsampled==1
replace W_dead=dead if sexsampled==2
replace H_death=death if sexsampled==1
replace W_death=death if sexsampled==2
replace H_yrsed=yrsed if sexsampled==1
replace W_yrsed=yrsed if sexsampled==2

replace H_agelastmarriage=sp_agelastmarriage if sexsampled==2
replace W_agelastmarriage=sp_agelastmarriage if sexsampled==1
replace H_age=sp_age if sexsampled==2
replace W_age=sp_age if sexsampled==1
replace H_edugrp=sp_edugrp if sexsampled==2
replace W_edugrp=sp_edugrp if sexsampled==1
replace H_health=sp_health if sexsampled==2
replace W_health=sp_health if sexsampled==1
replace H_inactive=sp_inactive if sexsampled==2
replace W_inactive=sp_inactive if sexsampled==1
replace H_formal=sp_formal if sexsampled==2
replace W_formal=sp_formal if sexsampled==1
replace H_informal=sp_informal if sexsampled==2
replace W_informal=sp_informal if sexsampled==1
replace H_avwage=sp_avwage if sexsampled==2
replace W_avwage=sp_avwage if sexsampled==1
replace H_agegrp=sp_agegrp if sexsampled==2
replace W_agegrp=sp_agegrp if sexsampled==1
replace H_cohort=sp_cohort if sexsampled==2
replace W_cohort=sp_cohort if sexsampled==1
replace H_parttime=sp_parttime if sexsampled==2
replace W_parttime=sp_parttime if sexsampled==1
replace H_dead=sp_dead if sexsampled==2
replace W_dead=sp_dead if sexsampled==1
replace H_death=sp_death if sexsampled==2
replace W_death=sp_death if sexsampled==1
replace H_yrsed=sp_yrsed if sexsampled==2
replace W_yrsed=sp_yrsed if sexsampled==1



***********************************************************************************************

****2. Create somme additional variables of interest

***********************************************************************************************
***Generate labor sector dummies
***********************************************************************************************

g  H_formal_dum=( H_formal >= H_informal & H_formal>0)
replace H_formal_dum=. if H_formal==.|H_informal==.|H_inactive==.
g  H_informal_dum=( H_informal > H_formal & H_informal >0)
replace H_informal_dum=. if H_formal==.|H_informal==.|H_inactive==.
g  H_inactive_dum=( H_formal==0 & H_informal==0)
replace H_inactive_dum=. if H_formal==.|H_informal==.|H_inactive==.

g  W_formal_dum=( W_formal >= W_informal & W_formal>0) 
replace W_formal_dum=. if W_formal==.|W_informal==.|W_inactive==.
g  W_informal_dum=( W_informal > W_formal & W_informal >0)
replace W_informal_dum=. if W_formal==.|W_informal==.|W_inactive==.
g  W_inactive_dum=( W_formal==0 & W_informal==0)
replace W_inactive_dum=. if W_formal==.|W_informal==.|W_inactive==.

g H_XPincr=.
replace H_XPincr=0   if H_inactive_dum==1
replace H_XPincr=0.5 if H_inactive_dum==0 & H_parttime==1
replace H_XPincr=1   if H_inactive_dum==0 & H_parttime==0

g W_XPincr=.
replace W_XPincr=0   if W_inactive_dum==1
replace W_XPincr=0.5 if W_inactive_dum==0 & W_parttime==1
replace W_XPincr=1   if W_inactive_dum==0 & W_parttime==0

g W_XPincr_F=.
replace W_XPincr_F=0   if W_inactive_dum==1 | W_formal_dum==0
replace W_XPincr_F=0.5 if W_formal_dum==1 & W_parttime==1
replace W_XPincr_F=1   if W_formal_dum==1 & W_parttime==0

g W_XPincr_I=.
replace W_XPincr_I=0   if W_inactive_dum==1 | W_informal_dum==0
replace W_XPincr_I=0.5 if W_informal_dum==1 & W_parttime==1
replace W_XPincr_I=1   if W_informal_dum==1 & W_parttime==0

*g checkH= H_formal_dum + H_informal_dum + H_inactive_dum
*g checkW= W_formal_dum + W_informal_dum + W_inactive_dum
*ta checkH,m
*ta checkW,m

g JLS=.
replace JLS=1 if (H_formal_dum==1 & W_formal_dum==1)
replace JLS=2 if (H_formal_dum==1 & W_informal_dum==1)
replace JLS=3 if (H_formal_dum==1 & W_inactive_dum==1)
replace JLS=4 if (H_informal_dum==1 & W_formal_dum==1)
replace JLS=5 if (H_informal_dum==1 & W_informal_dum==1)
replace JLS=6 if (H_informal_dum==1 & W_inactive_dum==1)
replace JLS=7 if (H_inactive_dum==1 & W_formal_dum==1)
replace JLS=8 if (H_inactive_dum==1 & W_informal_dum==1)
replace JLS=9 if (H_inactive_dum==1 & W_inactive_dum==1)
ta JLS if JLS!=.,m g(JLSdum)

g oneincome=(JLS==3|JLS==6|JLS==7|JLS==8)
replace oneincome=. if JLS==.
g twoincomes=(JLS==1|JLS==2|JLS==4|JLS==5)
replace twoincomes=. if JLS==.

***********************************************************************************************
*Annualize wages 
***********************************************************************************************

g H_anwage=wage if sexsampled==1
g W_anwage=wage if sexsampled==2
replace H_anwage=sp_avwage*(12-sp_inactive) if sexsampled==2
replace W_anwage=sp_avwage*(12-sp_inactive) if sexsampled==1

replace H_anwage=1.1*H_anwage if H_formal==1 
replace W_anwage=1.1*W_anwage if W_formal==1

replace H_anwage=0 if (H_inactive==12 & year>2001 & sexsampled==1)
replace H_anwage=0 if (H_inactive==12 & (year==2002|year==2004|year==2006|year==2009) & sexsampled==2)
replace W_anwage=0 if (W_inactive==12 & year>2001 & sexsampled==2)
replace W_anwage=0 if (W_inactive==12 & (year==2002|year==2004|year==2006|year==2009) & sexsampled==1)

g H_logwage = ln(H_anwage)
g W_logwage = ln(W_anwage)
g H_logwage_lag=H_logwage[_n-1] if folio==folio[_n-1]
g W_logwage_lag=W_logwage[_n-1] if folio==folio[_n-1]
g H_logwagedfce=H_logwage-H_logwage_lag
g W_logwagedfce=W_logwage-W_logwage_lag


***********************************************************************************************
*sum labor sector dummies to get sector-specific experience 
***********************************************************************************************

sort folio year

                                                           
bysort folio: gen H_Xformal=sum(H_formal_dum) if H_age>15
bysort folio: gen H_Xinformal=sum(H_informal_dum) if H_age>15 
bysort folio: gen W_Xformal=sum(W_formal_dum) if W_age>15 
bysort folio: gen W_Xinformal=sum(W_informal_dum) if W_age>15 

bysort folio: gen H_XPpt=sum(H_XPincr) if H_age>15
bysort folio: gen W_XPpt=sum(W_XPincr) if W_age>15

bysort folio: gen W_XPpt_F=sum(W_XPincr_F) if W_age>15
bysort folio: gen W_XPpt_I=sum(W_XPincr_I) if W_age>15

replace H_Xformal=H_Xformal-H_formal_dum + Xformal_pre80 if H_age>15 & sexsampled==1
replace H_Xinformal=H_Xinformal-H_informal_dum  + Xinformal_pre80 if H_age>15 & sexsampled==1
replace W_Xformal=W_Xformal-W_formal_dum  + Xformal_pre80 if W_age>15 & sexsampled==2
replace W_Xinformal=W_Xinformal-W_informal_dum  + Xinformal_pre80 if W_age>15 & sexsampled==2


replace H_XPpt=H_XPpt-H_XPincr + Xformal_pre80 + Xinformal_pre80 if H_age>15 & sexsampled==1
replace W_XPpt=W_XPpt-W_XPincr + Xformal_pre80 + Xinformal_pre80 if W_age>15 & sexsampled==2


replace W_XPpt_F=W_XPpt_F-W_XPincr_F + Xformal_pre80 if W_age>15 & sexsampled==2
replace W_XPpt_I=W_XPpt_I-W_XPincr_I + Xinformal_pre80 if W_age>15 & sexsampled==2


replace H_Xformal= 0 if H_age<16
replace H_Xinformal=0 if H_age<16
replace W_Xformal= 0 if W_age<16
replace W_Xinformal=0 if W_age<16
replace H_XPpt=0 if H_age<16
replace W_XPpt=0 if W_age<16

replace W_XPpt_F=0 if W_age<16
replace W_XPpt_I=0 if W_age<16


*create a variable with the number of missing labor sectors by folio

sort folio year

gen H_Xformal_miss=(H_formal_dum==.) & year<2007
gen H_Xinformal_miss=(H_informal_dum==.) & year<2007
gen W_Xformal_miss=(W_formal_dum==.) & year<2007
gen W_Xinformal_miss=(W_informal_dum==.) & year<2007
bysort folio: egen H_Xformal_nbmiss=total(H_Xformal_miss)
bysort folio: egen H_Xinformal_nbmiss=total(H_Xinformal_miss)
bysort folio: egen W_Xformal_nbmiss=total(W_Xformal_miss)
bysort folio: egen W_Xinformal_nbmiss=total(W_Xinformal_miss)


*replace by missing value if labor sector choice was missing at some point


replace H_Xformal=. if H_Xformal_nbmiss>0
replace H_Xinformal=. if H_Xinformal_nbmiss>0
replace W_Xformal=. if W_Xformal_nbmiss>0
replace W_Xinformal=. if W_Xinformal_nbmiss>0

replace H_XPpt=. if H_Xformal_nbmiss+H_Xinformal_nbmiss>0
replace W_XPpt=. if W_Xformal_nbmiss+W_Xinformal_nbmiss>0

replace H_Xformal=. if sexsampled==2
replace H_Xinformal=. if sexsampled==2
replace W_Xformal=. if sexsampled==1
replace W_Xinformal=. if sexsampled==1

replace H_XPpt=. if sexsampled==2
replace W_XPpt=. if sexsampled==1

replace W_XPpt_F=. if sexsampled==1
replace W_XPpt_I=. if sexsampled==1

g H_XPpt_F=H_Xformal
g H_XPpt_I=H_Xinformal

***********************************************************************************************
* Create lagged wage and employment status variables
***********************************************************************************************

g H_status=1*(H_formal_dum==1)+2*(H_informal_dum==1)+3*(H_inactive_dum==1)
replace H_status=. if H_formal_dum==. | H_informal_dum==. | H_inactive_dum==.
g H_status_lag=1*(H_formal_dum[_n-1]==1)+2*(H_informal_dum[_n-1]==1)+3*(H_inactive_dum[_n-1]==1)
replace H_status_lag=. if H_formal_dum[_n-1]==. | H_informal_dum[_n-1]==. | H_inactive_dum[_n-1]==.| folio!=folio[_n-1]

g W_status=1*(W_formal_dum==1)+2*(W_informal_dum==1)+3*(W_inactive_dum==1)
replace W_status=. if W_formal_dum==. | W_informal_dum==. | W_inactive_dum==.
g W_status_lag=1*(W_formal_dum[_n-1]==1)+2*(W_informal_dum[_n-1]==1)+3*(W_inactive_dum[_n-1]==1)
replace W_status_lag=. if W_formal_dum[_n-1]==. | W_informal_dum[_n-1]==. | W_inactive_dum[_n-1]==.| folio!=folio[_n-1]



g JLS_lag=JLS[_n-2] if year==2006 & year[_n-2]==2004 & folio==folio[_n-2]
*ta JLS JLS_lag, col 
g nbincomes=0*(JLS==9)+1*(JLS==3|JLS==6|JLS==7|JLS==8)+2*(JLS==1|JLS==2|JLS==4|JLS==5)
replace nbincomes=. if JLS==.
g nbincomes_lag=0*(JLS_lag==9)+1*(JLS_lag==3|JLS_lag==6|JLS_lag==7|JLS_lag==8)+2*(JLS_lag==1|JLS_lag==2|JLS_lag==4|JLS_lag==5)
replace nbincomes_lag=. if JLS_lag==.

label define employment_status 1 "Covered" 2 "Uncovered" 3 "Inactive"
label values H_status employment_status
label values H_status_lag employment_status
label values W_status employment_status
label values W_status_lag employment_status

label define JLS 1 "Covered-Covered" 2 "Covered-Uncovered" 3 "Covered-Inactive" 4 "Uncovered-Covered" ///
5 "Uncovered-Uncovered" 6 "Uncovered-Inactive" 7 "Inactive-Covered" 8 "Inactive-Uncovered" 9 "Inactive-Inactive"

label values JLS JLS
label values JLS_lag JLS


label define education 1 "No HS" 2 "HS dropout" 3 "HS grad" 4 "College grad"
label values H_edugrp education
label values W_edugrp education

label define gender 1 "Male" 2 "Female"
label values sexsampled gender


***********************************************************************************************
***create variables describing the career of the household
***********************************************************************************************

sort folio year
bysort folio: gen totalyears=_n

g H_Xtotal=H_Xformal+H_Xinformal
g W_Xtotal=W_Xformal+W_Xinformal
g H_pctformal=H_Xformal/H_Xtotal
g W_pctformal=W_Xformal/W_Xtotal
g H_pctinformal=H_Xinformal/H_Xtotal
g W_pctinformal=W_Xinformal/W_Xtotal
g H_alwaysformal=(H_pctformal==1) 
g W_alwaysformal=(W_pctformal==1)
g H_alwaysinformal=(H_pctformal==0) 
g W_alwaysinformal=(W_pctformal==0)
g H_switcher=(H_pctformal<1 & H_pctformal>0)
g W_switcher=(W_pctformal<1 & W_pctformal>0)

replace H_alwaysformal=. if (H_pctformal==.) 
replace W_alwaysformal=. if (W_pctformal==.)
replace H_alwaysinformal=. if (H_pctformal==.) 
replace W_alwaysinformal=. if (W_pctformal==.)
replace H_switcher=. if (H_pctformal==.)
replace W_switcher=. if (W_pctformal==.)

g XPFhgroup=int(H_Xformal/5)+1
g XPIhgroup=int(H_Xinformal/5)+1
g XPFwgroup=int(W_Xformal/5)+1
g XPIwgroup=int(W_Xinformal/5)+1

label define XPgr 1 "<5" 2 "5-9" 3 "10-14" 4 "15-19" 5 "20-24" 6 "25-29" 7 "30-34" 8 "35-39" 9 "40-44" 10 "45-49"
label values XPFhgroup XPgr
label values XPIhgroup XPgr
label values XPFwgroup XPgr
label values XPIwgroup XPgr

g H_nbyrsinactive=H_age-1-15-H_Xformal-H_Xinformal
g W_nbyrsinactive=W_age-1-15-W_Xformal-W_Xinformal
replace W_nbyrsinactive=0 if W_nbyrsinactive<0

g H_pctformalgr     = int(4*H_pctformal)+1
g W_pctformalgr     = int(4*W_pctformal)+1

g H_pctinactive=H_nbyrsinactive/(H_age-1-15)
g W_pctinactive=W_nbyrsinactive/(W_age-1-15)

g H_inactivegr     = int(4*H_nbyrsinactive    /(H_age-1-15))+1
g W_inactivegr     = int(4*W_nbyrsinactive    /(W_age-1-15))+1

***********************************************************************************************
* create variables describing asset holdings
***********************************************************************************************

g Nowealth = . 
replace Nowealth = (wealth<0.001) if wealth!=.
g wealthunder6 = .
replace wealthunder6 = (wealth >=0.001 & wealth < 6) if wealth!=.
g wealthover6= . 
replace wealthover6= (wealth >= 6) if wealth!=.



save "${workdata}\masterdataHW.dta", replace

	
