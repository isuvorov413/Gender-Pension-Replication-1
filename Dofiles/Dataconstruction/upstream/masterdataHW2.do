
capture log close
set more off
cd ""



**********************************************************************************************
*Chilean Pension System
************************
*by Clement Joubert
*March 12th 2009

*Object: This dofile converts masterdata into a Husband/Wife format (as opposed to interviewee/spouse)
*IN: .\masterdata.dta
*OUT: .\masterdataHW.dta
***********************************************************************************************




clear
set memory 900000
use "${workdata}\masterdata.dta"
***

rename sex sexsampled


***********************************************************************************************
*Create H_var and W_var variables
***********************************************************************************************


g H_ageatmarriage=.
g W_ageatmarriage=.
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
g H_contribEPS=.
g W_contribEPS=.
g H_avwageEPS=.
g W_avwageEPS=.
g H_anincomeEPS=.
g W_anincomeEPS=.
g H_wageDA=.
g W_wageDA=.
g H_retpay=.
g W_retpay=.
g H_contribDA=.
g W_contribDA=.
g H_yearafil=.
g W_yearafil=.
g H_balance=.
g W_balance=.
g H_impbal=.
g W_impbal=.
g H_june05bal=.
g W_june05bal=.
g H_agegrp=.
g W_agegrp=.
g H_ocupation=.
g W_ocupation=.
g H_activity=.
g W_activity=.


*replace H_ageatmarriage=ageatmarriage if sex==1 XXX
*replace W_ageatmarriage=ageatmarriage if sex==2 XXX
replace H_ageatmarriage=agelastmarriage if sex==1
replace W_ageatmarriage=agelastmarriage if sex==2
*replace H_age=imputedage if sex==1 XXX
*replace W_age=imputedage if sex==2 XXX
replace H_age=age if sex==1 
replace W_age=age if sex==2 
replace H_edugrp=edugrp if sex==1
replace W_edugrp=edugrp if sex==2
replace H_health=. if sex==1   /*var health not in dataset at this point*/
replace W_health=. if sex==2
replace H_inactive=inactive+unemployed if sex==1
replace W_inactive=inactive+unemployed if sex==2
replace H_formal=salariedformal if sex==1
replace W_formal=salariedformal if sex==2
replace H_informal=salariedinformal + selfemployed + boss if sex==1
replace W_informal=salariedinformal + selfemployed + boss if sex==2
*replace H_avwageEPS=avwagenomiss if sex==1 XXX
*replace W_avwageEPS=avwagenomiss if sex==2 XXX
replace H_avwageEPS=avwage if sex==1 
replace W_avwageEPS=avwag if sex==2
*replace H_anincomeEPS=annualincome if sex== 1 XXX
*replace W_anincomeEPS=annualincome if sex==2 XXX
*replace H_wageDA=wageDA if sex==1
*replace W_wageDA=wageDA if sex==2
/*replace H_retpay=retpay1 if sex==1
replace W_retpay=retpay1 if sex==2
replace H_contribDA=mandcont if sex==1
replace W_contribDA=mandcont if sex==2
replace H_yearafil=yearafil if sex==1
replace W_yearafil=yearafil if sex==2
replace H_balance=balance if sex==1
replace W_balance=balance if sex==2
replace H_impbal=imp_bal if sex==1
replace W_impbal=imp_bal if sex==2
replace H_june05bal=june05bal if sex==1
replace W_june05bal=june05bal if sex==2*/
replace H_agegrp=agegrp if sex==1
replace W_agegrp=agegrp if sex==2
replace H_ocupation=ocupation if sex==1
replace W_ocupation=ocupation if sex==2
replace H_activity=activity if sex==1
replace W_activity=activity if sex==2


*replace H_ageatmarriage=sp_ageatmarriage if sex==2
*replace W_ageatmarriage=sp_ageatmarriage if sex==1
replace H_ageatmarriage=sp_agelastmarriage if sex==2
replace W_ageatmarriage=sp_agelastmarriage if sex==1
*replace H_age=imp_sp_age if sex==2 XXX
*replace W_age=imp_sp_age if sex==1 XXX
replace H_age=sp_age if sex==2
replace W_age=sp_age if sex==1
*replace H_edugrp=imp_sp_edu if sex==2 XXX
*replace W_edugrp=imp_sp_edu if sex==1 XXX
replace H_edugrp=sp_edu if sex==2
replace W_edugrp=sp_edu if sex==1
replace H_health=sp_health if sex==2
replace W_health=sp_health if sex==1
replace H_inactive=sp_inactive if sex==2
replace W_inactive=sp_inactive if sex==1
replace H_formal=sp_formal if sex==2
replace W_formal=sp_formal if sex==1
replace H_informal=sp_informal if sex==2
replace W_informal=sp_informal if sex==1
replace H_contribEPS=. if sex==2
replace W_contribEPS=. if sex==1
replace H_avwageEPS=sp_avwage if sex==2
replace W_avwageEPS=sp_avwage if sex==1
*replace H_anincomeEPS=sp_income if sex==2
*replace W_anincomeEPS=sp_income if sex==1
replace H_wageDA=. if sex==2 | year>2004
replace W_wageDA=. if sex==1 | year>2004
replace H_retpay=. if sex==2
replace W_retpay=. if sex==1
replace H_contribDA=. if sex==2
replace W_contribDA=. if sex==1
replace H_yearafil=. if sex==2
replace W_yearafil=. if sex==1
replace H_balance=. if sex==2
replace W_balance=. if sex==1
*replace H_agegrp=int(imp_sp_age/5)*5 if sex==2
*replace W_agegrp=int(imp_sp_age/5)*5 if sex==1
replace H_agegrp=int(sp_age/5)*5 if sex==2
replace W_agegrp=int(sp_age/5)*5 if sex==1

***minor adjustments***
replace H_health=. if H_health==9|H_health==-4
replace W_health=. if W_health==9|W_health==-4
replace H_ageatmarriage=. if H_ageatmarriage==-4
replace W_ageatmarriage=. if W_ageatmarriage==-4
replace H_age=H_ageatmarriage+W_age-W_ageatmarriage if H_age==.
replace H_agegrp=int(H_age/5)*5 if H_agegrp==.

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


***********************************************************************************************
*Annualize wages (formal sector wages should include contribution to compare w/ simulation)
***********************************************************************************************

g H_anwage=wage if sexsampled==1
g W_anwage=wage if sexsampled==2

replace H_anwage=1.1*H_anwage if H_formal==1
replace W_anwage=1.1*W_anwage if W_formal==1

replace H_anwage=0 if (H_inactive_dum==1 & year>2001 & sexsampled==1)
replace H_anwage=0 if (H_inactive_dum==1 & (year==2004|year==2006) & sexsampled==2)
replace W_anwage=0 if (W_inactive_dum==1 & year>2001 & sexsampled==2)
replace W_anwage=0 if (W_inactive_dum==1 & (year==2004|year==2006) & sexsampled==1)



***********************************************************************************************
*Flag folios with missing education
***********************************************************************************************


bysort folio: egen missingH_edu=max((H_edugrp==.))
bysort folio: egen missingW_edu=max((W_edugrp==.))
ta missingH_edu
ta missingW_edu

g flagedu=(missingH_edu==1|missingW_edu==1)
drop missingH_edu
drop missingW_edu


***********************************************************************************************
*sum labor sector dummies to get sector-specific experience 
***********************************************************************************************
sort folio year

                                                           
bysort folio: gen H_Xformal=sum(H_formal_dum) if H_age>15
bysort folio: gen H_Xinformal=sum(H_informal_dum) if H_age>15
bysort folio: gen W_Xformal=sum(W_formal_dum) if W_age>15
bysort folio: gen W_Xinformal=sum(W_informal_dum) if W_age>15


replace H_Xformal=H_Xformal-H_formal_dum if H_age>15
replace H_Xinformal=H_Xinformal-H_informal_dum if H_age>15
replace W_Xformal=W_Xformal-W_formal_dum if W_age>15
replace W_Xinformal=W_Xinformal-W_informal_dum if W_age>15

replace H_Xformal= 0 if H_age<16
replace H_Xinformal=0 if H_age<16
replace W_Xformal= 0 if W_age<16
replace W_Xinformal=0 if W_age<16


replace H_Xformal=. if sexsampled==2
replace H_Xinformal=. if sexsampled==2
replace W_Xformal=. if sexsampled==1
replace W_Xinformal=. if sexsampled==1


g H_Xtotal=H_Xformal+H_Xinformal
g W_Xtotal=W_Xformal+W_Xinformal


  
keep  flag* folio year sexsampled wealth  cohort  H_activity W_activity H_ocupation W_ocupation H_edugrp W_edugrp H_age W_age H_ageatmarriage W_ageatmarriage H_anwage H_wageDA W_anwage W_wageDA H_balance W_balance H_impbal W_impbal H_june05bal W_june05bal H_agegrp H_Xformal H_Xinformal W_Xformal W_Xinformal H_inactive_dum W_inactive_dum H_formal_dum H_informal_dum W_formal_dum W_informal_dum H_Xtotal W_Xtotal

sort folio year

save "${workdata}\masterdataHW.dta", replace

preserve
sort folio year
keep folio year sexsampled
save  "${workdata}\sexsampled.dta", replace
restore

