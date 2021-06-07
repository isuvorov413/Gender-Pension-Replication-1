
version 10
capture log close
set more off

*working directory
cd C:\Users\Clement\Dropbox\Genderequity\dofiles\

**********************************************************************************************
*Chilean Pension System: Labor history
******************************************
*by Clement Joubert
*December 6th 2008

*Object: This dofile extracts labor histories from EPS02:

*IN: C:\DATA\EPS02\Base7.dta
*OUT: C:\DATA\jointlaborstatus\masterdata\laborhistory19802002.dta

***********************************************************************************************


clear
set memory 900000


use "C:\DATA\EPS02\Base7.dta",clear

****Create firm size groups***

g firmsizegr=.
replace firmsizegr=1 if viip14=="A"
replace firmsizegr=10 if viip14=="B"
replace firmsizegr=20 if viip14=="C"
replace firmsizegr=50 if viip14=="D"
replace firmsizegr=100 if viip14=="E"
replace firmsizegr=200 if viip14=="F"
replace firmsizegr=500 if viip14=="G"
replace firmsizegr=1500 if viip14=="H"
replace firmsizegr=. if viip14=="X"

*Obvious miscoding
replace viip1da = 2000 if (folio==1452240 & orden==5)
replace viip1aa = 2000 if (folio==1452240 & orden==5)
replace viip1aa = 2000 if (folio==1452240 & orden==6)

drop if folio==45333&orden==4&viip1aa==1991

*1.Missing spell dates
*********************


*drop if year is missing
drop if viip1da==9999|viip1aa==9999

*if the month is missing on first spell, impute 1
replace viip1am=1 if (orden==1&viip1am==99)

*if the month is missing on the last spell, impute 12 (except in 2002)
sort folio orden
egen max_orden=max(orden), by(folio)
replace viip1cm=12 if (orden==max_orden&viip1cm==99&viip1da!=2002)

*if the end month of a spell and beginning month of next spell are missing:
*	if same year for the two spells: impute 1 and 2 respectively
*	if consecutive years for the two spells, impute 12 and 1

sort folio orden
by folio: gen lagviip1am=viip1am[_n-1]
by folio: gen lagviip1cm=viip1cm[_n-1]
by folio: gen lagviip1aa=viip1aa[_n-1]
by folio: gen lagviip1da=viip1da[_n-1]

g case1=1 if (viip1am==99&lagviip1cm==99&viip1aa==lagviip1da)

replace viip1am=2 if case1==1
replace viip1cm=1 if case1[_n+1]==1

g case2=1 if (viip1am==99&lagviip1cm==99&viip1aa==lagviip1da+1) 
replace viip1am=1 if case2==1
replace viip1cm=12 if case2[_n+1]==1

*if the beginning month is missing and previous spell ended in december of previous year, impute 1
replace viip1am=1 if (viip1am==99&lagviip1cm==12&lagviip1da==viip1aa-1)

*drop remaining spells with missing end month or begining month
drop if viip1am==99|viip1cm==99

drop case1 case2 lag* 
sort folio orden



****Expand years****

g spell=(viip1da-viip1aa)+1
expand spell
sort folio orden
by folio orden: egen incr=seq(), from(0)
g year=viip1aa+incr

egen incr_max=max(incr), by(folio orden)
g first_month=viip1am if incr==0
replace first_month=1 if year<=viip1da & incr!=0
g last_month=12 if year<viip1da & incr!=incr_max
replace last_month=viip1cm if incr==incr_max

by folio: g  first_monthlag=  first_month[_n+1]

g indcor=1 if first_monthlag!=last_month+1
replace indcor=. if first_monthlag==1 & last_month==12
replace indcor=. if orden==max_orden & (year==2005 | year==2004)

by folio: g  yearlag=year[_n+1]
g indcoryear=1 if last_month==12 & (yearlag!=year+1)
replace indcoryear=. if orden==max_orden & (year==2005 | year==2004)

replace  indcoryear=1 if viip1aa==viip1da & viip1am>viip1cm  

****Expand months****

g nbmonths=(last_month-first_month)+1
expand nbmonths
sort folio orden year
by folio orden year: egen incr2=seq(), from(0)
g month=first_month+incr2

g date=year*100+month

****Create a variable indicating whether the respondent changed jobs or labor force status in midyear***
g midyearjobchange=0
replace midyearjobchange=1 if (month==last_month&year==viip1da&month<12)|(month==first_month&year==viip1aa&month>1)
drop  max_orden spell incr year incr_max first_month last_month indcoryear nbmonths incr2 month

*duplicates tag folio date, generate(dupli_date)

****Merge info on affiliation****

sort folio
merge folio using "C:\DATA\EPS04\tipoafil.dta"

drop if _merge==2
drop _merge

sort folio


g affiliation=1 if tipoafi==1 | tipoafi==3
replace affiliation=2 if tipoafi==4

label define affiliationlbl 1 "Affiliate" 2 "Non affiliate"
label values affiliation affiliationlbl

sort folio

save lab2.dta, replace


***check affiliation with Administrative Data***

use "C:\DATA\Cruce_SAFP_Mayo_2007\ide.dta",clear

sort folio

merge folio using lab2.dta

drop if _merge==1


replace affiliation=1 if affiliation==2 & _merge==3 & fecafil<200312 & fecafil!=0

drop _merge



**********Labor status************************


***nb of months*****

g total_months=1

***months working*****

g working=0 
replace working=1 if viip2==3 

***months unemployed*****

g unemployed=0
replace unemployed=1 if viip2==2 | viip2==1

***months inactive****

g inactive=0
replace inactive=1 if viip2==4

***months as a boss***

g boss=0
replace boss=1 if (viip2==3|viip2==.) & viip8==1  /*Si no responde a viip2 (viip2==.) pero contesta las demas pregunta, cuenta como trabajador active)*/

***months self-employed****

g selfemployed=0
replace selfemployed=1 if (viip2==3|viip2==.)  & viip8==2

***months as salaried worker******

g salaried=0
replace salaried=1 if (viip2==3|viip2==.)  & (viip8==3 | viip8==4)

***months in domestic service****

g domestic=0
replace domestic=1 if (viip2==3|viip2==. ) & (viip8==5 | viip8==6)

***months as non-paid relative****

g relative=0
replace relative=1 if (viip2==3|viip2==.)  & viip8==7

***months in the army*****

g army=0
replace army=1 if (viip2==3|viip2==. ) & viip8==8

***months with contract***

g contract=0
replace contract=1 if viip10==1|viip10==2|viip10==3   /*unsigned contract or "don't know" is counted as no contract*/

***labor status***

g salariedformal=0
replace salariedformal=1 if contract==1 
g salariedinformal=0
replace salariedinformal=1 if contract==0 & viip8>2 & viip8!=.

g test=inactive+unemployed+selfemployed+boss+salariedformal+salariedinformal
ta test
drop test

***income****

g wage=.

***contributions***

***months contributed***

g AFPcontribution=1 if viip21==1
replace AFPcontribution=0 if viip21!=1
g otherpensionsystem=1 if viip21==2|viip21==3|viip21==4|viip21==5
replace otherpensionsystem=0 if otherpensionsystem!=1

g genderDA=1 if sexo=="M"
replace genderDA=2 if sexo=="F"

label define genderlbl 1 "Male" 2 "Female"
label values genderDA genderlbl


rename viip8 position
rename viip5 term
rename viip12 weeklyhours
g firmsize=.
rename viip14 firmsizetranche
rename viip15 union
g contributiondefault=(viip24==1)
replace contributiondefault=2 if (viip24==2|viip24==3|viip24==4)
replace contributiondefault=3 if (viip24==5)
rename viip4 ocupation
rename viip7 activity

replace activity=. if activity==99|activity==0
replace activity=int(activity/10)
replace ocupation=. if ocupation==99|ocupation==0
replace ocupation=int(ocupation/10)
replace weeklyhours=. if weeklyhours==999

keep  folio date position term weeklyhours firmsizegr union ocupation activity contributiondefault otherpensionsystem ///
 affiliation total_months unemployed inactive boss selfemployed salariedformal salariedinformal wage AFPcontribution midyearjobchange
gsort folio -date
duplicates drop folio date, force
sort folio date
g inEPS02=1
save laborhistory19802002.dta, replace

***Create data set with only last record, deemed to be at the date of interview (unless last date is outside of interview period)
use laborhistory19802002.dta, replace
sort folio date
bysort folio: g orden=_n
bysort folio: egen lastrec=max(orden)
keep if orden == lastrec
ta date
g intdate=date
replace intdate=. if date<200204
drop orden lastrec date
sort folio
save labor_lastmonth02.dta, replace
