
version 10
capture log close
set more off

*working directory
cd C:\Users\Clement\Dropbox\Genderequity\dofiles\

**********************************************************************************************
*Chilean Pension System: Labor history
******************************************
*by Clement Joubert
*first: July 16th 2008

*Object: This dofile extracts labor histories from EPS04:
*The EPS04 contains the 2003-2004 labor histories plus the 1980-2002 histories for respondents 
*who were not in the 2002 sample (2002 had only pension system affiliates, EPS04 added about 4000
*non-affiliates.

*IN: C:\DATA\EPS04\historialaboral.dta
*OUT: C:\DATA\jointlaborstatus\masterdata\laborhistory19802004.dta


***********************************************************************************************


clear
set memory 500000


use "C:\DATA\EPS04\h_laboral.dta",clear

replace b1ia=2002 if folio==2405462 & orden==25
replace b1ia=2003 if folio==2405462 & orden==27
replace b1ta=2003 if folio==2405462 & orden==26
replace b1ta=2004 if folio==270805 & orden==6
replace b1tm=3 if folio== 2408345 & orden==1


****Create firm size groups***
replace b14=. if b14==-4
g firmsizegr=.
replace firmsizegr=1 if (b14==1|b14t==1)
replace firmsizegr=10 if ((b14>1&b14<10)|b14t==2)
replace firmsizegr=20 if ((b14>=10&b14<20)|b14t==3)
replace firmsizegr=50 if ((b14>=20&b14<50)|b14t==4)
replace firmsizegr=100 if ((b14>=50&b14<100)|b14t==5)
replace firmsizegr=200 if ((b14>=100&b14<200)|b14t==6)
replace firmsizegr=500 if ((b14>=200&b14<500)|b14t==7)
replace firmsizegr=1500 if (((b14>=500)&b14!=.)|b14t==8)


egen max_orden=max(orden), by(folio)

****Expand years****

g spell=(b1ta-b1ia)+1
expand spell
sort folio orden
by folio orden: egen incr=seq(), from(0)
g year=b1ia+incr

egen incr_max=max(incr), by(folio orden)
g first_month=b1im if incr==0
replace first_month=1 if year<=b1ta & incr!=0
g last_month=12 if year<b1ta & incr!=incr_max
replace last_month=b1tm if incr==incr_max

by folio: g  first_monthlag=  first_month[_n+1]

g indcor=1 if first_monthlag!=last_month+1
replace indcor=. if first_monthlag==1 & last_month==12
replace indcor=. if orden==max_orden & (year==2005 | year==2004)

by folio: g  yearlag=year[_n+1]
g indcoryear=1 if last_month==12 & (yearlag!=year+1)
replace indcoryear=. if orden==max_orden & (year==2005 | year==2004)

replace  indcoryear=1 if b1ia==b1ta & b1im>b1tm  

****Expand months****

g nbmonths=(last_month-first_month)+1
expand nbmonths
sort folio orden year
by folio orden year: egen incr2=seq(), from(0)
g month=first_month+incr2

g date=year*100+month

****Create a variable indicating whether the respondent changed jobs or labor force status in midyear***
g midyearjobchange=0
replace midyearjobchange=1 if (month==last_month&year==b1ta&month<12)|(month==first_month&year==b1ia&month>1)

drop  max_orden spell incr year incr_max first_month last_month indcoryear nbmonths incr2 month

duplicates tag folio date, generate(dupli_date)

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
replace working=1 if b2==1 

***months unemployed*****

g unemployed=0
replace unemployed=1 if b2==2 | b2==3

***months inactive****

g inactive=0
replace inactive=1 if b2==4

***months as a boss***

g boss=0
replace boss=1 if b2==1 & b7==1

***months self-employed****

g selfemployed=0
replace selfemployed=1 if b2==1 & b7==2

***months as salaried worker******

g salaried=0
replace salaried=1 if b2==1 & (b7==3 | b7==4)

***months in domestic service****

g domestic=0
replace domestic=1 if b2==1 & (b7==5 | b7==6)

***months as non-paid relative****

g relative=0
replace relative=1 if b2==1 & b7==7

***months in the army*****

g army=0
replace army=1 if b2==1 & b7==8

***months with contract***

g contract=0
replace contract=1 if b8==1   /*unsigned contract or "don't know" is counted as no contract*/

***labor status***

g salariedformal=0
replace salariedformal=1 if contract==1 
g salariedinformal=0
replace salariedinformal=1 if contract==0 & b7>2 & b7!=.

g test=inactive+unemployed+selfemployed+boss+salariedformal+salariedinformal
ta test
drop test

***income****

g wage=b11
replace wage=. if b2>1 & b2!=.
replace wage=. if b11<0
*replace wage=. if date<200201

***contributions***

***months contributed***

g AFPcontribution=1 if b17==1
replace AFPcontribution=0 if b17!=1
g otherpensionsystem=1 if b17==2|b17==3|b17==4|b17==5
replace otherpensionsystem=0 if otherpensionsystem!=1

g genderDA=1 if sexo=="M"
replace genderDA=2 if sexo=="F"

label define genderlbl 1 "Male" 2 "Female"
label values genderDA genderlbl


rename b7 position
rename b5 term
rename g_activ activity
rename b12 weeklyhours
rename b14 firmsize
rename b14t firmsizetranche
rename b15 union
rename b19 contributiondefault
rename g_oficio ocupation

replace activity=. if activity==99|activity==0
replace ocupation=. if ocupation==99|ocupation==0
replace weeklyhours=. if weeklyhours==-4

keep  folio date position term activity weeklyhours firmsizegr union contributiondefault otherpensionsystem ///
 ocupation affiliation total_months unemployed inactive boss selfemployed salariedformal salariedinformal wage AFPcontribution midyearjobchange
duplicates report folio date
sort folio date
g inEPS04=1
save laborhistory19802004.dta, replace


***Create data set with only last record, deemed to be at the date of interview (unless last date is outside of interview period)

use laborhistory19802004.dta, replace
sort folio date
bysort folio: g orden=_n
bysort folio: egen lastrec=max(orden)
keep if orden == lastrec
ta date
g intdate=date
replace intdate=. if date<200411
drop orden lastrec date
sort folio 
save labor_lastmonth04.dta, replace

