
version 10
capture log close
set more off
cd C:\DATA\EPSlabor


**********************************************************************************************
*Chilean Pension System: Labor history 2005-2006
************************************************
*by Clement Joubert
*1st: July 16th 2008
*Last: November 14th 2010
*Location: C:\DATA\EPSlabor

*Object: This dofile extracts labor histories from EPS06 (based on Javiera's dofiles):

*IN: C:\DATA\EPS06\historialaboral.dta
*IN: C:\DATA\EPS06\entrevistado.dta
*IN: C:\DATA\EPS04\tipoafil.dta
*IN: "C:\DATA\Cruce_SAFP_Mayo_2007\ide.dta",clear
*OUT: C:\DATA\jointlaborstatus\masterdata\laborhistory0506.dta

***********************************************************************************************


clear
set memory 500000


*Sample
*******
*Some people couldn't be interviewed in 2004, but were interviewed in 2006 (in2004=2)

use "C:\DATA\EPS06\entrevistado.dta",clear
keep folio tipomues
rename tipomues in2004
sort folio
save 2006not2004.dta, replace




*Number of spells
*****************
*for a given individual (=folio), "orden" contains the rank of the labor spell described in a given observation

use "C:\DATA\EPS06\historialaboral.dta",clear
egen nb_spells=max(orden), by(folio)

****Create firm size groups***
replace b15=. if b15==99999
g firmsizegr=.
replace firmsizegr=1 if (b15==1|b15t==1)
replace firmsizegr=10 if ((b15>1&b15<10)|b15t==2)
replace firmsizegr=20 if ((b15>=10&b15<20)|b15t==3)
replace firmsizegr=50 if ((b15>=20&b15<50)|b15t==4)
replace firmsizegr=100 if ((b15>=50&b15<100)|b15t==5)
replace firmsizegr=200 if ((b15>=100&b15<200)|b15t==6)
replace firmsizegr=500 if ((b15>=200&b15<500)|b15t==7)
replace firmsizegr=1000 if (((b15>=500)&b15!=.)|b15t==8)



*Create an annual dataset
*************************

g spell_yrs=(b1ta-b1ia)+1
expand spell_yrs
sort folio orden
by folio orden: egen incr=seq(), from(0)
g year=b1ia+incr

egen incr_max=max(incr), by(folio orden)
g month_beg=b1im if incr==0
replace month_beg=1 if year<=b1ta & incr!=0
g month_end=12 if year<b1ta & incr!=incr_max
replace month_end=b1tm if incr==incr_max

by folio: g  month_beglag=  month_beg[_n+1]

g indcor=1 if month_beglag!=month_end+1
replace indcor=. if month_beglag==1 & month_end==12
replace indcor=. if orden==nb_spells & (year==2007 | year==2006)

by folio: g  yearlag=year[_n+1]
g indcoryear=1 if month_end==12 & (yearlag!=year+1)
replace indcoryear=. if orden==nb_spells & (year==2007 | year==2006)

replace  indcoryear=1 if b1ia==b1ta & b1im>b1tm  

*Create a monthly dataset
*************************

g nb_months=(month_end-month_beg)+1
expand nb_months
sort folio orden year
by folio orden year: egen incr2=seq(), from(0)
g month=month_beg+incr2

g date=year*100+month


****Create a variable indicating whether the respondent changed jobs or labor force status in midyear***
g midyearjobchange=0
replace midyearjobchange=1 if (month==month_end&year==b1ta&month<12)|(month==month_beg&year==b1ia&month>1)


drop  nb_spells spell_yrs incr year incr_max month_beg month_end indcoryear nb_months incr2 month

duplicates tag folio date, generate(dupli_date)

*Get whether respondent is affiliated to the pension system
************************************************************

sort folio
merge folio using "C:\DATA\EPS04\tipoafil.dta"
ta _merge
drop if _merge==2
drop _merge


sort folio
merge folio using 2006not2004.dta
tab _merge
drop if _merge==2
drop _merge

g affiliation=1 if (tipoafi==1 | tipoafi==3) & in2004==1
replace affiliation=1 if in2004==2
replace affiliation=2 if tipoafi==4
label define affiliationlbl 1 "Affiliated to the Pension System" 2 "Not affiliated to the Pension System"
label values affiliation affiliationlbl
sort folio
save lab2.dta, replace


*Verify affiliation using administrative data
*********************************************

use "C:\DATA\Cruce_SAFP_Mayo_2007\ide.dta",clear

sort folio
merge folio using lab2.dta
drop if _merge==1
replace affiliation=1 if affiliation==2 & _merge==3 & fecafil<200312 & fecafil!=0
drop _merge


*Create labor history variables
*******************************

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
replace boss=1 if b2==1 & b8==1

***months self-employed****
g selfemployed=0
replace selfemployed=1 if b2==1 & b8==2

***months as salaried worker******
g salaried=0
replace salaried=1 if b2==1 & (b8==3 | b8==4)

***months in domestic service****
g domestic=0
replace domestic=1 if b2==1 & (b8==5 | b8==6)

***months as non-paid relative****
g relative=0
replace relative=1 if b2==1 & b8==7

***months in the army*****
g army=0
replace army=1 if b2==1 & b8==8

***months with contract***
g contract=0
replace contract=1 if b9==1   /*unsigned contract or "don't know" is counted as no contract*/

***labor status***
g salariedformal=0
replace salariedformal=1 if contract==1 
g salariedinformal=0
replace salariedinformal=1 if contract==0 & b8>2 & b8!=.

*Check that everybody as 1 and only one status
g test=inactive+unemployed+selfemployed+boss+salariedformal+salariedinformal
ta test
drop test

***wage***
g wage=b12
replace wage=. if b2>1 & b2!=.
replace wage=. if b12==1000000000


***months contributed***
g AFPcontribution=1 if b18==1 
replace AFPcontribution=0 if b18!=1
g otherpensionsystem=1 if b18==2|b18==3|b18==4|b18==5
replace otherpensionsystem=0 if otherpensionsystem!=1

***gender from administrative data***
g genderDA=1 if sexo=="M"
replace genderDA=2 if sexo=="F"

label define genderlbl 1 "Male" 2 "Female"
label values genderDA genderlbl

rename b8 position
rename b6 term
rename actividad activity
rename b13 weeklyhours
rename b15 firmsize
rename b15t firmsizetranche
rename b16 union
rename b20 contributiondefault
rename oficio ocupation

replace activity=. if activity==99|activity==0
replace ocupation=. if ocupation==99|ocupation==0
replace weeklyhours=. if weeklyhours==999

keep  folio date affiliation position term activity weeklyhours firmsizegr union contributiondefault otherpensionsystem ///
ocupation total_months unemployed inactive boss selfemployed salariedformal salariedinformal wage AFPcontribution midyearjobchange
duplicates report folio date
sort folio date
g inEPS06=1
save C:\DATA\EPSlabor\laborhistory0506.dta, replace


***Create data set with only last record, deemed to be at the date of interview (unless last date is outside of interview period)

use laborhistory0506.dta, replace
sort folio date
bysort folio: g orden=_n
bysort folio: egen lastrec=max(orden)
keep if orden == lastrec
ta date
g intdate=date
*replace intdate=. if date<200411
drop orden lastrec date
sort folio
save labor_lastmonth06.dta, replace
