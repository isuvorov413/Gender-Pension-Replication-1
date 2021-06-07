
version 10
capture log close
set more off

*working directory
cd C:\Users\Clement\Dropbox\Genderequity\dofiles\
 
**********************************************************************************************
*Chilean Pension System: Labor history
******************************************
*by Clement Joubert and modified by Naoki Aizawa
*November 19th 2010

*Object: This dofile combines labor histories from EPS02, EPS04, EPS06 and EPS08

*Note: wage is the sum of received wages in the year, avwage is the average monthly wage over months worked

*IN: C:\DATA\JLSdata\masterdata\laborhistory0506.dta
*IN: C:\DATA\JLSdata\masterdata\laborhistory0708.dta
*IN: C:\DATA\JLSdata\masterdata\laborhistory19802004.dta
*IN: C:\DATA\JLSdata\masterdata\laborhistory19802002.dta
*OUT: C:\DATA\JLSdata\masterdata\laborhistory.dta

***********************************************************************************************


clear
set memory 900000


*1.Append labor histories from EPS02,04,06,08
**********************************************

use C:\DATA\EPSlabor\laborhistory19802002.dta, clear

append using C:\DATA\EPSlabor\laborhistory19802004.dta
*duplicates tag folio date, g(dupli_date)
*drop if dupli_date==1&inEPS02==1
*drop if dupli_date==1&inEPS04==1

append using C:\DATA\EPSlabor\laborhistory0506.dta
*duplicates tag folio date, g(dupli_date2)
*drop if dupli_date2==1&(inEPS04==1|inEPS02==1)
*drop if dupli_date2==1&(inEPS06==1)
append using C:\DATA\EPSlabor\laborhistory0708.dta
*duplicates tag folio date, g(dupli_date3)
*drop if dupli_date3==1&(inEPS06==1|inEPS04==1|inEPS02==1)
*drop if dupli_date3==1&(inEPS08==1)

*duplicates report folio date

replace inEPS08=0 if inEPS08==.
replace inEPS04=0 if inEPS04==.
replace inEPS06=0 if inEPS06==.
replace inEPS02=0 if inEPS02==.


save C:\DATA\EPSlabor\laborhistory.dta, replace

*Trim wages
***********

replace wage=. if wage>=100000000

*Create annual dataset
**********************

g year=int(date/100)
g month=date-year*100

replace wage=0 if wage==. & (inactive==1|unemployed==1) & year>2001
replace wage=. if year<2002
g missingwage=(wage==.)

sort folio year month

collapse (sum)total_months (sum)unemployed (sum)inactive (sum)boss (sum)missingwage (sum)selfemployed (sum)salariedformal ///
(sum)salariedinformal (sum)wage (sum) otherpensionsystem ///
(sum)AFPcontribution (mean)ocupation (mean)activity (sum)midyearjobchange (mean)firmsizegr (mean)position (mean)term (mean) weeklyhours, by(folio year)

*note investigate non integer ocupation, activity etc as weel as 99s
replace ocupation=. if midyearjobchange>1|int(ocupation)!=ocupation|ocupation>9
replace activity=. if midyearjobchange>1|int(activity)!=activity|activity>9
replace firmsizegr=. if midyearjobchange>1|int(firmsizegr)!=firmsizegr
replace position=. if midyearjobchange>1|int(position)!=position
replace term=. if midyearjobchange>1|int(term)!=term

replace wage=. if missingwage==12
g avwage=wage/(total_months-unemployed-inactive-missingwage)
replace wage=. if missingwage>0
su avwage, detail

*extrapolate annual earnings when individual is not observed for the whole 12 months (this is for year 2009)
g wagenoextrap=wage
replace wage=wage*12/total_months

g parttime=.
replace parttime=1 if weeklyhours<31
replace parttime=0 if weeklyhours>30 & weeklyhours!=.

*label define ocupation 1 
*label define activity 1
*label values ocupation ocupation
*label values activity activity
sort folio year
save anlaborhistory.dta, replace


