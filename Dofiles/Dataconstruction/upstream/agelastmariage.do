
version 10
capture log close
set more off
cd C:\DATA\EPScivilstatus

**********************************************************************************************
*Chilean Pension System: Age at last marriage
*********************************************
*by Clement Joubert
*January 20th 2011
*Location: C:\DATA\EPScivilstatus

*Object: This dofile extracts the age at last marriage/cohabitation to be used in selecting the estimating sample 

*IN: C:\DATA\EPS04\h_individual.dta
*IN: C:\DATA\EPS06\h_individual.dta
*IN: C:\DATA\EPS09\h_individual.dta
*OUT: C:\DATA\EPScivilstatus\agelastmarriage


*NOTE: some people now their spouse's age at marriage but not their own? check
***********************************************************************************************


clear
set memory 500000

*append the records for all partners from EPS04, EPS06, EPS09

use C:\DATA\EPS04\h_individual.dta, clear
save temp, replace
rename i6 ageatmarriage
rename i7 sp_ageatmarriage
g sp_dead04=0
replace sp_dead04=1 if i9==2
keep folio ageatmarriage sp_ageatmarriage sp_dead04
replace ageatmarriage=. if ageatmarriage==-4
replace sp_ageatmarriage=. if sp_ageatmarriage==-4
save temp04, replace

use C:\DATA\EPS06\historiaindividual.dta, clear
save temp, replace
rename i5 ageatmarriage
rename i6 sp_ageatmarriage
g sp_dead06=0
replace sp_dead06=1 if i9==2
keep folio ageatmarriage sp_ageatmarriage sp_dead06
replace ageatmarriage=. if ageatmarriage==99
replace sp_ageatmarriage=. if sp_ageatmarriage==99
save temp06, replace

use C:\DATA\EPS09\hindividual.dta, clear
save temp, replace
keep if i2_2>0 & i2_2!=.
rename i5 ageatmarriage
rename i6 sp_ageatmarriage
g sp_dead09=0
replace sp_dead09=1 if i9==2
keep folio ageatmarriage sp_ageatmarriage sp_dead09
replace ageatmarriage=. if ageatmarriage==9 | ageatmarriage==8
replace sp_ageatmarriage=. if sp_ageatmarriage==9 | sp_ageatmarriage==8
save temp09, replace

use temp04, clear
append using temp06
append using temp09

*sort spouses by age at marriage of respondent

sort folio ageatmarriage

*keep the last record for each folio and value of age at last marriage

bysort folio: egen orden=seq(), from (1)
bysort folio: egen lastspouse=max(orden)
keep if orden==lastspouse

*merge with total EPS sample

merge folio using C:\DATA\EPSsamples\EPSsample.dta
ta _merge

*for respondents who never married/cohabitated, code age at last marriage as 999

replace ageatmarriage=999 if _merge==2

replace sp_dead06=sp_dead04 if sp_dead06==.
replace sp_dead09=sp_dead06 if sp_dead09==.

rename ageatmarriage agelastmarriage
rename sp_ageatmarriage sp_agelastmarriage
keep folio agelastmarriage sp_agelastmarriage sp_dead04 sp_dead06 sp_dead09

sort folio
save agelastmarriage, replace
