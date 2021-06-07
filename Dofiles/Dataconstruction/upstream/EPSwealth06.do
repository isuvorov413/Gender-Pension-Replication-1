version 8
capture log close
*log using "C:\DATA\EPSwealth\EPSwealth.log", replace
set more off
cd C:\DATA\EPSwealth

**********************************************************************************************
*Chilean Pension System: Wealth Data 2006
*****************************************
*by Clement Joubert
*August 20th 2008
*Revised January 2011
*Location: C:\DATA\EPSwealth

*OBJECT: 
*********

*Creates "EPSwealth06.dta" with a measure of household wealth variables from 2006

*DATASETS:
**********

*IN: C:\DATA\EPS06\entrevistado.dta
*OUT: C:\DATA\EPSwealth\EPSwealth06.dta

*NOTES:
*******
*Treatment of missing values:
* - replace by 0 for categories other than housing: A VERIFIER
* - for housing if estimated rent is not missing, impute using hedonic regression on rent and house characteristics: A FAIRE

*Top-coding and trimming
* A FAIRE

*Imputation of tranches
* A VERIFIER

*NOTE ON MISSING VALUES: missing values are left missing except the following imputations
*-ownership proportion is imputed with the median
*-if respondent doesn't know whether she has debt/savings/..., a 0 is imputed
*NOTE ON TOPCODING
*I don't top code for now in case it generates artificial variations (if a given respondent is top coded one year but not the other)
*NOTE ON TRANCHES
*For the upper tranche, I impute the lower bound instead of the median to improve consistency over rounds

***********************************************************************************************


clear
set memory 500000

use C:\DATA\EPS06\entrevistado.dta


*****************************************************************************************
*No response and neg values recoded as missing
*****************************************************************************************

replace d8=. if d8==9999999
replace d8=. if d8==999999
replace d8=. if d8==99999
replace d8=. if d8==1000000000
replace d8=. if d8==1.00e+08 		/* new*/
replace d14=. if d14==9999999
replace d14=. if d14==9999
replace d14=. if d14==99999
replace d14=. if d14==999999
replace d14=. if d14==1000000000
replace d19=. if d19==1000000000
*d19
*9989999616
*1000000000
replace d21=. if d21==10000000000
replace d21=. if d21==99999
replace d21=. if d21==999999
replace d21=. if d21==9999999
replace d22=. if d22==10000000000
replace d22=. if d22==99999
replace d22=. if d22==999999
replace d22=. if d22==9999999
*d22
*1000000000
replace d25_1=. if d25_1==1000000000
replace d25_1=. if d25_1==999999
replace d25_1=. if d25_1==9999999
replace d25_2=. if d25_2==1000000000
replace d25_3=. if d25_3==1000000000
replace d25_4=. if d25_4==1000000000
replace d25_4=. if d25_4==999999
replace d27_1m=. if d27_1m==1000000000
replace d27_1m=. if d27_1m==9999
replace d27_1m=. if d27_1m==99999
replace d27_1m=. if d27_1m==999999
replace d27_2m=. if d27_2m==1000000000
replace d27_2m=. if d27_2m==99999
replace d27_3m=. if d27_3m==1000000000
replace d27_3m=. if d27_3m==9999
replace d27_3m=. if d27_3m==99999
replace d27_3m=. if d27_3m==999999
replace d27_3m=. if d27_3m==9999999
replace d27_4m=. if d27_4m==1000000000
replace d27_4m=. if d27_4m==9999
replace d27_4m=. if d27_4m==99999
*d27_4
*1000000000
replace d27_5m=. if d27_5m==1000000000
***OJO hizo 342 cambios, igual harto***
replace d27_5m=. if d27_5m==9999
replace d27_5m=. if d27_5m==99999
replace d27_5m=. if d27_5m==999999
*d27_5
*1000000000
replace d27_6m=. if d27_6m==1000000000
replace d27_6m=. if d27_6m==9999
replace d27_6m=. if d27_6m==99999
replace d27_6m=. if d27_6m==9999999
replace d27_7m=. if d27_7m==1000000000
replace d27_7m=. if d27_7m==9999
replace d27_7m=. if d27_7m==999999
replace d27_8m=. if d27_8m==1000000000
replace d27_8m=. if d27_8m==9999
replace d27_8m=. if d27_8m==99999
replace d27_9m=. if d27_9m==1000000000
replace d27_10m=. if d27_10m==1000000000

replace d29_1=. if d29_1==1000000000
replace d29_1=. if d29_1==99999
replace d29_1=. if d29_1==999999
replace d29_1=. if d29_1==9999999
replace d29_2=. if d29_2==1000000000
replace d29_2=. if d29_2==99999
replace d29_2=. if d29_2==999999
replace d29_2=. if d29_2==9999999
replace d29_3=. if d29_3==1000000000
replace d29_3=. if d29_3==99999
replace d29_3=. if d29_3==9999999
replace d29_4=. if d29_4==1000000000
replace d29_4=. if d29_4==9999
replace d30_1=. if d30_1==10000000000
replace d30_1=. if d30_1==999999
replace d30_1=. if d30_1==9999999
replace d30_2=. if d30_2==10000000000
replace d30_3=. if d30_3==10000000000
replace d30_4=. if d30_4==10000000000
replace d34_1m=. if d34_1m>=10000000000
***igual se van dos valores que no empiezan con 1*****
replace d34_1m=. if d34_1m==9999
replace d34_1m=. if d34_1m==99999
replace d34_1m=. if d34_1m==999999
replace d34_1m=. if d34_1m==9999999
replace d34_2m=. if d34_2m>=10000000000
replace d34_2m=. if d34_2m==9999
replace d34_2m=. if d34_2m==99999
replace d37_1m=. if d37_1m==1000000000
***no hay observaciones****
replace d37_2m=. if d37_2m==1000000000
replace d37_2m=. if d37_2m==99999
replace d37_2m=. if d37_2m==999999
replace d37_2m=. if d37_2m==9999999
replace d37_3m=. if d37_3m==1000000000
replace d37_3m=. if d37_3m==99999
replace d37_3m=. if d37_3m==999999
replace d37_3m=. if d37_3m==9999999
replace d37_4m=. if d37_4m==1000000000
*****Ojo igual cambia mas de 400 valores***
replace d37_4m=. if d37_4m==99999
replace d37_4m=. if d37_4m==999999
replace d37_4m=. if d37_4m==9999999
replace d37_5m=. if d37_5m==1000000000
*****Ojo igual cambia mas de 100 valores***
replace d37_5m=. if d37_5m==99999
replace d37_5m=. if d37_5m==999999
replace d37_6m=. if d37_6m==1000000000
replace d37_6m=. if d37_6m==99999
replace d37_7m=. if d37_7m==1000000000
replace d37_8m=. if d37_8m==1000000000
replace d37_8m=. if d37_8m==999999
replace d37_9m=. if d37_9m==1000000000
replace d37_9m=. if d37_9m==99999
replace d37_9m=. if d37_9m==999999
replace d37_9m=. if d37_9m==9999999
replace d37_10m=. if d37_10m==1000000000
replace d37_11m=. if d37_11m==1000000000
replace d37_12m=. if d37_12m==1000000000
replace d37_12m=. if d37_12m==9
replace d37_12m=. if d37_12m==9999999

replace d40_1=. if d40_1==10000000
replace d40_1=. if d40_1==9999
replace d40_1=. if d40_1==99999
replace d40_1=. if d40_1==999999
replace d40_2=. if d40_2==9999999
replace d40_2=. if d40_2==9999
replace d40_2=. if d40_2==99999
replace d40_2=. if d40_2==999999
replace d40_3=. if d40_3==9999999
replace d40_3=. if d40_3==9999
replace d40_3=. if d40_3==99999
replace d40_3=. if d40_3==999999
replace d40_4=. if d40_4==9999999
replace d40_4=. if d40_4==9999
replace d40_4=. if d40_4==99999
replace d40_4=. if d40_4==999999
replace d40_5=. if d40_5==9999999
replace d40_5=. if d40_5==9999
replace d40_5=. if d40_5==99999
replace d40_5=. if d40_5==999999

replace d41_1=. if d41_1==9999999
replace d41_1=. if d41_1==9999
replace d41_1=. if d41_1==99999
replace d41_1=. if d41_1==999999
replace d41_2=. if d41_2==9999999
replace d41_2=. if d41_2==9999
replace d41_2=. if d41_2==99999
replace d41_2=. if d41_2==999999
replace d41_3=. if d41_3==9999999
replace d41_3=. if d41_3==9999
replace d41_3=. if d41_3==99999
replace d41_3=. if d41_3==999999
replace d41_4=. if d41_4==9999999
replace d41_4=. if d41_4==9999
replace d41_4=. if d41_4==99999
replace d41_4=. if d41_4==999999
replace d41_5=. if d41_5==9999999
replace d41_5=. if d41_5==9999
replace d41_5=. if d41_5==99999
replace d41_5=. if d41_5==999999
replace d44_1=. if d44_1==1000000000
***Ojo que ha hace como 500 cambios****
replace d44_1=. if d44_1==9999
replace d44_1=. if d44_1==99999
replace d44_1=. if d44_1==999999
replace d44_2=. if d44_2==1000000000
***Ojo que ha hace como 600 cambios****
replace d44_2=. if d44_2==9999
replace d44_2=. if d44_2==99999
replace d44_2=. if d44_2==999999
replace d44_2=. if d44_2==9999999
replace d44_3=. if d44_3==1000000000
replace d44_3=. if d44_3==9999
replace d44_3=. if d44_3==99999
replace d44_3=. if d44_3==9999999
replace d44_4=. if d44_4==1000000000
replace d44_4=. if d44_4==9999
replace d44_4=. if d44_4==99999
replace d44_4=. if d44_4==999999
replace d44_4=. if d44_4==9999999
replace d44_5=. if d44_5==1000000000
replace d44_5=. if d44_5==9999
replace d44_5=. if d44_5==99999
replace d44_5=. if d44_5==999999
replace d44_5=. if d44_5==9999999
replace d44_6=. if d44_6==1000000000
***Ojo que ha hace como 150 cambios****
replace d44_6=. if d44_6==9999
replace d44_6=. if d44_6==99999
replace d44_6=. if d44_6==9999999
replace d44_7=. if d44_7==1000000000
replace d44_7=. if d44_7==9999
replace d44_7=. if d44_7==99999
replace d44_8=. if d44_8==1000000000
replace d44_8=. if d44_8==9999
replace d44_8=. if d44_8==99999
replace d45_1=. if d45_1==1000000000 | d45_1==9999 | d45_1==99999 | d45_1==999999 | d45_1==9999999
***Ojo que ha hace como 1600 cambios****
replace d45_2=. if d45_2==1000000000 | d45_2==9999 | d45_2==99999 | d45_2==999999 | d45_2==9999999
***Ojo que ha hace como 1200 cambios****
replace d45_3=. if d45_3==1000000000 | d45_3==9999 | d45_3==99999 | d45_3==999999 | d45_3==9999999
replace d45_4=. if d45_4==1000000000 | d45_4==9999 | d45_4==99999 | d45_4==999999 | d45_4==9999999
replace d45_5=. if d45_5==1000000000 | d45_5==9999 | d45_5==99999 | d45_5==999999 | d45_5==9999999
replace d45_6=. if d45_6==1000000000 | d45_6==9999 | d45_6==99999 | d45_6==999999 | d45_6==9999999
replace d45_7=. if d45_7==1000000000 | d45_7==9999 | d45_7==99999 | d45_7==999999 | d45_7==9999999
replace d45_8=. if d45_8==1000000000 | d45_8==9999 | d45_8==99999 | d45_8==999999 | d45_8==9999999
replace d48_1=. if d48_1==1000000000 | d48_1==99999 | d48_1==999999 | d48_1==9999999

*************************************************************************************
****Se censura el los valores del percentil 99 para todas las variables de monto*****
*************************************************************************************

***DOES THAT GENERATE UNWANTED VARIATION IN THE WEALTH PANEL?

sum d8, detail
replace d8=r(p99) if d8>r(p99) & d8!=.
sum d14, detail
replace d14=r(p99) if d14>r(p99) & d14!=.
sum d19, detail
replace d19=r(p99) if d19>r(p99) & d19!=.
sum d21, detail
replace d21=r(p99) if d21>r(p99) & d21!=.
sum d22, detail
replace d22=r(p99) if d22>r(p99) & d22!=.
sum d25_1, detail
replace d25_1=r(p99) if d25_1>r(p99) & d25_1!=.
sum d25_2, detail
replace d25_2=r(p99) if d25_2>r(p99) & d25_2!=.
sum d25_3, detail
replace d25_3=r(p99) if d25_3>r(p99) & d25_3!=.
sum d25_4, detail
replace d25_4=r(p99) if d25_4>r(p99) & d25_4!=.

local i=1
while `i'<=10{
sum d27_`i'm, detail
replace d27_`i'm=r(p99) if d27_`i'm>r(p99) & d27_`i'm!=.
local i=`i'+1
}

local i=1
while `i'<=4{
sum d29_`i', detail
replace d29_`i'=r(p99) if d29_`i'>r(p99) & d29_`i'!=.
local i=`i'+1
}

local i=1
while `i'<=4{
sum d30_`i', detail
replace d30_`i'=r(p99) if d30_`i'>r(p99) & d30_`i'!=.
local i=`i'+1
}

sum d34_1m, detail
replace d34_1m=r(p99) if d34_1m>r(p99) & d34_1m!=.
sum d34_2m, detail
replace d34_2m=r(p99) if d34_2m>r(p99) & d34_2m!=.

local i=1
while `i'<=12{
sum d37_`i'm, detail
replace d37_`i'm=r(p99) if d37_`i'm>r(p99) & d37_`i'm!=.
local i=`i'+1
}

local i=2
while `i'<=5{
sum d40_`i', detail
replace d40_`i'=r(p99) if d40_`i'>r(p99) & d40_`i'!=.
local i=`i'+1
}

local i=1
while `i'<=5{
sum d41_`i', detail
replace d41_`i'=r(p99) if d41_`i'>r(p99) & d41_`i'!=.
local i=`i'+1
}

local i=1
while `i'<=8{
sum d44_`i', detail
replace d44_`i'=r(p99) if d44_`i'>r(p99) & d44_`i'!=.
local i=`i'+1
}

local i=1
while `i'<=8{
sum d45_`i', detail
replace d45_`i'=r(p99) if d45_`i'>r(p99) & d45_`i'!=.
local i=`i'+1
}

sum d48_1, detail
replace d48_1=r(p99) if d48_1>r(p99) & d48_1!=.
*/

****************************************************
******Reemplazar por ceros los missing values*******
****************************************************

***DOES THAT GENERATE UNWANTED VARIATION IN THE WEALTH PANEL?

/*
replace d8=0 if d8==.
replace d8=0 if d8==.
replace d14=0 if d14==.
replace d14=0 if d14==.
replace d17=0 if d17==.
replace d19=0 if d19==.
replace d21=0 if d21==.
replace d22=0 if d22==.
replace d25_1=0 if d25_1==.
replace d25_2=0 if d25_2==.
replace d25_3=0 if d25_3==.
replace d25_4=0 if d25_4==.
replace d27_1m=0 if d27_1m==.
replace d27_2m=0 if d27_2m==.
replace d27_3m=0 if d27_3m==.
replace d27_4m=0 if d27_4m==.
replace d27_5m=0 if d27_5m==.
replace d27_6m=0 if d27_6m==.
replace d27_7m=0 if d27_7m==.
replace d27_8m=0 if d27_8m==.
replace d27_9m=0 if d27_9m==.
replace d27_10m=0 if d27_10m==.
replace d29_1=0 if d29_1==.
replace d29_2=0 if d29_2==.
replace d29_3=0 if d29_3==.
replace d29_4=0 if d29_4==.
replace d30_1=0 if d30_1==.
replace d30_2=0 if d30_2==.
replace d30_3=0 if d30_3==.
replace d30_4=0 if d30_4==.
replace d34_1m=0 if d34_1m>=.
replace d34_2m=0 if d34_2m>=.
replace d37_1m=0 if d37_1m==.
replace d37_2m=0 if d37_2m==.
replace d37_3m=0 if d37_3m==.
replace d37_4m=0 if d37_4m==.
replace d37_5m=0 if d37_5m==.
replace d37_6m=0 if d37_6m==.
replace d37_7m=0 if d37_7m==.
replace d37_8m=0 if d37_8m==.
replace d37_9m=0 if d37_9m==.
replace d37_10m=0 if d37_10m==.
replace d37_11m=0 if d37_11m==.
replace d37_12m=0 if d37_12m==.
replace d40_1=0 if d40_1==.
replace d40_2=0 if d40_2==.
replace d40_3=0 if d40_3==.
replace d40_4=0 if d40_4==.
replace d40_5=0 if d40_5==.
replace d41_1=0 if d41_1==.
replace d41_2=0 if d41_2==.
replace d41_3=0 if d41_3==.
replace d41_4=0 if d41_4==.
replace d41_5=0 if d41_5==.
replace d44_1=0 if d44_1==.
replace d44_2=0 if d44_2==.
replace d44_3=0 if d44_3==.
replace d44_4=0 if d44_4==.
replace d44_5=0 if d44_5==.
replace d44_6=0 if d44_6==.
replace d44_7=0 if d44_7==.
replace d44_8=0 if d44_8==.
replace d45_1=0 if d45_1==.
replace d45_2=0 if d45_2==.
replace d45_3=0 if d45_3==.
replace d45_4=0 if d45_4==.
replace d45_5=0 if d45_5==.
replace d45_6=0 if d45_6==.
replace d45_7=0 if d45_7==.
replace d45_8=0 if d45_8==.
replace d48_1=0 if d48_1==.
*/

*****************************************************
*******Construccion de variables de patrimonio*******
*****************************************************

***Valor de la vivienda para los propietarios********

ta d7,m
*10051+2084+276=12411 owners and 4032 non owners

ta d17 d7, m
*2414 owners don't know the exact value of their house

ta d17t d7, m
ta d17 d17t
*1019 owners don't know the exact or approximate value of their house

ta  d8 if d17t==99, m
*504 of the 1019 know how much rent they would pay for the house

ta  d8 if d17t==99, m
*all 1019 know the number of bedrooms

ta   d14  if d17t==99 & d7==2, m
ta   d14t  if d17t==99 & d7==2, m
*70 housevalues can be infered from mortgage payments

ta  d9 if d17t==99,m
*in 560 cases the repsondent is not the owner but rather another family member
*could impute 0 if it is neither the spouse or the son?

ta  d6 if d17t==99,m 
*690 of the 1019 know the date of construction

su d17, detail

replace d17=. if d17==1000000000


g vivienda=.
replace vivienda=0 if d7==4|d7==5|d7==6
replace vivienda=d17 if d7==1 | d7==3 
replace vivienda=5000000 if d17t==1 & d17==. & (d7==1 | d7==3)
replace vivienda=15000000 if d17t==2 & d17==. & (d7==1 | d7==3)
replace vivienda=25000000 if d17t==3 & d17==. & (d7==1 | d7==3)
replace vivienda=35000000 if d17t==4 & d17==. & (d7==1 | d7==3)
replace vivienda=45000000 if d17t==5 & d17==. & (d7==1 | d7==3)
replace vivienda=55000000 if d17t==6 & d17==. & (d7==1 | d7==3)
replace vivienda=65000000 if d17t==7 & d17==. & (d7==1 | d7==3)
replace vivienda=75000000 if d17t==8 & d17==. & (d7==1 | d7==3)
replace vivienda=90000000 if d17t==9 & d17==. & (d7==1 | d7==3)
replace vivienda=100000000 if d17t==10 & d17==. & (d7==1 | d7==3)



****Para los entrevistados que aún están pagando la vivienda, se imputa el valor de la vivienda proporcionalmente al tiempo pagado*****
****NOTE: propown is imputed if missing****

ta vivienda, m




ta  d15a d7
ta  d15m d7
ta  d16a d7
ta  d16m d7

replace d15a=. if d15a==99
replace d15m=. if d15m==99
replace d16a=. if d16a==99
replace d16m=. if d16m==99

g propown=(d15a*12+d15m)/(d15a*12+d15m+d16a*12+d16m)
sum propown, detail
replace propown=r(p50) if propown==. & d7==2
*IMPUTED propown for 390
*better to impute years and months for each of the 4 questions (especially month, when years is available)


replace vivienda=d17*propown if d7==2
replace vivienda=5000000*propown if d17t==1 & d17==. & (d7==2)
replace vivienda=15000000*propown if d17t==2 & d17==. & (d7==2)
replace vivienda=25000000*propown if d17t==3 & d17==. & (d7==2)
replace vivienda=35000000*propown if d17t==4 & d17==. & (d7==2)
replace vivienda=45000000*propown if d17t==5 & d17==. & (d7==2)
replace vivienda=55000000*propown if d17t==6 & d17==. & (d7==2)
replace vivienda=65000000*propown if d17t==7 & d17==. & (d7==2)
replace vivienda=75000000*propown if d17t==8 & d17==. & (d7==2)
replace vivienda=90000000*propown if d17t==9 & d17==. & (d7==2)
replace vivienda=100000000*propown if d17t==10 & d17==. & (d7==2)




****Otros Bienes raíces******

g bsraices=.

replace bsraices=0 if d18==2|d18==9
replace bsraices=d19 if d18==1 & d20==1

replace bsraices=5000000 if d19t==1 & d19==. & (d18==1 & d20==1)
replace bsraices=15000000 if d19t==2 & d19==. & (d18==1 & d20==1)
replace bsraices=25000000 if d19t==3 & d19==. & (d18==1 & d20==1)
replace bsraices=35000000 if d19t==4 & d19==. & (d18==1 & d20==1)
replace bsraices=45000000 if d19t==5 & d19==. & (d18==1 & d20==1)
replace bsraices=55000000 if d19t==6 & d19==. & (d18==1 & d20==1)
replace bsraices=65000000 if d19t==7 & d19==. & (d18==1 & d20==1)
replace bsraices=75000000 if d19t==8 & d19==. & (d18==1 & d20==1)
replace bsraices=90000000 if d19t==9 & d19==. & (d18==1 & d20==1)
sum d19 if d19>100000000 & (d18==1 & d20==1), detail
replace bsraices=100000000 if d19t==10 & d19==. & (d18==1 & d20==1)

ta bsraices d20, m

****Deuda en bienes raíces******

g deudabsraices=0 
replace deudabsraices=d21 if d20==2

*NOTE: noone gives a "tramo" answer for deudas raices
*NOTE: Impute a value to missing deudabsraices? 0?

***Valor de la propiedad de bien raíz para los que tienen deuda************

replace bsraices=d19-deudabsraices if d18==1 & d20==2

replace bsraices=5000000-deudabsraices if d19t==1 & d19==. & (d18==1 & d20==2)
replace bsraices=15000000-deudabsraices if d19t==2 & d19==. & (d18==1 & d20==2)
replace bsraices=25000000-deudabsraices if d19t==3 & d19==. & (d18==1 & d20==2)
replace bsraices=35000000-deudabsraices if d19t==4 & d19==. & (d18==1 & d20==2)
replace bsraices=45000000-deudabsraices if d19t==5 & d19==. & (d18==1 & d20==2)
replace bsraices=55000000-deudabsraices if d19t==6 & d19==. & (d18==1 & d20==2)
replace bsraices=65000000-deudabsraices if d19t==7 & d19==. & (d18==1 & d20==2)
replace bsraices=75000000-deudabsraices if d19t==8 & d19==. & (d18==1 & d20==2)
replace bsraices=90000000-deudabsraices if d19t==9 & d19==. & (d18==1 & d20==2)
sum d19 if d19>100000000 & (d18==1 & d20==2), detail
replace bsraices=100000000-deudabsraices if d19t==10 & d19==. & (d18==1 & d20==2)
replace bsraices=. if bsraices==1000000000 & bsraices!=.

replace bsraices=0 if bsraices<0

ta bsraices, m

*******Valor de los vehículos******
replace d25_1=0 if d23_1==2|d23_1==9
replace d25_2=0 if d23_2==2|d23_2==9
replace d25_3=0 if d23_3==2|d23_3==9
replace d25_4=0 if d23_4==2|d23_4==9

*Impute the median value when sale value is missing? Proportional to number of vehicles?

g autos=d25_1+d25_2+d25_3+d25_4

*******Ahorros*********************

*impute 0 when don't know that they have savings

replace d27_1m=0 if d27_1==2 | d27_1==9
replace d27_2m=0 if d27_2==2| d27_2==9
replace d27_3m=0 if d27_3==2| d27_3==9
replace d27_4m=0 if d27_4==2| d27_4==9
replace d27_5m=0 if d27_5==2| d27_5==9
replace d27_6m=0 if d27_6==2| d27_6==9
replace d27_7m=0 if d27_7==2| d27_7==9
replace d27_8m=0 if d27_8==2| d27_8==9
replace d27_9m=0 if d27_9==2| d27_9==9
replace d27_10m=0 if d27_10==2| d27_10==9
g ahorro_vivienda=d27_1m+d27_2m
g ahorro_previsional=d27_3m+d27_4m
g ahorro_cuentaahorro=d27_5m
g ahorro_financiero=d27_6m+d27_7m+d27_8m
g ahorro_otros=d27_9m+d27_10m

g ahorros=d27_1m+d27_2m+d27_3m+d27_4m+d27_5m+d27_6m+d27_7m+d27_8m+d27_9m+d27_10m

*****Otros activos*****************
replace d29_1=0 if d28_1==2 | d28_1==9
replace d29_2=0 if d28_2==2 |d28_2==9
replace d29_3=0 if d28_3==2 |d28_3==9
replace d29_4=0 if d28_4==2 |d28_4==9
replace d30_1=0 if d28_1==2 |d28_1==9
replace d30_2=0 if d28_2==2 |d28_2==9
replace d30_3=0 if d28_3==2 |d28_3==9
replace d30_4=0 if d28_4==2 |d28_4==9

g maquinas= d29_1+d29_2+d29_3+d29_4-d30_1-d30_2-d30_3-d30_4

replace maquinas=0 if maquinas<0

ta maquinas, m

*IMPUTE missing amount ==0
replace d34_1m=0 if d34==2 | d34==3 | d34==.
replace d34_2m=0 if d34==1 | d34==3 | d34==.

g negocios=d34_1m-d34_2m
ta negocios, m

******DEUDAS***********************

replace d37_1m=0 if d37_1==2 | d37_1==9
replace d37_2m=0 if d37_2==2| d37_2==9
replace d37_3m=0 if d37_3==2| d37_3==9
replace d37_4m=0 if d37_4==2| d37_4==9
replace d37_5m=0 if d37_5==2| d37_5==9
replace d37_6m=0 if d37_6==2 |d37_6==9
replace d37_7m=0 if d37_7==2|d37_7==9
replace d37_8m=0 if d37_8==2|d37_8==9
replace d37_9m=0 if d37_9==2|d37_9==9
replace d37_10m=0 if d37_10==2|d37_10==9
replace d37_11m=0 if d37_11==2|d37_11==9
replace d37_12m=0 if d37_12==2|d37_12==9

g deuda_corriente=d37_1m
g deuda_linea=d37_2m
g deuda_TCbancaria=d37_3m
g deuda_CC=d37_4m
g deuda_CConsumo=d37_5m+d37_6m
g deuda_automotriz=d37_7m
g deuda_educ=d37_9m
g deuda_otros=d37_8m+d37_10m+d37_11m+d37_12m

*note don't include cuenta corriente in the debt
*note noone knows their debt in checking account

g deudas=d37_2m+d37_3m+d37_4m+d37_5m+d37_6m+d37_7m+d37_8m+d37_9m+d37_10m+d37_11m+d37_12m

ta deudas, m

*keep folio  vivienda bsraices autos ahorros maquinas negocios deudas ahorro_vivienda ahorro_previsional ahorro_cuentaahorro ahorro_financiero ahorro_otros deuda_linea deuda_TCbancaria deuda_CC deuda_CConsumo deuda_automotriz deuda_educ deuda_otros

label variable vivienda "valor de la vivienda"
label variable bsraices "valor de otros bienes raíces"
label variable autos "valor de automoviles"
label variable ahorros "monto total ahorrado"
label variable maquinas "valor maquinas"
label variable negocios "valor saldo de los negocios"
label variable deudas "monto de deudas"


sort folio


**********ADDED BY CLEMENT***************

rename vivienda vivienda06
rename bsraices bsraices06
rename autos autos06
rename ahorros ahorros06
rename maquinas maquinas06
rename negocios negocios06
rename deudas deudas06
rename ahorro_vivienda ahorro_vivienda06
rename ahorro_previsional ahorro_previsional06
rename ahorro_cuentaahorro ahorro_cuentaahorro06
rename ahorro_financiero ahorro_financiero06
rename ahorro_otros ahorros_otros06
rename deuda_linea deuda_linea06
rename deuda_TCbancaria deuda_TCbancaria06
rename deuda_CC deuda_CC06
rename deuda_CConsumo deuda_CConsumo06
rename deuda_automotriz deuda_automotriz06
rename deuda_educ deuda_educ06
rename deuda_otros deuda_otros06

g ahorro_liquido06=d27_1m+d27_2m+d27_4m+d27_5m+d27_6m+d27_7m+d27_8m+d27_9m+d27_10m 						/*contains all savings except for voluntary contributions*/
g apv06=d27_3m																					/*contains voluntary contributions*/
gen patrimonio_liquido06= vivienda06+ bsraices06 + autos06 + ahorro_liquido06 + maquinas06 + negocios06-deudas06						

g test=ahorros06-apv06-ahorro_liquido06
su test

rename patrimonio_liquido06 wealth06
rename vivienda06 housing06
rename bsraices06 realestate06
rename autos06 cars06
rename maquinas06 equipment06
rename negocios06 business06
rename deudas06 debt06
rename apv06 retirement06
rename ahorro_liquido06 savings06


keep folio factor_EPS06 wealth06 housing06 realestate06 cars06 savings06 equipment06 business06 debt06 retirement06

************************************************

sort folio
save C:\DATA\EPSWealth\wealth06.dta, replace

