
version 8
capture log close
*log using "C:\DATA\EPSwealth\EPSwealth.log", replace
set more off
cd C:\DATA\EPSwealth

**********************************************************************************************
*Chilean Pension System: Wealth Data 2004
******************************************
*by Clement Joubert
*August 20th 2008
*Revised January 2011
*Location: C:\DATA\EPSwealth

*OBJECT: 
*********

*Creates "EPSwealth04.dta" with a measure of household wealth variables from 2004

*DATASETS:
**********

*IN: C:\DATA\EPS04\entrevistado.dta
*OUT: C:\DATA\EPSwealth\EPSwealth04.dta

*NOTES:
*******
*Treatment of missing values:
* - replace by 0 for categories other than housing: A VERIFIER
* - for housing if estimated rent is not missing, impute using hedonic regression on rent and house characteristics: A FAIRE

*Top-coding and trimming
* A FAIRE

*Imputation of tranches
* A VERIFIER

***********************************************************************************************


clear
set memory 500000

use C:\DATA\EPS04\entrevistado.dta


*****************************************************************************************
*No response and neg values recoded as missing
*****************************************************************************************

replace d8    =. if d8<0
replace d8t   =. if d8<0
replace d12_01=. if d12_01<0
replace d12_02=. if d12_02<0	
replace d12t  =. if d12t<0
replace d13_01=. if d13_01<0
replace d13_02=. if d13_02<0
replace d14_01=. if d14_01<0
replace d14_02=. if d14_02<0
replace d15_01=. if d15_01<0
replace d15_02=. if d15_02<0
replace d17_01=. if d17_01<0
replace d17_02=. if d17_02<0
replace d19_01=. if d19_01<0
replace d19_02=. if d19_02<0

replace d21b_01=. if d21b_01<0 | d21_01<0						/*cars*/
replace d21b_02=. if d21b_02<0 | d21_02<0      					/*motorbikes*/
replace d21b_03=. if d21b_03<0 | d21_03<0						/*pick-ups*/
replace d21b_04=. if d21b_04<0 | d21_04<0						/*other vehicles*/
replace d23m_01=. if d23m_01<0 | d23_01<0
replace d23m_02=. if d23m_02<0 | d23_02<0
replace d23m_03=. if d23m_03<0 | d23_03<0
replace d23m_04=. if d23m_04<0 | d23_04<0
replace d23m_05=. if d23m_05<0 | d23_05<0
replace d23m_06=. if d23m_06<0 | d23_06<0
replace d23m_07=. if d23m_07<0 | d23_07<0
replace d23m_08=. if d23m_08<0 | d23_08<0
replace d23m_09=. if d23m_09<0 | d23_09<0
replace d23m_0a=. if d23m_0a<0 | d23_0a<0


replace d24b_01=. if d24b_01<0 | d24a_01<0			/*machinery and equipement*/
replace d24b_02=. if d24b_02<0 | d24a_02<0			/*land*/
replace d24b_03=. if d24b_03<0 | d24a_03<0			/*livestock*/
replace d24b_04=. if d24b_04<0 | d24a_04<0			/*other*/
replace d24c_01=. if d24c_01<0 | d24a_01<0
replace d24c_02=. if d24c_02<0 | d24a_02<0			/*c is debt, b is positive value*/
replace d24c_03=. if d24c_03<0 | d24a_03<0
replace d24c_04=. if d24c_04<0 | d24a_04<0

replace d30m_01=. if d30m_01<0
replace d30m_02=. if d30m_02<0
replace d30m_03=. if d30m_03<0
replace d30m_04=. if d30m_04<0
replace d30m_05=. if d30m_05<0
replace d30m_06=. if d30m_06<0
replace d30m_07=. if d30m_07<0
replace d30m_08=. if d30m_08<0
replace d30m_09=. if d30m_09<0
replace d30m_0a=. if d30m_0a<0
replace d30m_0b=. if d30m_0b<0



*******************************************************************************************
*Top coding
*******************************************************************************************

sum d8, detail
replace d8=r(p99) if d8>r(p99) & d8!=.
sum d12_01, detail
replace d12_01=r(p99) if d12_01>r(p99) & d12_01!=.
sum d12_02, detail
replace d12_02=r(p90) if d12_02>r(p90) & d12_02!=.

sum d15_01, detail
replace d15_01=r(p99) if d15_01>r(p99) & d15_01!=.
sum d15_02, detail
replace d15_02=r(p95) if d15_02>r(p95) & d15_02!=.


sum d17_01, detail
replace d17_01=r(p99) if d17_01>r(p99) & d17_01!=.
sum d17_02, detail
replace d17_02=r(p90) if d17_02>r(p90) & d17_02!=.


sum d19_01, detail
replace d19_01=r(p99) if d19_01>r(p99) & d19_01!=.
sum d19_02, detail
replace d19_02=r(p99) if d19_02>r(p99) & d19_02!=.
sum d21b_01, detail
replace d21b_01=r(p99) if d21b_01>r(p99) & d21b_01!=.
sum d21b_02, detail
replace d21b_02=r(p99) if d21b_02>r(p99) & d21b_02!=.
sum d21b_03, detail
replace d21b_03=r(p95) if d21b_03>r(p95) & d21b_03!=.
sum d21b_04, detail
replace d21b_04=r(p95) if d21b_04>r(p95) & d21b_04!=.

/*
replace d21b_01=50000000 if d21b_01>50000000 & d21a_01==1	& d21b_01!=. 	/*Top coding if owns 1 car*/
replace d21b_01=100000000 if d21b_01>100000000 & d21a_01>1	& d21b_01!=. 	/*Top coding if owns several cars*/
replace d21b_04=50000000 if d21b_04>50000000 & d21a_04==1 & d21b_01!=. 
replace d21b_04=100000000 if d21b_04>100000000 & d21a_04>1 & d21b_01!=. 
*/
sum d23m_01, detail
replace d23m_01=r(p99) if d23m_01>r(p99) & d23m_01!=.
sum d23m_02, detail
replace d23m_02=r(p99) if d23m_02>r(p99) & d23m_02!=.
sum d23m_03, detail
replace d23m_03=r(p99) if d23m_03>r(p99) & d23m_03!=.
sum d23m_04, detail
replace d23m_04=r(p95) if d23m_04>r(p95) & d23m_04!=.
sum d23m_05, detail
replace d23m_05=r(p99) if d23m_05>r(p99) & d23m_05!=.
sum d23m_06, detail
replace d23m_06=r(p95) if d23m_06>r(p95) & d23m_06!=.
sum d23m_07, detail
replace d23m_07=r(p95) if d23m_07>r(p95) & d23m_07!=.
sum d23m_08, detail
replace d23m_08=r(p99) if d23m_08>r(p99) & d23m_08!=.
sum d23m_09, detail
replace d23m_09=r(p99) if d23m_09>r(p99) & d23m_09!=.
sum d23m_0a, detail
replace d23m_0a=r(p99) if d23m_0a>r(p99) & d23m_0a!=.

sum d24b_01, detail
replace d24b_01=r(p95) if d24b_01>r(p95) & d24b_01!=.
sum d24b_02, detail
replace d24b_02=r(p99) if d24b_02>r(p99) & d24b_02!=.
sum d24b_03, detail
replace d24b_03=r(p99) if d24b_03>r(p99) & d24b_03!=.
sum d24b_04, detail
replace d24b_04=r(p99) if d24b_04>r(p99) & d24b_04!=.

sum d24c_01, detail
replace d24c_01=r(p99) if d24c_01>r(p99) & d24c_01!=.
sum d24c_02, detail
replace d24c_02=r(p99) if d24c_02>r(p99) & d24c_02!=.
sum d24c_03, detail
replace d24c_03=r(p99) if d24c_03>r(p99) & d24c_03!=.
sum d24c_04, detail
replace d24c_04=r(p99) if d24c_04>r(p99) & d24c_04!=.

/*
replace d24b_02=800000000 if d24b_02>800000000 & d24b_02!=.		/*equipement top coded at 800milions*/
replace d24c_01=d24b_01 if d24b_01<d24c_01
replace d24c_02=d24b_02 if d24b_02<d24c_02
replace d24c_03=d24b_03 if d24b_03<d24c_03
replace d24c_04=d24b_04 if d24b_04<d24c_04
*/

su d28_01, detail
replace d28_01=r(p99) if d28_01>r(p99) & d28_01!=.
su d28_02, detail
replace d28_02=r(p99) if d28_02>r(p99) & d28_02!=.

sum d30m_01, detail
replace d30m_01=r(p99) if d30m_01>r(p99) & d30m_01!=.
sum d30m_02, detail
replace d30m_02=r(p99) if d30m_02>r(p99) & d30m_02!=.
sum d30m_03, detail
replace d30m_03=r(p99) if d30m_03>r(p99) & d30m_03!=.
sum d30m_04, detail
replace d30m_04=r(p99) if d30m_04>r(p99) & d30m_04!=.
sum d30m_05, detail
replace d30m_05=r(p99) if d30m_05>r(p99) & d30m_05!=.
sum d30m_06, detail
replace d30m_06=r(p99) if d30m_06>r(p99) & d30m_06!=.
sum d30m_07, detail
replace d30m_07=r(p99) if d30m_07>r(p99) & d30m_07!=.
sum d30m_08, detail
replace d30m_08=r(p99) if d30m_08>r(p99) & d30m_08!=.
sum d30m_09, detail
replace d30m_09=r(p99) if d30m_09>r(p99) & d30m_09!=.
sum d30m_0a, detail
replace d30m_0a=r(p99) if d30m_0a>r(p99) & d30m_0a!=.
sum d30m_0b, detail
replace d30m_0b=r(p99) if d30m_0b>r(p99) & d30m_0b!=.



***************************************************************************************************
*Replace amount by 0 if the respondent doesn't own anything (or doesn't know whether he does) in that category
****************************************************************************************************

replace d21b_01=0 if d21_01==2
replace d21b_02=0 if d21_02==2							
replace d21b_03=0 if d21_03==2
replace d21b_04=0 if d21_04==2
replace d23m_01=0 if d23_01==2							/*Bank house savings*/
replace d23m_02=0 if d23_02==2							/*Housing fund administrator (afv)*/	
replace d23m_03=0 if d23_03==2							/*Ahorro Previsional voluntario*/
replace d23m_04=0 if d23_04==2							/*Ahorro voluntario (cuenta 2)*/
replace d23m_05=0 if d23_05==2							/*Bank saving account*/
replace d23m_06=0 if d23_06==2							/*Term deposits*/
replace d23m_07=0 if d23_07==2							/*Mutual fund investments*/
replace d23m_08=0 if d23_08==2							/*Company share or bonds*/
replace d23m_09=0 if d23_09==2							/*Third party loans*/
replace d23m_0a=0 if d23_0a==2							/*other savings*/


replace d30m_01=0 if d30_01==2							/*credit card debt*/
replace d30m_02=0 if d30_02==2							/*bank credit line*/
replace d30m_03=0 if d30_03==2							/*store credit*/
replace d30m_04=0 if d30_04==2							/*bank loans*/
replace d30m_05=0 if d30_05==2							/*consumer loans*/
replace d30m_06=0 if d30_06==2							/*car loan*/
replace d30m_07=0 if d30_07==2							/*government social support loans*/
replace d30m_08=0 if d30_08==2							/*loans from friends/relatives*/
replace d30m_09=0 if d30_09==2							/*moneylender loans*/
replace d30m_0a=0 if d30_0a==2		
replace d30m_0b=0 if d30_0b==2

replace d21b_01=0 if d21_01==3
replace d21b_02=0 if d21_02==3							
replace d21b_03=0 if d21_03==3
replace d21b_04=0 if d21_04==3
replace d23m_01=0 if d23_01==3							/*Bank house savings*/
replace d23m_02=0 if d23_02==3							/*Housing fund administrator (afv)*/	
replace d23m_03=0 if d23_03==3							/*Ahorro Previsional voluntario*/
replace d23m_04=0 if d23_04==3							/*Ahorro voluntario (cuenta 2)*/
replace d23m_05=0 if d23_05==3							/*Bank saving account*/
replace d23m_06=0 if d23_06==3							/*Term deposits*/
replace d23m_07=0 if d23_07==3							/*Mutual fund investments*/
replace d23m_08=0 if d23_08==3							/*Company share or bonds*/
replace d23m_09=0 if d23_09==3							/*Third party loans*/
replace d23m_0a=0 if d23_0a==3							/*other savings*/


replace d30m_01=0 if d30_01==3							/*credit card debt*/
replace d30m_02=0 if d30_02==3							/*bank credit line*/
replace d30m_03=0 if d30_03==3							/*store credit*/
replace d30m_04=0 if d30_04==3							/*bank loans*/
replace d30m_05=0 if d30_05==3							/*consumer loans*/
replace d30m_06=0 if d30_06==3							/*car loan*/
replace d30m_07=0 if d30_07==3							/*government social support loans*/
replace d30m_08=0 if d30_08==3							/*loans from friends/relatives*/
replace d30m_09=0 if d30_09==3							/*moneylender loans*/
replace d30m_0a=0 if d30_0a==3		
replace d30m_0b=0 if d30_0b==3



************************************
/*1.       MAIN HOUSE (vivienda)*/
************************************


/*Value of the house of the owners*/
**************************************

ta d7,m
*10339+2419+249=13007 owners and 3720 non owners

ta d15_01 d7, m
*2994 owners don't know the exact value of their house in pesos

ta d15_02 d7 if d15_01==. & d15_02!=., m
*162 know the exact value in UFs

ta d15t d7, m
ta d15t if d15_01!=. | d15_02!=.
*860 owners don't know the exact or approximate value of their house


ta  d8 if d15t<0, m
*570 of the 860 know how much rent they would pay for the house

ta  d3_01 if d15t<0, m
*all 860 but 5 know the number of bedrooms

ta   d12_01  if d15t<0 & d7==2, m
ta   d12_02  if d15t<0 & d7==2, m
ta   d12t  if d15t<0 & d7==2, m
*84 housevalues might be infered from mortgage payments

ta  d9 if d15t<0,m
*in 534 cases the repsondent is not the owner but rather another family member
*could impute 0 if it is neither the spouse or the son?

ta  d6 if d15t<0,m 
*691 of the 860 know the date of construction


gen vivienda=.
replace vivienda=0            if  d7==4| d7==5|d7==6
replace vivienda=d15_01       if  d7==1| d7==3
replace vivienda=d15_02*17000 if (d7==1| d7==3)& (d15_01==0|d15_01==.) & d15_02>0 /*1 UF=17.000 pesos*/ 	/*choose UF value if peso value is 0*/
replace vivienda=5000000      if d15t==1 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0		/*value expressed in tramos*/
replace vivienda=15000000     if d15t==2 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=25000000     if d15t==3 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=35000000     if d15t==4 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=45000000     if d15t==5 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=55000000     if d15t==6 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=65000000     if d15t==7 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=75000000     if d15t==8 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=90000000     if d15t==9 & (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=120000000    if d15t==10& (d7==1| d7==3) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0


ta vivienda d7, m


/*For the interviewees that actually are paying the property, it imputes the proportional value of the property according to what they have paid*/
************************************************************************************************************************************************************

gen propown=(d13_01*12+d13_02)/(d13_01*12+d13_02+d14_01*12+d14_02) 				/*Proportion of the credit that is paid*/ 
sum propown, detail
replace propown=r(p50) if propown==. & d7==2
*IMPUTED propown for 231


replace vivienda=d15_01*propown       if d7==2
replace vivienda=d15_02*17000*propown if           (d7==2) & (d15_01==0|d15_01==.)  & d15_02>0
replace vivienda=5000000*propown      if d15t==1 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=15000000*propown     if d15t==2 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=25000000*propown     if d15t==3 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=35000000*propown     if d15t==4 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=45000000*propown     if d15t==5 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=55000000*propown     if d15t==6 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=65000000*propown     if d15t==7 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=75000000*propown     if d15t==8 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=90000000*propown     if d15t==9 & (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0
replace vivienda=120000000*propown    if d15t==10& (d7==2) & (d15_01==0 | d15_01==.) & (d15_02==0|d15_02==.) & d15t>0

ta vivienda, m

rename vivienda vivienda04



******************************************
/*2.       OTHER PROPERTY (bsraices)*/
******************************************


/*Value of other property*/
****************************

gen gross_bsraices=.
replace gross_bsraices=0 if d16==2|d16<0
replace gross_bsraices=d17_01 if d16==1 & d18==1										/*d16 and d18 identify fully owned other property*/
replace gross_bsraices=d17_02*17000 if d16==1 & d18==1 &(d17_01==0 | d17_01==.)  & d17_02>0
replace gross_bsraices=5000000 if d17t==1 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=15000000 if d17t==2 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=25000000 if d17t==3 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=35000000 if d17t==4 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=45000000 if d17t==5 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=55000000 if d17t==6 & (d16==1& d18==1) &(d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=65000000 if d17t==7 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.)& d17t>0
replace gross_bsraices=75000000 if d17t==8 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=90000000 if d17t==9 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace gross_bsraices=100000000 if d17t==10 & (d16==1& d18==1) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0

ta gross_bsraices d18, m


/*Amount that left to paid of the second property (deudasraices)*/
*********************************************************************

gen deudaraices=0
replace deudaraices=d19_01 if d18==2
replace deudaraices=d19_02 if d18==2 & (d19_01==0|d19_01==.) & d19_02>0
replace deudaraices=1250000 if d19t==1 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=3750000 if d19t==2 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=7500000 if d19t==3 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=12500000 if d19t==4 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=17500000 if d19t==5 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=22500000 if d19t==6 & (d18==2) &(d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=27500000 if d19t==7 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=35000000 if d19t==8 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=45000000 if d19t==9 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=55000000 if d19t==10 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=70000000 if d19t==11 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0
replace deudaraices=100000000 if d19t==12 & (d18==2) & (d19_01==0|d19_01==.) & (d19_02==0|d19_02==.) & d19t>0


/*Value of the other property for those who have debt (value-deudasraices)*/
***************************************************************************
g bsraices=gross_bsraices
replace bsraices=d17_01-deudaraices if d16==1 & d18==2
replace bsraices=d17_02*17000-deudaraices if d16==1 & d18==2 & d17_01==0 & d17_02>0
replace bsraices=5000000-deudaraices if d17t==1 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace bsraices=15000000-deudaraices if d17t==2 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.)& d17t>0
replace bsraices=25000000-deudaraices if d17t==3 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace bsraices=35000000-deudaraices if d17t==4 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace bsraices=45000000-deudaraices if d17t==5 & (d16==1& d18==2) &(d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.)& d17t>0
replace bsraices=55000000-deudaraices if d17t==6 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.)& d17t>0
replace bsraices=65000000-deudaraices if d17t==7 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace bsraices=75000000-deudaraices if d17t==8 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0
replace bsraices=90000000-deudaraices if d17t==9 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.)& d17t>0
replace bsraices=120000000-deudaraices if d17t==10 & (d16==1& d18==2) & (d17_01==0 | d17_01==.) & (d17_02==0|d17_02==.) & d17t>0

replace bsraices=0 if bsraices<0
ta bsraices, m


rename bsraices bsraices04


***********************************
/*3. 		VALUE OF VEHICLE*/
***********************************

replace d21b_01=0 if d21_01<0
replace d21b_02=0 if d21_02<0
replace d21b_03=0 if d21_03<0
replace d21b_04=0 if d21_04<0

gen autos04=d21b_01 + d21b_02 + d21b_03 + d21b_04


***********************************
/*4. 		VALUE OF SAVINGS*/
***********************************
/*
/*ta d23_0b*/

replace d23m_01=150000000 if d23m_01>150000000 & d23m_01!=.		/*Top coding at 150milions*/
replace d23m_02=150000000 if d23m_02>150000000 & d23m_02!=.
replace d23m_03=150000000 if d23m_03>150000000 & d23m_03!=.
replace d23m_04=150000000 if d23m_04>150000000 & d23m_04!=.
replace d23m_05=150000000 if d23m_05>150000000 & d23m_05!=.
replace d23m_06=150000000 if d23m_06>150000000 & d23m_06!=.
replace d23m_07=150000000 if d23m_07>150000000 & d23m_07!=.
replace d23m_08=150000000 if d23m_08>150000000 & d23m_08!=.
replace d23m_09=150000000 if d23m_09>150000000 & d23m_09!=.
replace d23m_0a=150000000 if d23m_0a>150000000 & d23m_0a!=.
*/

ta d23m_01 d23_01, m

replace d23m_01=0 if d23_01<0
replace d23m_02=0 if d23_02<0
replace d23m_03=0 if d23_03<0
replace d23m_04=0 if d23_04<0
replace d23m_05=0 if d23_05<0
replace d23m_06=0 if d23_06<0
replace d23m_07=0 if d23_07<0
replace d23m_08=0 if d23_08<0
replace d23m_09=0 if d23_09<0
replace d23m_0a=0 if d23_0a<0

gen ahorros04=d23m_01 + d23m_02 + d23m_03 + d23m_04 + d23m_05 + d23m_06 + d23m_07 + d23m_08 + d23m_09 + d23m_0a

ta ahorros, m

***************************
/*5.  	OTHER ASSETS*/
***************************

/*Machinery or equipment*/
*****************************

replace d24b_01=0 if d24a_01==2| d24a_01<0					/*machinery and equipement*/
replace d24b_02=0 if d24a_02==2| d24a_02<0					/*land*/
replace d24b_03=0 if d24a_03==2| d24a_03<0					/*livestock*/
replace d24b_04=0 if d24a_04==2| d24a_04<0					/*other*/
replace d24c_01=0 if d24a_01==2| d24a_01<0					/*machinery and equipement*/
replace d24c_02=0 if d24a_02==2| d24a_02<0					/*land*/
replace d24c_03=0 if d24a_03==2| d24a_03<0					/*livestock*/
replace d24c_04=0 if d24a_04==2| d24a_04<0					/*other*/

gen maquinas04= d24b_01+d24b_02+d24b_03+d24b_04-d24c_01-d24c_02-d24c_03-d24c_04
replace maquinas=0 if maquinas<0

ta maquinas, m

/*Business*/
**************
ta d28_01 d25, m

replace d28_01=0 if d25==2	| d25<0						/*01 is assets*/
replace d28_02=0 if d25==2	| d25<0						/*02 is debt*/

gen negocios04=d28_01 - d28_02

ta negocios, m

*****************
/*6.		DEBTS*/
*****************

ta d30m_01 d30_01,m
replace d30m_01=0 if d30_01<0
replace d30m_02=0 if d30_02<0
replace d30m_03=0 if d30_03<0
replace d30m_04=0 if d30_04<0
replace d30m_05=0 if d30_05<0
replace d30m_06=0 if d30_06<0
replace d30m_07=0 if d30_07<0
replace d30m_08=0 if d30_08<0
replace d30m_09=0 if d30_09<0
replace d30m_0a=0 if d30_0a<0
replace d30m_0b=0 if d30_0b<0

gen deudas04= d30m_01 + d30m_02 + d30m_03 + d30m_04 + d30m_05 + d30m_06 + d30m_07 + d30m_08 + d30m_09 + d30m_0a + d30m_0b

ta deudas04, m


******************************ADDED BY CLEMENT*********************************

g ahorro_liquido04=d23m_01 + d23m_02 + d23m_04 + d23m_05 + d23m_06 + d23m_07 + d23m_08 + d23m_09 + d23m_0a
g apv04=d23m_03

gen patrimonio_liquido04= vivienda04 + bsraices04 + autos04 + ahorro_liquido04 + maquinas04 + negocios04 - deudas04


rename patrimonio_liquido04 wealth04
rename vivienda04 housing04
rename bsraices04 realestate04
rename autos04 cars04
rename maquinas04 equipment04
rename negocios04 business04
rename deudas04 debt04
rename apv04 retirement04
rename ahorro_liquido04 savings04
*keep folio fact_exp patrimonio_liquido04 vivienda04 bsraices04 autos04 ahorro_liquido04 maquinas04 negocios04 deudas04 apv04
keep folio fact_exp wealth04 savings04 housing04 realestate04 cars04 equipment04 business04 debt04 retirement04 

***************************************************************************************
sort folio
save C:\DATA\EPSWealth\wealth04,replace
capture log close



