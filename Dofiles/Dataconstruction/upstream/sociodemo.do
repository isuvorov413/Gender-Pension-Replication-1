

version 10
capture log close
set more off

*working directory
cd C:\Users\Clement\Dropbox\Genderequity\dofiles\

**********************************************************************************************
*Chilean Pension System: Socio-demographics
*******************************************
*by Clement Joubert
*December 06th 2008

*OBJECT: 
*********

*Creates "sociodemo.dta" with variables age, sex, education region household size, civilstatus, health


*DATASETS:
**********

*IN: C:\DATA\EPS06\entrevistado.dta
*IN: C:\DATA\EPS04\entrevistado.dta

*OUT: C:\DATA\jointlaborstatus\masterdata\sociodemo.dta


***********************************************************************************************
clear
set memory 500000


*2. Socio-demo from EPS02
******************************
use C:\DATA\EPS02\base1.dta, clear
save nbspouses02, replace

rename vip2 nbspouses02
g health02 = vip25+1 /*note: no "excellent" answer available: consistency problem!!"*/
replace health02=. if health02==7
keep folio nbspouses02 health02
sort folio

save temp, replace

use C:\DATA\EPS02\base2.dta, clear
****


save sociodemoEPS02, replace
keep if orden==1
sort folio
merge folio using temp

rename marcajh kinship02
rename ip5 age02
rename ip4 sex02
rename region region02
rename ip8 student02
rename ip6 civilstatus02

label drop _all

replace nbspouses02=. if nbspouses02==-4
replace health02=. if health02==-4
replace civilstatus02=. if civilstatus02==-4
replace region02=. if region02==9


/*** recode years of education  ***/
*from Petra's sas code ed02insert
g tipo=ip9t
g grade=ip9c

g yrsed=.
replace yrsed=0 if (tipo == 16 | tipo == 1)

/* old system */
replace grade=6 if grade>6 & grade !=. & (tipo==2|5)
replace yrsed=grade if tipo==2
replace yrsed=grade+6 if tipo==5
replace yrsed=grade+7 if tipo==7 /* could be 6 | 8 , so we give 7 */

/* new system */  
replace yrsed=grade	 if tipo==3
replace yrsed=8 if yrsed>=9 & yrsed!=. & tipo==3

replace grade=4 if grade>4  & grade !=.& tipo==6
replace yrsed=grade+8 if tipo==6| tipo==8
replace yrsed=12 if yrsed>=13 & yrsed!=. & tipo==8

/* postsecondary */
replace grade=5 if tipo==12 & grade>5 & grade !=.
replace grade=6 if tipo==13 & grade>6 & grade !=.
replace grade=7 if tipo==14 & grade>7 & grade !=.

replace yrsed=grade+12 if (tipo == 9 | tipo == 10 | tipo == 11 | tipo == 12 | tipo == 13 | tipo == 14)
replace yrsed=17 if tipo == 14 & yrsed > 17 & yrsed!=. 
replace yrsed=16 if tipo == 13 & yrsed > 16 & yrsed!=. 
replace grade = grade - 4 if tipo == 15   /* recoding because noone answers less than 5 */
replace yrsed = grade + 17 if tipo == 15 

/* special ed */
replace yrsed=grade if tipo == 4
replace yrsed=. if tipo == 17  /* there should be no tipo beyond 16 */
replace yrsed=. if grade < 0 | tipo < 0

rename yrsed yrsed02

/*Create four education groups: No highschool, Some highschool, highschool grad, college grad */

g edugrp02=.
replace ip9c=0 if ip9c==9
g ip9tbis=ip9t
replace ip9tbis=-4 if ip9t==17
replace ip9tbis=1 if ip9t==1 |ip9t==16
replace ip9tbis=3 if ip9t==2
replace ip9tbis=4 if ip9t==3
replace ip9tbis=5 if ip9t==4
replace ip9tbis=6 if ip9t==5
replace ip9tbis=7 if ip9t==6
replace ip9tbis=8 if ip9t==7
replace ip9tbis=9 if ip9t==8
replace ip9tbis=10 if ip9t==9 | ip9t==10
replace ip9tbis=11 if ip9t==11 | ip9t==12
replace ip9tbis=12 if ip9t==13 | ip9t ==14
replace ip9tbis=13 if ip9t==15

replace edugrp02=1 if   ip9tbis==1| ip9tbis==2 | ip9tbis==3   |   ((ip9tbis==4|ip9tbis==5|ip9tbis==17)&ip9c<8) | ((ip9tbis==6|ip9tbis==8)&ip9c<2)
replace edugrp02=2 if   (( ip9tbis==4|ip9tbis==5|ip9tbis==99)&ip9c>7)     |     ((ip9tbis==6|ip9tbis==8)&ip9c<6&ip9c>1)    |    ((ip9tbis==7|ip9tbis==9)&ip9c<4)
replace edugrp02=3 if   ((ip9tbis==6|ip9tbis==8)&ip9c>5)    |    ((ip9tbis==7|ip9tbis==9)&ip9c>3)   |   ((ip9tbis==10|ip9tbis==11|ip9tbis==12|ip9tbis==13)&ip9c<4) 
replace edugrp02=4 if   ((ip9tbis==10|ip9tbis==11|ip9tbis==12|ip9tbis==13)&ip9c>3)
replace edugrp02=. if    ip9tbis==.|(ip9c==. & ip9tbis!=1)|ip9tbis==-4|ip9c==-4

label values edugrp02 edugrp

g agegrp02 = int(age02/5)*5

sort folio
keep folio age02 sex02 kinship02 region02 health02 student02 civilstatus02 nbspouses02 agegrp02 edugrp02 yrsed02

save sociodemoEPS02, replace


*2. Socio-demo from EPS04
******************************

use C:\DATA\EPS04\entrevistado.dta, clear
****
rename a6 age04
rename a4b kinship04
rename a5 sex04
rename region region04
rename a8 health04
rename a9 student04
rename a7 civilstatus04
rename i4 nbspouses04

label drop _all

replace nbspouses04=. if nbspouses04==-4
replace health04=. if health04==-4
replace civilstatus04=. if civilstatus04==-4
replace region04=. if region04==9

g agegrp04 = int(age04/5)*5


/*** recode years of education  ***/
*from Petra's sas code ed02insert
g tipo=a10n
g grade=a10c

g yrsed=.
replace yrsed=0 if (tipo == 16 | tipo == 1)

/* old system */
replace grade=6 if grade>6 & grade !=. & (tipo==2|5)
replace yrsed=grade if tipo==2
replace yrsed=grade+6 if tipo==5
replace yrsed=grade+7 if tipo==7 /* could be 6 | 8 , so we give 7 */

/* new system */  
replace yrsed=grade	 if tipo==3
replace yrsed=8 if yrsed>=9 & yrsed!=. & tipo==3

replace grade=4 if grade>4 & grade !=. & tipo==6
replace yrsed=grade+8 if tipo==6| tipo==8
replace yrsed=12 if yrsed>=13 & yrsed!=. & tipo==8

/* postsecondary */
replace grade=5 if tipo==12 & grade>5 & grade !=.
replace grade=6 if tipo==13 & grade>6 & grade !=.
replace grade=7 if tipo==14 & grade>7 & grade !=.

replace yrsed=grade+12 if (tipo == 9 | tipo == 10 | tipo == 11 | tipo == 12 | tipo == 13 | tipo == 14)
replace yrsed=17 if tipo == 14 & yrsed > 17 & yrsed!=. 
replace yrsed=16 if tipo == 13 & yrsed > 16 & yrsed!=. 
replace grade = grade - 4 if tipo == 15   /* recoding because noone answers less than 5 */
replace yrsed = grade + 17 if tipo == 15 

/* special ed */
replace yrsed=grade if tipo == 4
replace yrsed=. if tipo == 17  /* there should be no tipo beyond 16 */
replace yrsed=. if grade < 0 | tipo < 0

rename yrsed yrsed04

***Create four education groups: No highschool, Some highschool, highschool grad, college grad ****
replace a10n=. if a10n==-4 & a10c==-4
replace a10c=. if a10c==-4 & a10n==-4

g edugrp04=.
replace edugrp04=1 if   a10n==1| a10n==2| a10n==3   |   ((a10n==4|a10n==5|a10n==99)&a10c<8) | ((a10n==6|a10n==8)&a10c<2)
replace edugrp04=2 if   (( a10n==4|a10n==5|a10n==99)&a10c>7)     |     ((a10n==6|a10n==8)&a10c<6&a10c>1)    |    ((a10n==7|a10n==9)&a10c<4)
replace edugrp04=3 if   ((a10n==6|a10n==8)&a10c>5)    |    ((a10n==7|a10n==9)&a10c>3)   |   ((a10n==10|a10n==11|a10n==12|a10n==13)&a10c<4) 
replace edugrp04=4 if   ((a10n==10|a10n==11|a10n==12|a10n==13)&a10c>3) 
replace edugrp04=. if    a10n==.|(a10c==. & a10n!=1)|a10n==-4|a10c==-4

sort folio
keep folio age04  kinship04 sex04 region04 health04 student04 civilstatus04 nbspouses04 agegrp04 edugrp04 yrsed04


save sociodemoEPS04, replace

*3. Socio-demographics from EPS06
*********************************

use C:\DATA\EPS06\entrevistado.dta, clear
****

replace i1=. if i1==9

rename a5 kinship06
rename a9 age06
rename a8 sex06
rename region region06
rename a10 health06
rename a11 student06
rename i1 civilstatus06
rename i2a civilstatuschange06
rename i2b nbspouses06
rename tipomues in04EPS06

replace in04EPS06=1 if folio==179
replace nbspouses06=0 if nbspouses06==.
replace health06=. if health06==9
replace region06=. if region06==9

g agegrp06 = int(age06/5)*5


/*** recode years of education  ***/
*from Petra's sas code ed02insert
g tipo=a12n
g grade=a12c

replace tipo=. if tipo==99
replace grade=. if grade==99

g yrsed=.
replace yrsed=0 if (tipo == 16 | tipo == 1)

/* old system */
replace grade=6 if grade>6 & grade!=. & (tipo==2|5)
replace yrsed=grade if tipo==2
replace yrsed=grade+6 if tipo==5
replace yrsed=grade+7 if tipo==7 /* could be 6 | 8 , so we give 7 */

/* new system */  
replace yrsed=grade	 if tipo==3
replace yrsed=8 if yrsed>=9 & yrsed!=. & tipo==3

replace grade=4 if grade>4 & grade !=. & tipo==6
replace yrsed=grade+8 if tipo==6| tipo==8
replace yrsed=12 if yrsed>=13 & yrsed!=. & tipo==8

/* postsecondary */
replace grade=5 if tipo==12 & grade>5 & grade!=.
replace grade=6 if tipo==13 & grade>6 & grade!=.
replace grade=7 if tipo==14 & grade>7 & grade!=.

replace yrsed=grade+12 if (tipo == 9 | tipo == 10 | tipo == 11 | tipo == 12 | tipo == 13 | tipo == 14)
replace yrsed=17 if tipo == 14 & yrsed > 17 & yrsed!=.
replace yrsed=16 if tipo == 13 & yrsed > 16 & yrsed!=.
replace grade = grade - 4 if tipo == 15   /* recoding because noone answers less than 5 */
replace yrsed = grade + 17 if tipo == 15 

/* special ed */
replace yrsed=grade if tipo == 4
replace yrsed=. if tipo == 17  /* there should be no tipo beyond 16 */
replace yrsed=. if grade == 99 | tipo == 99

rename yrsed yrsed06


***Create four education groups: No highschool, Some highschool, highschool grad, college grad ****

g edugrp06=.

replace edugrp06=1 if   a12n==1|a12n==2| a12n==3  |   ((a12n==4|a12n==5)&a12c<8) | ((a12n==6|a12n==8)&a12c<2)
replace edugrp06=2 if   (( a12n==4|a12n==5)&a12c>7)     |     ((a12n==6|a12n==8)&a12c<6&a12c>1)    |    ((a12n==7|a12n==9)&a12c<4)
replace edugrp06=3 if   ((a12n==6|a12n==8)&a12c>5)    |    ((a12n==7|a12n==9)&a12c>3)   |   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c<4) 
replace edugrp06=4 if   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c>3) 
replace edugrp06=. if    a12n==.|(a12c==.& a12n!=1)|a12n==99|a12c==99

sort folio
keep folio student06 kinship06 civilstatus06 health06 region06 sex06 civilstatuschange06 nbspouses06 age06 agegrp06 edugrp06 in04EPS06 yrsed06

save sociodemoEPS06, replace

*4. Socio-demographics from EPS09
*********************************

use C:\DATA\EPS09\entrevistado.dta, clear
****

rename a5 kinship09
rename a9 age09
rename a8 sex09
rename a6a region09
rename a10 health09
rename a11 student09

replace health09=. if health09==9|health09==8 
replace student09=. if student09==8
replace region09=. if region09==99|region09==88

g agegrp09 = int(age09/5)*5


/*** recode years of education  ***/
*from Petra's sas code ed02insert
g tipo=a12n
g grade=a12c

replace tipo=. if tipo==99 | tipo==88
replace grade=. if grade==99 | grade==88

g yrsed=.
replace yrsed=0 if (tipo == 16 | tipo == 1)

/* old system */
replace grade=6 if grade>6 & grade!=. & (tipo==2|5)
replace yrsed=grade if tipo==2
replace yrsed=grade+6 if tipo==5
replace yrsed=grade+7 if tipo==7 /* could be 6 | 8 , so we give 7 */

/* new system */  
replace yrsed=grade	 if tipo==3
replace yrsed=8 if yrsed>=9 & yrsed!=. & tipo==3

replace grade=4 if grade>4 & grade !=. & tipo==6
replace yrsed=grade+8 if tipo==6| tipo==8
replace yrsed=12 if yrsed>=13 & yrsed!=. & tipo==8

/* postsecondary */
replace grade=5 if tipo==12 & grade>5 & grade!=.
replace grade=6 if tipo==13 & grade>6 & grade!=.
replace grade=7 if tipo==14 & grade>7 & grade!=.

replace yrsed=grade+12 if (tipo == 9 | tipo == 10 | tipo == 11 | tipo == 12 | tipo == 13 | tipo == 14)
replace yrsed=17 if tipo == 14 & yrsed > 17 & yrsed!=.
replace yrsed=16 if tipo == 13 & yrsed > 16 & yrsed!=.
replace grade = grade - 4 if tipo == 15   /* recoding because noone answers less than 5 */
replace yrsed = grade + 17 if tipo == 15 

/* special ed */
replace yrsed=grade if tipo == 4
replace yrsed=. if tipo == 17  /* there should be no tipo beyond 16 */
replace yrsed=. if grade == 99 | tipo == 99

rename yrsed yrsed09




***Create four education groups: No highschool, Some highschool, highschool grad, college grad ****

g edugrp09=.

replace edugrp09=1 if   a12n==1|a12n==2| a12n==3  |   ((a12n==4|a12n==5)&a12c<8) | ((a12n==6|a12n==8)&a12c<2)
replace edugrp09=2 if   (( a12n==4|a12n==5)&a12c>7)     |     ((a12n==6|a12n==8)&a12c<6&a12c>1)    |    ((a12n==7|a12n==9)&a12c<4)
replace edugrp09=3 if   ((a12n==6|a12n==8)&a12c>5)    |    ((a12n==7|a12n==9)&a12c>3)   |   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c<4) 
replace edugrp09=4 if   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c>3) 
replace edugrp09=. if    a12n==.|(a12c==.& a12n!=1)|a12n==99|a12c==99|a12n==88|a12c==88

sort folio
keep folio student09 kinship09  health09 region09 sex09 age09 agegrp09 edugrp09 yrsed09

save sociodemoEPS09, replace

use  C:\DATA\EPS09\hindividual.dta, clear

keep if orden==1

rename i1 civilstatus09
rename i2_1 civilstatuschange09
rename i2_2 nbspouses09

replace civilstatus09=. if civilstatus09==88
replace nbspouses09=0 if nbspouses09==.

sort folio
keep folio civilstatus09 civilstatuschange09 nbspouses09

merge folio using sociodemoEPS09
drop _merge

sort folio
save sociodemoEPS09, replace

*Merge information from different survey rounds
************************************************

merge folio using sociodemoEPS06

ta _merge
drop _merge

sort folio
merge folio using sociodemoEPS04

ta _merge
drop _merge

sort folio
merge folio using sociodemoEPS02

ta _merge
drop _merge

g nbspousesever02=.
g nbspousesever04=.
g nbspousesever06=.
g nbspousesever09=.

replace nbspousesever02=nbspouses02
replace nbspousesever04=nbspouses04
replace nbspousesever06=nbspouses04+nbspouses06 if in04EPS06==1
replace nbspousesever06=nbspouses06 if in04EPS06==2
replace nbspousesever09=nbspousesever06+nbspouses09

drop nbspouses02 nbspouses06 nbspouses04 nbspouses09

renvars _all, subst(02 2002)
renvars _all, subst(06 2006)
renvars _all, subst(04 2004)
renvars _all, subst(09 2009)

reshape long age kinship sex region health student civilstatus nbspousesever agegrp edugrp yrsed in2004EPS civilstatuschange, i(folio) j(year)


*create dummy for head of household
*************************************

g head=(kinship==1)

*Label variables
****************

label define edugrp 1 "No H.S." 2 "H.S. dropout" 3 "H.S. graduate" 4 "College graduate"
label values edugrp edugrp
label define agegrp 15 "15-19"20"20-24" 25"25-29"30"30-34" 35"35-39" 40"40-44" 45"45-49"50"50-54" 55"55-59" 60"60-64"65"65-69"70"70-74" 75"75-79" ///
80"80-84" 85"85-89" 90"90-94" 95"95-99" 100"100-105"
label values agegrp agegrp

save sociodemo.dta,replace
