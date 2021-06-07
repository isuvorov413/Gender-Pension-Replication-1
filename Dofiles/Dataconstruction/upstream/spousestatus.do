version 10
capture log close
set more off

*working directory
cd C:\Users\Clement\Dropbox\Genderequity\dofiles\

**********************************************************************************************
*Chilean Pension System: Labor Data
************************************
*by Clement Joubert
*July 1st 2008
*Revised January 19th 2011

*Object: This do file extracts the age, schooling, labor force participation status and average
*monthly wage of the respondent's spouse

*IN: Requires raw files from the EPS survey (2002, 2004, 2006, 2009) to run
*OUT: generates spousestatus.dta
***********************************************************************************************


clear
set memory 500000

*A. 2009
********

use C:\DATA\EPS09\hogar.dta,clear
****


***************************************************************************************************
*Get spouse's labor status
***************************************************************************************************

*keep only the interviewee's spouse among members of the household

*keep if a5a==1 | a5a ==2                 /*That's if we want only heads and heads spouses*/
keep if a4==2
*drop if spouse is not in the household anymore
drop if a2a==9|a2a==2


*In some cases several spouses are interviewed: in that case check each record to figure out which one was wrongly coded as a spouse (17 such cases)

drop if folio==198563 & orden==3
drop if folio==450344 & orden==2
drop if folio==554948
drop if folio==578016 & orden==4
drop if folio==1047805 & orden==4
drop if folio==1218016 & (orden==3 | orden==4)
drop if folio== 2406704 & orden==5
drop if folio==2407908 & orden==3

*Check for several spouses are interviewed
duplicates report folio

*Generate a variable that contains whether the spouse was working and in what sector

ta c1 c2,m

g sp_inactive = 0
replace sp_inactive=12 if c1==2
replace sp_inactive=12-c4 if c1==1

g sp_formal= 0 
replace sp_formal= c4 if c1==1 & c2==1

g sp_informal=0
replace sp_informal= c4 if c1==1 & (c2==2|c2==3)

replace sp_inactive=. if c1==9|c1==.|c1==8
replace sp_informal=. if c1==9|c1==.|c1==8
replace sp_formal=. if c1==9|c1==.|c1==8

g sp_weeklyhours=c5
replace sp_weeklyhours=. if c5==999|c5==888

g sp_parttime=.
replace sp_parttime=1 if sp_weeklyhours<31
replace sp_parttime=0 if sp_weeklyhours>30 & sp_weeklyhours!=.

**** Get monthly wage ***

*C.3. En su trabajo principal, ¿Cuál fue su ingreso o
*remuneración líquida mensual promedio de los
*últimos 12 meses?
*Anote el monto declarado en pesos
*INCLUYA: Los descuentos por préstamos y
*consumos en casas comerciales.
*EXCLUYA: Las bonificaciones, gratificaciones,
*descuentos previsionales y de salud.


g sp_avwage=c3
replace sp_avwage=. if c3==1000000000

*** sex age health education***

rename a8 sp_sex
replace sp_sex=. if sp_sex==9
rename a9 sp_age
rename a10 sp_health
replace sp_health=. if sp_health==9|sp_healt==8



/*** recode years of education  ***/
*from Petra's sas code ed02insert
g tipo=a12n
g grade=a12c

replace tipo=. if tipo==99|tipo==88
replace grade=. if grade==99|tipo==88

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

rename yrsed sp_yrsed


g sp_edugrp=.
replace a12n=0 if a12n==99
replace sp_edugrp=1 if   a12n==1| a12n==2| a12n==3   |   ((a12n==4|a12n==5|a12n==99)&a12c<8) | ((a12n==6|a12n==8)&a12c<2)| a12c==80
replace sp_edugrp=2 if   (( a12n==4|a12n==5|a12n==99)&a12c>7)     |     ((a12n==6|a12n==8)&a12c<6&a12c>1)    |    ((a12n==7|a12n==9)&a12c<4)
replace sp_edugrp=3 if   ((a12n==6|a12n==8)&a12c>5)    |    ((a12n==7|a12n==9)&a12c>3)   |   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c<4) 
replace sp_edugrp=4 if   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c>3) 
replace sp_edugrp=. if    a12n==.|(a12c==.&a12n!=1)|a12n==99|a12c==99|a12c==88
label define edugrp 1 "No H.S." 2 "H.S. dropout" 3 "H.S. graduate" 4 "College graduate"
label values sp_edugrp edugrp


*Generate a variable with the estimated nb of month of AFP contributions for the spouse (nb month worked*I(contributed in AFP)
*NOTE: I include contributions made when in an informal job
*NOTE: contributions==0 if a37*==.

ta a37 sp_formal if sp_informal==0
ta a37 sp_informal if sp_formal==0
g sp_AFPcont=0
replace sp_AFPcont=sp_formal if a37==1 & sp_formal>0
replace sp_AFPcont=sp_informal if a37==1 & sp_informal>0

g year=2009

replace sp_age=. if sp_age<10
g sp_agegrp=5*int(sp_age/5)

keep folio year orden sp_sex sp_edugrp sp_age sp_agegrp sp_health sp_inactive sp_avwage sp_formal sp_informal sp_health sp_AFPcont sp_yrsed sp_parttime sp_weeklyhours
sort folio


save spousestatus2009.dta, replace



*A. 2006
********

use C:\DATA\EPS06\hogar.dta,clear
****


***************************************************************************************************
*Get spouse's labor status
***************************************************************************************************

*keep only the interviewee's spouse among members of the household

*keep if a5a==1 | a5a ==2                 /*That's if we want only heads and heads spouses*/
keep if a4==2
*drop if spouse is not in the household anymore
drop if a2a==9|a2a==2


*In some cases several spouses are interviewed: in that case check each record to figure out which one was wrongly coded as a spouse (11 such cases)

drop if (folio==528410 & orden==2) | ///
(folio==528410 & orden==5) | ///
(folio==580410 & orden==3) | ///
(folio==736025 & orden==3) | ///
(folio==1071102 & orden==4) | ///
(folio==1212564 & orden==3) | ///
(folio==1352564 & orden==4) | ///
(folio==1866016 & orden==8) | ///
(folio==2112959 & orden==3) | ///
(folio==2412885 & orden==4) | ///
(folio==2195025 & orden==2) | ///
(folio==363871 & orden==3)

*Check for several spouses are interviewed
duplicates report folio

*Generate a variable that contains whether the spouse was working and in what sector

ta c1 c2,m

g sp_inactive = 0
replace sp_inactive=12 if c1==2
replace sp_inactive=12-c4 if c1==1

g sp_formal= 0 
replace sp_formal= c4 if c1==1 & c2==1

g sp_informal=0
replace sp_informal= c4 if c1==1 & (c2==2|c2==3)

replace sp_inactive=. if c1==9|c1==.
replace sp_informal=. if c1==9|c1==.
replace sp_formal=. if c1==9|c1==.

g sp_weeklyhours=c5
replace sp_weeklyhours=. if c5==999

g sp_parttime=.
replace sp_parttime=1 if sp_weeklyhours<31
replace sp_parttime=0 if sp_weeklyhours>30 & sp_weeklyhours!=.

**** Get monthly wage ***

*C.3. En su trabajo principal, ¿Cuál fue su ingreso o
*remuneración líquida mensual promedio de los
*últimos 12 meses?
*Anote el monto declarado en pesos
*INCLUYA: Los descuentos por préstamos y
*consumos en casas comerciales.
*EXCLUYA: Las bonificaciones, gratificaciones,
*descuentos previsionales y de salud.


g sp_avwage=c3
replace sp_avwage=. if c3==1000000000

*** sex age health education***

rename a8 sp_sex
replace sp_sex=. if sp_sex==9
rename a9 sp_age
rename a10 sp_health
replace sp_health=. if sp_health==9



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

rename yrsed sp_yrsed


g sp_edugrp=.
replace a12n=0 if a12n==99
replace sp_edugrp=1 if   a12n==1| a12n==2| a12n==3   |   ((a12n==4|a12n==5|a12n==99)&a12c<8) | ((a12n==6|a12n==8)&a12c<2)
replace sp_edugrp=2 if   (( a12n==4|a12n==5|a12n==99)&a12c>7)     |     ((a12n==6|a12n==8)&a12c<6&a12c>1)    |    ((a12n==7|a12n==9)&a12c<4)
replace sp_edugrp=3 if   ((a12n==6|a12n==8)&a12c>5)    |    ((a12n==7|a12n==9)&a12c>3)   |   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c<4) 
replace sp_edugrp=4 if   ((a12n==10|a12n==11|a12n==12|a12n==13)&a12c>3) 
replace sp_edugrp=. if    a12n==.|(a12c==.&a12n!=1)|a12n==99|a12c==99
label define edugrp 1 "No H.S." 2 "H.S. dropout" 3 "H.S. graduate" 4 "College graduate"
label values sp_edugrp edugrp


*Generate a variable with the estimated nb of month of AFP contributions for the spouse (nb month worked*I(contributed in AFP)
*NOTE: I include contributions made when in an informal job
*NOTE: contributions==0 if a37*==.

ta a37a sp_formal if sp_informal==0
ta a37a sp_informal if sp_formal==0
g sp_AFPcont=0
replace sp_AFPcont=sp_formal if a37a==1 & a37b==1 & sp_formal>0
replace sp_AFPcont=sp_informal if a37a==1 & a37b==1 & sp_informal>0

g year=2006

replace sp_age=. if sp_age<10
g sp_agegrp=5*int(sp_age/5)

keep folio year orden sp_sex sp_edugrp sp_age sp_agegrp sp_health sp_inactive sp_avwage sp_formal sp_informal sp_health sp_AFPcont sp_yrsed sp_parttime sp_weeklyhours
sort folio


save spousestatus2006.dta, replace



*B. 2004
********

use C:\DATA\EPS04\hogar.dta,clear
****


***************************************************************************************************
*Get spouse's labor status
***************************************************************************************************

*keep only the interviewee's spouse among members of the household
ta a4a a4b

*keep if a5a==1 | a5a ==2                 /*That's if we want only heads and heads spouses*/
keep if a4a==2
*drop spouses that are not in the household anymore
drop if a2==-4|a2==2

*Check for several spouses are interviewed
duplicates report folio

*Generate a variable that contains whether the spouse was working and in what sector

ta c1 c2,m

g sp_inactive = 0
replace sp_inactive=12 if c1==2
replace sp_inactive=12-c4 if c1==1
replace sp_inactive=. if c4==-4
replace sp_inactive=. if c1==.


g sp_formal= 0 
replace sp_formal= c4 if c1==1 & c2==1
replace sp_formal=. if c4==-4
replace sp_formal=. if c1==.


g sp_informal=0
replace sp_informal= c4 if c1==1 & (c2==2|c2==3|c2==-4)
replace sp_informal=. if c4==-4
replace sp_informal=. if c1==.


g sp_weeklyhours=c5
replace sp_weeklyhours=. if c5==-4

g sp_parttime=.
replace sp_parttime=1 if sp_weeklyhours<31
replace sp_parttime=0 if sp_weeklyhours>30 & sp_weeklyhours!=.


***wage***
*Use: C.3. En su trabajo principal, ¿Cuál fue su ingreso o remuneración líquida mensual promedio de los últimos 12 meses?

g sp_avwage=c3
replace sp_avwage=. if c3==-4


***sex age and health****

rename a5 sp_sex
rename a6 sp_age
rename a8 sp_health
replace sp_health=. if sp_health==-4


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

rename yrsed sp_yrsed

g sp_edugrp=.
replace a10n=0 if a10n==-4
replace sp_edugrp=1 if   a10n==1| a10n==2| a10n==3   |   ((a10n==4|a10n==5|a10n==99)&a10c<8) | ((a10n==6|a10n==8)&a10c<2)
replace sp_edugrp=2 if   (( a10n==4|a10n==5|a10n==99)&a10c>7)     |     ((a10n==6|a10n==8)&a10c<6&a10c>1)    |    ((a10n==7|a10n==9)&a10c<4)
replace sp_edugrp=3 if   ((a10n==6|a10n==8)&a10c>5)    |    ((a10n==7|a10n==9)&a10c>3)   |   ((a10n==10|a10n==11|a10n==12|a10n==13)&a10c<4) 
replace sp_edugrp=4 if   ((a10n==10|a10n==11|a10n==12|a10n==13)&a10c>3) 
replace sp_edugrp=. if    a10n==.|(a10c==.&a10n!=1)|a10n==-4|a10c==-4

label values sp_edugrp edugrp


*** contributions***

*Generate a variable with the estimated nb of month of AFP contributions for the spouse (nb month worked*I(contributed in AFP)
*NOTE: I include contributions made when in an informal job

ta a37 sp_formal if sp_informal==0
ta a37b sp_informal if sp_formal==0
g sp_AFPcont=0
replace sp_AFPcont=sp_formal if a37==1 & a37b==1 & sp_formal>0
replace sp_AFPcont=sp_informal if a37==1 & a37b==1 & sp_informal>0

g year=2004

replace sp_age=. if sp_age<10
g sp_agegrp=5*int(sp_age/5)

keep folio year orden sp_sex sp_edugrp sp_age sp_agegrp sp_health sp_inactive sp_formal sp_informal sp_AFPcont sp_avwage sp_yrsed sp_parttime sp_weeklyhours
sort folio


save spousestatus2004.dta, replace


*C. 2002
********

use C:\DATA\EPS02\Base2.dta,clear
****


***************************************************************************************************
*Get spouse's labor status
***************************************************************************************************

*keep only the interviewee's spouse among members of the household
ta ip2

keep if ip2==2

*Check for several spouses are interviewed
duplicates report folio

*Generate a variable that contains whether the spouse was working and in what sector (note: this is an extrapolation from the
*questions that ask whether the spouse had a job (time frame not specified) or not

g sp_inactive = 0
replace sp_inactive=12 if iip1a==2 & iip1b==2
replace sp_inactive=. if iip1a==.


g sp_formal= 0 
replace sp_formal= 12 if (iip1a==1 | (iip1a==2 & iip1b==1)) & (iip12==1|iip12==2|iip12==3)
replace sp_formal=. if iip1a==.

*Assume that respondents who don't know whether they have a contract or don't answer the question don't have one
g sp_informal=0
replace sp_informal= 12 if (iip1a==1 | (iip1a==2 & iip1b==1)) & (iip12==4|iip12==5|iip12==.)
replace sp_informal=. if iip1a==.


g sp_weeklyhours=iip19
replace sp_weeklyhours=. if iip19==999

g sp_parttime=.
replace sp_parttime=1 if sp_weeklyhours<31
replace sp_parttime=0 if sp_weeklyhours>30 & sp_weeklyhours!=.

***wage***

g sp_avwage=iip18

***sex age and health****

rename ip4 sp_sex
rename ip5 sp_age
g sp_health=.    


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

rename yrsed sp_yrsed

g sp_edugrp=.
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

replace sp_edugrp=1 if   ip9tbis==1| ip9tbis==2 | ip9tbis==3   |   ((ip9tbis==4|ip9tbis==5|ip9tbis==17)&ip9c<8) | ((ip9tbis==6|ip9tbis==8)&ip9c<2)
replace sp_edugrp=2 if   (( ip9tbis==4|ip9tbis==5|ip9tbis==99)&ip9c>7)     |     ((ip9tbis==6|ip9tbis==8)&ip9c<6&ip9c>1)    |    ((ip9tbis==7|ip9tbis==9)&ip9c<4)
replace sp_edugrp=3 if   ((ip9tbis==6|ip9tbis==8)&ip9c>5)    |    ((ip9tbis==7|ip9tbis==9)&ip9c>3)   |   ((ip9tbis==10|ip9tbis==11|ip9tbis==12|ip9tbis==13)&ip9c<4) 
replace sp_edugrp=4 if   ((ip9tbis==10|ip9tbis==11|ip9tbis==12|ip9tbis==13)&ip9c>3)
replace sp_edugrp=. if    ip9tbis==.|(ip9c==. & ip9tbis!=1)|ip9tbis==-4|ip9c==-4

label values sp_edugrp edugrp

g sp_AFPcont=.
g year=2002
replace sp_age=. if sp_age==999|sp_age<10
g sp_agegrp=5*int(sp_age/5)
keep folio year orden sp_sex sp_edugrp sp_yrsed sp_age sp_agegrp sp_health sp_inactive sp_formal sp_informal sp_AFPcont sp_avwage sp_yrsed sp_parttime sp_weeklyhours
sort folio

save spousestatus2002.dta, replace
sort folio


*Merge all survey rounds

append using spousestatus2004.dta
append using spousestatus2006.dta
append using spousestatus2009.dta

g sp_dum=1

sort folio year
save spousestatus.dta, replace

