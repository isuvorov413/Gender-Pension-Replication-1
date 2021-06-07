
foreach round in 04 06 09 {        
	replace married=married`round' if year==20`round'
}
replace married=married04 if year==2005			// XXX keep marital status of previous survey round for intermediate years
replace married=married06 if year==2007 | year==2008




g H_balance=.
g W_balance=.

replace H_balance=baltot if sexsampled==1
replace W_balance=baltot if sexsampled==2
/*unaffiliated individuals get a balance of 0*/
replace H_balance=0 if H_balance==. & sexsampled==1 & year>2002 & year <2006 & inanDAbal==1 
replace W_balance=0 if W_balance==. & sexsampled==2 & year>2002 & year <2006 & inanDAbal==1


replace bono2004=0 if bono2004==.
replace H_balance=H_balance+bono2004 if sexsampled==1 
replace W_balance=W_balance+bono2004 if sexsampled==2


replace H_balance=-99 if sexsampled==2
replace H_balance=-99 if year<2003|year>2005
replace W_balance=-99 if sexsampled==1
replace W_balance=-99 if year<2003|year>2005


g H_working= 1-H_inactive_dum
g W_working=1-W_inactive_dum
replace W_working=2 if W_XPincr==0.5 
g H_formal_lag=H_formal_dum[_n-1]
g W_formal_lag=W_formal_dum[_n-1]
g H_working_lag=H_working[_n-1]
g W_working_lag=W_working[_n-1]
drop W_Xformal
drop W_Xinformal
rename H_Xtotal H_XP
rename W_XPpt W_XP
rename W_XPpt_F W_Xformal
rename W_XPpt_I W_Xinformal



g singlespl=(married04==0)
recode H_agelastmarriage (999=.) 
recode W_agelastmarriage (999=.)

g marriageduration=.
replace marriageduration = H_age-H_agelastmarriage if sexsampled==1 & singlespl==0
replace marriageduration = W_age-W_agelastmarriage if sexsampled==2 & singlespl==0
g sex=.
replace sex=sexsampled if singlespl==1

g female=(sexsampled==2)


replace married=1 if inEPS06==1 & inEPS04==0 & married==. & year==2004 & (H_age>i5_1  & year<=i8_1 & i9_1==1 & sexsampled==1)|(W_age>i5_1  & i9_1==1 & year<=i8_1 & sexsampled==2)
replace married=1 if inEPS06==1 & inEPS04==0 & married==. & year==2004 & (H_age>i5_2  & year<=i8_2 & i9_1==2 & sexsampled==1)|(W_age>i5_2  & i9_1==2 & year<=i8_2 & sexsampled==2)
replace married=1 if inEPS06==1 & inEPS04==0 & married==. & year==2004 & (H_age>i5_3  & year<=i8_3 & i9_1==3 & sexsampled==1)|(W_age>i5_3  & i9_1==3 & year<=i8_3 & sexsampled==2)
replace married=1 if inEPS06==1 & inEPS04==0 & married==. & year==2004 & (H_age>i5_4  & year<=i8_4 & i9_1==4 & sexsampled==1)|(W_age>i5_4  & i9_1==4 & year<=i8_4 & sexsampled==2)
replace married=0 if inEPS06==1 & inEPS04==0 & married==. & year==2004 

replace marriageduration=W_age-i5_1   if inEPS06==1 & inEPS04==0 & marriageduration==.   & year==2004 & W_age>i5_1  & i9_1==1 & year<=i8_1 
replace marriageduration=W_age-i5_2   if inEPS06==1 & inEPS04==0 & marriageduration==.   & year==2004 & W_age>i5_2  & i9_2==1 & year<=i8_2 
replace marriageduration=W_age-i5_3   if inEPS06==1 & inEPS04==0 & marriageduration==.   & year==2004 & W_age>i5_3  & i9_3==1 & year<=i8_3 
replace marriageduration=W_age-i5_4   if inEPS06==1 & inEPS04==0 & marriageduration==.   & year==2004 & W_age>i5_4  & i9_4==1 & year<=i8_4 
replace marriageduration=-99 if inEPS06==1 & inEPS04==0 & married==0 & year==2004 

capture drop tipo
capture drop grade
g tipo=.
g grade=.

replace tipo=i12_1 if inEPS06==1 & inEPS04==0 & tipo==. & year==2004 & (H_age>i5_1  & year<=i8_1 & i9_1==1 & sexsampled==1)|(W_age>i5_1  & i9_1==1 & year<=i8_1 & sexsampled==2)
replace tipo=i12_2 if inEPS06==1 & inEPS04==0 & tipo==. & year==2004 & (H_age>i5_2  & year<=i8_2 & i9_1==2 & sexsampled==1)|(W_age>i5_2  & i9_1==2 & year<=i8_2 & sexsampled==2)
replace tipo=i12_3 if inEPS06==1 & inEPS04==0 & tipo==. & year==2004 & (H_age>i5_3  & year<=i8_3 & i9_1==3 & sexsampled==1)|(W_age>i5_3  & i9_1==3 & year<=i8_3 & sexsampled==2)
replace tipo=i12_4 if inEPS06==1 & inEPS04==0 & tipo==. & year==2004 & (H_age>i5_4  & year<=i8_4 & i9_1==4 & sexsampled==1)|(W_age>i5_4  & i9_1==4 & year<=i8_4 & sexsampled==2)

replace grade=i13_1 if inEPS06==1 & inEPS04==0 & grade==. & year==2004 & (H_age>i5_1  & year<=i8_1 & i9_1==1 & sexsampled==1)|(W_age>i5_1  & i9_1==1 & year<=i8_1 & sexsampled==2)
replace grade=i13_2 if inEPS06==1 & inEPS04==0 & grade==. & year==2004 & (H_age>i5_2  & year<=i8_2 & i9_1==2 & sexsampled==1)|(W_age>i5_2  & i9_1==2 & year<=i8_2 & sexsampled==2)
replace grade=i13_3 if inEPS06==1 & inEPS04==0 & grade==. & year==2004 & (H_age>i5_3  & year<=i8_3 & i9_1==3 & sexsampled==1)|(W_age>i5_3  & i9_1==3 & year<=i8_3 & sexsampled==2)
replace grade=i13_4 if inEPS06==1 & inEPS04==0 & grade==. & year==2004 & (H_age>i5_4  & year<=i8_4 & i9_1==4 & sexsampled==1)|(W_age>i5_4  & i9_1==4 & year<=i8_4 & sexsampled==2)

replace tipo=. if tipo==99
replace grade=. if grade==99

drop yrsed sp_yrsed
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


replace H_yrsed=sp_yrsed   if inEPS06==1 & inEPS04==0 & H_yrsed==.   & year==2004 & sexsampled==2 & W_age>i5_1  & i9_1==1 & year<=i8_1 
replace H_yrsed=sp_yrsed   if inEPS06==1 & inEPS04==0 & H_yrsed==.   & year==2004 & sexsampled==2 & W_age>i5_2  & i9_2==1 & year<=i8_2 
replace H_yrsed=sp_yrsed   if inEPS06==1 & inEPS04==0 & H_yrsed==.   & year==2004 & sexsampled==2 & W_age>i5_3  & i9_3==1 & year<=i8_3 
replace H_yrsed=sp_yrsed   if inEPS06==1 & inEPS04==0 & H_yrsed==.   & year==2004 & sexsampled==2 & W_age>i5_4  & i9_4==1 & year<=i8_4 
replace H_yrsed=-99 	   if inEPS06==1 & inEPS04==0 & married==0   & year==2004 & sexsampled==2 

replace W_yrsed=sp_yrsed  if inEPS06==1 & inEPS04==0 & W_yrsed==.   & year==2004 & sexsampled==1 & H_age>i5_1  & i9_1==1 & year<=i8_1 
replace W_yrsed=sp_yrsed  if inEPS06==1 & inEPS04==0 & W_yrsed==.   & year==2004 & sexsampled==1 & H_age>i5_2  & i9_2==1 & year<=i8_2 
replace W_yrsed=sp_yrsed  if inEPS06==1 & inEPS04==0 & W_yrsed==.   & year==2004 & sexsampled==1 & H_age>i5_3  & i9_3==1 & year<=i8_3 
replace W_yrsed=sp_yrsed  if inEPS06==1 & inEPS04==0 & W_yrsed==.   & year==2004 & sexsampled==1 & H_age>i5_4  & i9_4==1 & year<=i8_4 
replace W_yrsed=-99 	  if inEPS06==1 & inEPS04==0 & married==0   & year==2004 & sexsampled==1


replace H_age=W_age+i6_1-i5_1   if inEPS06==1 & inEPS04==0 & H_age==.   & year==2004 & sexsampled==2 & W_age>i5_1  & i9_1==1 & year<=i8_1 
replace H_age=W_age+i6_2-i5_2   if inEPS06==1 & inEPS04==0 & H_age==.   & year==2004 & sexsampled==2 & W_age>i5_2  & i9_2==1 & year<=i8_2 
replace H_age=W_age+i6_3-i5_3   if inEPS06==1 & inEPS04==0 & H_age==.   & year==2004 & sexsampled==2 & W_age>i5_3  & i9_3==1 & year<=i8_3 
replace H_age=W_age+i6_4-i5_4   if inEPS06==1 & inEPS04==0 & H_age==.   & year==2004 & sexsampled==2 & W_age>i5_4  & i9_4==1 & year<=i8_4 
replace H_age=-99 				if inEPS06==1 & inEPS04==0 & married==0 & year==2004 & sexsampled==2 

replace W_age=H_age+i6_1-i5_1   if inEPS06==1 & inEPS04==0 & W_age==.   & year==2004 & sexsampled==1 & H_age>i5_1  & i9_1==1 & year<=i8_1 
replace W_age=H_age+i6_2-i5_2   if inEPS06==1 & inEPS04==0 & W_age==.   & year==2004 & sexsampled==1 & H_age>i5_2  & i9_2==1 & year<=i8_2 
replace W_age=H_age+i6_3-i5_3   if inEPS06==1 & inEPS04==0 & W_age==.   & year==2004 & sexsampled==1 & H_age>i5_3  & i9_3==1 & year<=i8_3 
replace W_age=H_age+i6_4-i5_4   if inEPS06==1 & inEPS04==0 & W_age==.   & year==2004 & sexsampled==1 & H_age>i5_4  & i9_4==1 & year<=i8_4 
replace W_age=-99 				if inEPS06==1 & inEPS04==0 & married==0 & year==2004 & sexsampled==1

replace H_working=(i11_1==1)   if inEPS06==1 & inEPS04==0 & H_working==.   & year==2004 & sexsampled==2 & W_age>i5_1  & i9_1==1 & year<=i8_1 
replace H_working=(i11_2==1)   if inEPS06==1 & inEPS04==0 & H_working==.   & year==2004 & sexsampled==2 & W_age>i5_2  & i9_2==1 & year<=i8_2 
replace H_working=(i11_3==1)   if inEPS06==1 & inEPS04==0 & H_working==.   & year==2004 & sexsampled==2 & W_age>i5_3  & i9_3==1 & year<=i8_3 
replace H_working=(i11_4==1)   if inEPS06==1 & inEPS04==0 & H_working==.   & year==2004 & sexsampled==2 & W_age>i5_4  & i9_4==1 & year<=i8_4 
replace H_working=-99 		   if inEPS06==1 & inEPS04==0 & married==0 	   & year==2004 & sexsampled==2 

replace W_working=(i11_1==1)+2*(i11_1==2)   if inEPS06==1 & inEPS04==0 & W_working==.   & year==2004 & sexsampled==1 & H_age>i5_1  & i9_1==1 & year<=i8_1 
replace W_working=(i11_2==1)+2*(i11_2==2)   if inEPS06==1 & inEPS04==0 & W_working==.   & year==2004 & sexsampled==1 & H_age>i5_2  & i9_2==1 & year<=i8_2 
replace W_working=(i11_3==1)+2*(i11_3==2)   if inEPS06==1 & inEPS04==0 & W_working==.   & year==2004 & sexsampled==1 & H_age>i5_3  & i9_3==1 & year<=i8_3 
replace W_working=(i11_4==1)+2*(i11_4==2)   if inEPS06==1 & inEPS04==0 & W_working==.   & year==2004 & sexsampled==1 & H_age>i5_4  & i9_4==1 & year<=i8_4 
replace W_working=-99                       if inEPS06==1 & inEPS04==0 & married==0     & year==2004 & sexsampled==1

replace H_formal_dum=(i15_1==1 & i14a_1<7)   if inEPS06==1 & inEPS04==0 & H_formal_dum==.   & year==2004 & sexsampled==2 & W_age>i5_1  & i9_1==1 & year<=i8_1 
replace H_formal_dum=(i15_2==1 & i14a_2<7)   if inEPS06==1 & inEPS04==0 & H_formal_dum==.   & year==2004 & sexsampled==2 & W_age>i5_2  & i9_2==1 & year<=i8_2 
replace H_formal_dum=(i15_3==1 & i14a_3<7)   if inEPS06==1 & inEPS04==0 & H_formal_dum==.   & year==2004 & sexsampled==2 & W_age>i5_3  & i9_3==1 & year<=i8_3 
replace H_formal_dum=(i15_4==1 & i14a_4<7)   if inEPS06==1 & inEPS04==0 & H_formal_dum==.   & year==2004 & sexsampled==2 & W_age>i5_4  & i9_4==1 & year<=i8_4 
replace H_formal_dum=-99 					 if inEPS06==1 & inEPS04==0 & married==0        & year==2004 & sexsampled==2 

replace W_formal_dum=(i15_1==1 & i14a_1<7)   if inEPS06==1 & inEPS04==0 & W_formal_dum==.   & year==2004 & sexsampled==1 & H_age>i5_1  & i9_1==1 & year<=i8_1 
replace W_formal_dum=(i15_2==1 & i14a_2<7)   if inEPS06==1 & inEPS04==0 & W_formal_dum==.   & year==2004 & sexsampled==1 & H_age>i5_2  & i9_2==1 & year<=i8_2 
replace W_formal_dum=(i15_3==1 & i14a_3<7)   if inEPS06==1 & inEPS04==0 & W_formal_dum==.   & year==2004 & sexsampled==1 & H_age>i5_3  & i9_3==1 & year<=i8_3 
replace W_formal_dum=(i15_4==1 & i14a_4<7)   if inEPS06==1 & inEPS04==0 & W_formal_dum==.   & year==2004 & sexsampled==1 & H_age>i5_4  & i9_4==1 & year<=i8_4 
replace W_formal_dum=-99 					 if inEPS06==1 & inEPS04==0 & married==0        & year==2004 & sexsampled==1

replace H_impbal=0 if H_impbal==. & sexsampled==1
replace W_impbal=0 if W_impbal==. & sexsampled==2
recode H_death W_death (.=0)
