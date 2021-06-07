
version 10
capture log close
set more off

cd C:\DATA\EPSkids
**********************************************************************************************
*Chilean Pension System: kids history
******************************************
*by Naoki Aizawa
*December 10th 2010


*Object: This dofile extracts kids panel from EPS:

*IN: C:\EPSlabor\EPSlabor02\EPS02\Base6.dta
*OUT: C:\DATA\jointlaborstatus\masterdata\laborhistory19802002.dta


***********************************************************************************************


clear
set memory 900000

**** create household kids informaiton at 2002 ******************
use "C:\DATA\EPS02\Base5.dta",clear
sort folio
save tempbase5,replace
clear
use "C:\DATA\EPS02\Base6.dta",clear

* my approach is not robust with respect to missing value

rename  vip12agn kids_birth_year

************** this is dummy whether kids are born at 2001 or 2002
* gen birth2001=1 if kids_birth_year==2001


gen birth2002=0
replace birth2002=1 if kids_birth_year==2002
* construct a dummy to make outflows from household due to age
gen birthold=1 if kids_birth_year<1984
gen drop2003=1 if kids_birth_year==1984
gen drop2004=1 if kids_birth_year==1985
gen drop2005=1 if kids_birth_year==1986
gen drop2006=1 if kids_birth_year==1987
gen drop2007=1 if kids_birth_year==1988
gen drop2008=1 if kids_birth_year==1989
gen drop2009=1 if kids_birth_year==1990
************** determine kids total and kids newly born
sort folio
by folio: egen kids_all2002=max(orden)
replace kids_all2002=0 if kids_all2002==.
* my approach does not distinguish missing value and others
by folio: egen kids_new2002=sum(birth2002)
by folio: egen kids_drop2002=sum(birthold)
by folio: egen drop_kids2003=sum(drop2003)
by folio: egen drop_kids2004=sum(drop2004)
by folio: egen drop_kids2005=sum(drop2005)
by folio: egen drop_kids2006=sum(drop2006)
by folio: egen drop_kids2007=sum(drop2007)
by folio: egen drop_kids2008=sum(drop2008)
by folio: egen drop_kids2009=sum(drop2009)

* count the number of kids except those under age 18
gen kids_tot2002=kids_all2002-kids_drop2002

duplicates drop folio, force

keep kids_all2002 folio kids_tot2002 kids_new2002 drop_kids2003 drop_kids2004 drop_kids2005 drop_kids2006 drop_kids2007 drop_kids2008 drop_kids2009

sort folio

save kids2002.dta,replace
clear
* inpute kids information to all sample
use "C:\DATA\EPS02\Base1.dta",clear
sort folio
merge folio using kids2002.dta

* assume people who do not report anything is 0. This may be problematic because I do not distinguish from missing infor.

replace kids_tot2002=0 if kids_tot2002==.
replace kids_new2002=0 if kids_new2002==.
replace drop_kids2003=0 if drop_kids2003==.
replace drop_kids2004=0 if drop_kids2004==.
replace drop_kids2005=0 if drop_kids2005==.
replace drop_kids2006=0 if drop_kids2006==.
replace drop_kids2007=0 if drop_kids2007==.
replace drop_kids2008=0 if drop_kids2008==.
replace drop_kids2009=0 if drop_kids2009==.


keep folio kids_tot2002 kids_new2002 drop_kids2003 drop_kids2004 drop_kids2005 drop_kids2006 drop_kids2007 drop_kids2008 drop_kids2009

sort folio

save kids2002updated.dta,replace
clear

**** create household kids informaiton at 2004 ******************

use "C:\DATA\EPS04\hijos.dta",clear

rename   i20_02 kids_birth_year


gen birth2003=1 if kids_birth_year==2003
gen birth2004=1 if kids_birth_year==2004

************** determine kids total and kids newly born
sort folio
by folio: egen kids_tot2004=max(orden)  
replace kids_tot2004=0 if kids_tot2004==.
* this total is created for new sample
by folio: egen kids_new2003=sum(birth2003)
by folio: egen kids_new2004=sum(birth2004)


duplicates drop folio, force

keep folio kids_tot2004 kids_new2003 kids_new2004

sort folio

save kids2004.dta,replace
clear
use "C:\DATA\EPS04\Entrevistado.dta",clear
sort folio

merge folio using kids2004.dta


replace kids_tot2004=0 if kids_tot2004==.
replace kids_new2003=0 if kids_new2003==.
replace kids_new2004=0 if kids_new2004==.



keep folio kids_tot2004 kids_new2003 kids_new2004

sort folio
save kids2004updated.dta,replace
clear


**** create household kids informaiton at 2006 ******************

use "C:\DATA\EPS06\hijos.dta",clear

rename    i21a kids_birth_year



gen birth2005=1 if kids_birth_year==2005
gen birth2006=1 if kids_birth_year==2006

************** determine kids total and kids newly born
sort folio
by folio: egen kids_tot2006=max(orden)  
replace kids_tot2006=0 if kids_tot2006==.
* this total is created for new sample
by folio: egen kids_new2005=sum(birth2005)
by folio: egen kids_new2006=sum(birth2006)


duplicates drop folio, force

keep folio kids_tot2006 kids_new2005 kids_new2006

sort folio

save kids2006.dta,replace
clear
use "C:\DATA\EPS06\Entrevistado.dta",clear

sort folio 

merge folio using kids2006.dta

replace kids_tot2006=0 if kids_tot2006==.
replace kids_new2005=0 if kids_new2005==.
replace kids_new2006=0 if kids_new2006==.



keep folio kids_tot2006 kids_new2005 kids_new2006

sort folio

save kids2006updated.dta,replace
clear
**** create household kids informaiton at 2008 ******************

use "C:\DATA\EPS09\hijos.dta",clear

rename    i21_02 kids_birth_year

gen birth2007=1 if kids_birth_year==2007
gen birth2008=1 if kids_birth_year==2008
gen birth2009=1 if kids_birth_year==2009


************** determine kids total and kids newly born
sort folio
by folio: egen kids_tot2009=max(orden)  
replace kids_tot2009=0 if kids_tot2009==.
* this total is created for new sample
by folio: egen kids_new2007=sum(birth2007)
by folio: egen kids_new2008=sum(birth2008)
by folio: egen kids_new2009=sum(birth2009)

duplicates drop folio, force

keep folio kids_tot2009 kids_new2007 kids_new2008 kids_new2009

sort folio

save kids2009.dta,replace
clear
use "C:\DATA\EPS09\Entrevistado.dta",clear

sort folio 

merge folio using "C:\DATA\EPSkids\kids2009.dta"

replace kids_tot2009=0 if kids_tot2009==.
replace kids_new2007=0 if kids_new2007==.
replace kids_new2008=0 if kids_new2008==.
replace kids_new2009=0 if kids_new2009==.



keep folio kids_tot2009 kids_new2007 kids_new2008 kids_new2009

sort folio

save "C:\DATA\EPSkids\kids2009updated.dta",replace
clear

****** construct a panel from 2002 **********************
use "C:\DATA\EPSkids\kids2002updated.dta", clear
g sample02in=1
sort folio
merge folio using "C:\DATA\EPSkids\kids2004updated.dta"
g sample0204in=1 if _merge==3
drop _merge
sort folio
merge folio using "C:\DATA\EPSkids\kids2006updated.dta"
g sample020406in=1 if _merge==3 & sample0204in==1
drop _merge
sort folio
merge folio using "C:\DATA\EPSkids\kids2009updated.dta"
g sample02040609in=1 if _merge==3 & sample020406in==1
drop _merge

g kids2002=kids_tot2002

* create kids per year
g kids2003=kids_tot2002+kids_new2003-drop_kids2003 if sample0204in==1
* replace kids2003=

g kids2004=kids2003+kids_new2004-drop_kids2004 if sample0204in==1
* replace kids2004

g kids2005=kids2004+kids_new2005-drop_kids2005 if sample020406in==1
* replace kids2005

g kids2006=kids2005+kids_new2006-drop_kids2006 if sample020406in==1
* replace kids2006

g kids2007=kids2006+kids_new2007-drop_kids2007 if sample02040609in==1
* replace kids2007

g kids2008=kids2007+kids_new2008-drop_kids2008 if sample02040609in==1
* replace kids2008

g kids2009=kids2008+kids_new2009-drop_kids2009 if sample02040609in==1
* replace kids2009


* right now, I only keep samples who are in all periods
*keep if sample02040609in==1

keep folio kids2002 kids2003 kids2004 kids2005 kids2006 kids2007 kids2008 kids2009 kids_new2002 kids_new2003 kids_new2004 kids_new2005 kids_new2006 kids_new2007 kids_new2008 kids_new2009


reshape long kids kids_new, i(folio) j(year)

save "C:\DATA\EPSkids\kidspanel.dta",replace

* as a first step, I will keep samples who are in all surveys
