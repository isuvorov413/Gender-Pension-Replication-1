
***********************************************************************************************************************************************************************
***********************************************************************************************************************************************************************

cd C:\DATA\EPSWealth

use wealth06

merge folio using wealth04

ta _merge
drop _merge

renvars _all, subst(06 2006)
renvars _all, subst(04 2004)
reshape long housing realestate cars equipment business debt savings retirement wealth, i(folio) j(year)
keep folio year housing realestate cars equipment business debt savings retirement wealth
sort folio year
save EPSwealth.dta, replace

