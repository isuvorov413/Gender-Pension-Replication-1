drop H_edugrp W_edugrp H_agegrp W_agegrp

g H_agegrp=5*int(H_age/5) if married==1
g W_agegrp=5*int(W_age/5) if married==1
g H_edugrp=5*int(H_yrsed/5) if married==1
g W_edugrp=5*int(W_yrsed/5) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_proba=mean((H_balance>0)) if H_balance!=. & married==1
bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_proba2=max(H_proba) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_proba=mean((W_balance>0)) if W_balance!=. & married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_proba2=max(W_proba) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgbal=mean((H_balance)) if H_balance>0 & married==1
bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgbal2=max((H_avgbal)) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgbal=mean((W_balance)) if W_balance>0 & married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgbal2=max((W_avgbal)) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgXP=mean((H_XP)) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgXP=mean((W_XP)) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgworkinglag=mean((H_working_lag)) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgworkinglag=mean((W_working_lag)) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgformallag=mean((H_formal_lag)) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgformallag=mean((W_formal_lag)) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_proba3=mean((H_Xformal>0)) if H_Xformal!=. & married==1
bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_proba4=max(H_proba3) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_proba3=mean((W_formal>0)) if W_Xformal!=. & married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_proba4=max(W_proba3) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_proba5=mean((H_Xinformal>0)) if H_Xinformal!=. & married==1
bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_proba6=max(H_proba5) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_proba5=mean((W_informal>0)) if W_Xinformal!=. & married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_proba6=max(W_proba5) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgXF=mean((H_Xformal)) if H_Xformal>0 & married==1
bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgXF2=max((H_avgXF)) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgXF=mean((W_Xformal)) if W_Xformal>0 & married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgXF2=max((W_avgXF)) if married==1

bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgXI=mean((H_Xinformal)) if H_Xinformal>0 & married==1
bysort H_edugrp H_agegrp H_working H_formal_dum : egen H_avgXI2=max((H_avgXI)) if married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgXI=mean((W_Xinformal)) if W_Xinformal>0 & married==1
bysort W_edugrp W_agegrp W_working W_formal_dum : egen W_avgXI2=max((W_avgXI)) if married==1


g rand=runiform()
drop H_impbal W_impbal

g H_impbal=H_balance
g W_impbal=W_balance
g H_impXP=H_XP
g W_impXP=W_XP
g H_impworkinglag=H_working_lag
g W_impworkinglag=W_working_lag
g H_impformallag=H_formal_lag
g W_impformallag=W_formal_lag
g H_impXformal=H_Xformal
g W_impXformal=W_Xformal
g H_impXinformal=H_Xinformal
g W_impXinformal=W_Xinformal

replace H_impbal=H_avgbal2 if rand<H_proba2 & H_impbal==. & sexsampled==2
replace H_impbal=0         if rand>H_proba2 & H_impbal==. & sexsampled==2
replace W_impbal=W_avgbal2 if rand<W_proba2 & W_impbal==. & sexsampled==1
replace W_impbal=0         if rand>W_proba2 & W_impbal==. & sexsampled==1
replace H_impbal=0         if H_impbal==. & sexsampled==1
replace W_impbal=0         if W_impbal==. & sexsampled==2

replace H_impXP=round(H_avgXP) if H_impXP==. & sexsampled==2
replace W_impXP=round(W_avgXP) if W_impXP==. & sexsampled==1

replace H_impXformal=round(H_avgXF2) 		if rand<H_proba4 & H_impXformal==.   & sexsampled==2
replace H_impXformal=0        				if rand>H_proba4 & H_impXformal==.   & sexsampled==2
replace H_impXinformal=round(H_avgXI2)     	if rand<H_proba6 & H_impXinformal==. & sexsampled==2
replace H_impXinformal=0            		if rand>H_proba6 & H_impXinformal==. & sexsampled==2
replace W_impXformal=round(W_avgXF2) 		if rand<W_proba4 & W_impXformal==.   & sexsampled==1
replace W_impXformal=0        				if rand>W_proba4 & W_impXformal==.   & sexsampled==1
replace W_impXinformal=round(W_avgXI2)     	if rand<W_proba6 & W_impXinformal==. & sexsampled==1
replace W_impXinformal=0            		if rand>W_proba6 & W_impXinformal==. & sexsampled==1

replace H_impXformal=0            		if H_impXformal==.   & sexsampled==1
replace H_impXinformal=0            	if H_impXinformal==. & sexsampled==1
replace W_impXformal=0 					if W_impXformal==.   & sexsampled==2
replace W_impXinformal=0 				if W_impXinformal==. & sexsampled==2

replace H_impworkinglag=1 if rand<H_avgworkinglag & H_impworkinglag==. & sexsampled==2
replace H_impworkinglag=0 if rand>H_avgworkinglag & H_impworkinglag==. & sexsampled==2
replace W_impworkinglag=1 if rand<W_avgworkinglag & W_impworkinglag==. & sexsampled==1
replace W_impworkinglag=0 if rand>W_avgworkinglag & W_impworkinglag==. & sexsampled==1
replace H_impformallag=1  if rand<H_avgformallag  & H_impformallag==.  & sexsampled==2
replace H_impformallag=0  if rand>H_avgformallag  & H_impformallag==.  & sexsampled==2
replace W_impformallag=1  if rand<W_avgformallag  & W_impformallag==.  & sexsampled==1
replace W_impformallag=0  if rand>W_avgformallag  & W_impformallag==.  & sexsampled==1

drop H_balance W_balance H_XP W_XP H_working_lag W_working_lag H_formal_lag W_formal_lag H_Xformal H_Xinformal W_Xformal W_Xinformal 

rename H_impbal H_balance
rename W_impbal W_balance
*rename H_impXP H_XP
*rename W_impXP W_XP
rename H_impworkinglag H_working_lag
rename W_impworkinglag W_working_lag
rename H_impformallag H_formal_lag
rename W_impformallag W_formal_lag 
rename H_impXformal H_Xformal
rename H_impXinformal H_Xinformal
rename W_impXformal W_Xformal
rename W_impXinformal W_Xinformal
g H_XP=H_Xformal+H_Xinformal
g W_XP=W_Xformal+W_Xinformal
recode H_XP W_XP (-198=-99)
