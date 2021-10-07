g cohort=year-hage
replace cohort=year-wage if female==1


g outlays=spsmcostopt+mmpgcostopt+pasiscostopt
replace outlays=spsfcostopt+fmpgcostopt+pasiscostopt+supplement if female==1

g Doutlays=0.94^(year-2009)*outlays
g Dspsmcostopt=0.94^(year-2009)*spsmcostopt
g Dspsfcostopt=0.94^(year-2009)*spsfcostopt
g Dfmpgcostopt=0.94^(year-2009)*fmpgcostopt
g Dmmpgcostopt=0.94^(year-2009)*mmpgcostopt
g Dpasiscostopt=0.94^(year-2009)*pasiscostopt
g Dsupplement=0.94^(year-2009)*supplement
g Dtax_paidopt=0.94^(year-2009)*tax_paidopt
g Dutil=0.94^(year-2009)*util
g Dconsumption=0.94^(year-2009)*consumption

g outlays_sim=spsmcostopt_sim+mmpgcostopt_sim+pasiscostopt_sim
replace outlays_sim=spsfcostopt_sim+fmpgcostopt_sim+pasiscostopt_sim+supplement_sim if female==1

g Doutlays_sim=0.94^(year-2009)*outlays_sim
g Dspsmcostopt_sim=0.94^(year-2009)*spsmcostopt_sim
g Dspsfcostopt_sim=0.94^(year-2009)*spsfcostopt_sim
g Dfmpgcostopt_sim=0.94^(year-2009)*fmpgcostopt_sim
g Dmmpgcostopt_sim=0.94^(year-2009)*mmpgcostopt_sim
g Dpasiscostopt_sim=0.94^(year-2009)*pasiscostopt_sim
g Dsupplement_sim=0.94^(year-2009)*supplement_sim
g Dtax_paidopt_sim=0.94^(year-2009)*tax_paidopt_sim
g Dutil_sim=0.94^(year-2009)*util_sim
g Dconsumption_sim=0.94^(year-2009)*consumption_sim

g edugrp=H_edugrp


g nwperiods =0
replace nwperiods = 1 if hage<66 & female==0
replace nwperiods = 1 if wage<66 & female==1

replace fpenwithdraw=. if wage<66 | female==0
replace fpenwithdraw_sim=. if wage<66 | female==0
replace mpenwithdraw=. if hage<66 | female==1
replace mpenwithdraw_sim=. if hage<66 | female==1

replace fpenwithdraw=fpenwithdraw/1000000
replace fpenwithdraw_sim=fpenwithdraw_sim/1000000
replace mpenwithdraw=mpenwithdraw/1000000
replace mpenwithdraw_sim=mpenwithdraw_sim/1000000

replace pensionw=pensionw/1000000
replace pensionw_sim=pensionw_sim/1000000
replace pensionh=pensionh/1000000
replace pensionh_sim=pensionh_sim/1000000

g wealth60=wealth if ((hage==60 & female==0)|(wage==60 & female==1))
g wealth60_sim=wealth_sim if ((hage==60 & female==0)|(wage==60 & female==1))


g pensionh60=pensionh if hage==60 & female==0
g pensionw60=pensionw if wage==60 & female==1
g pensionh60_sim=pensionh_sim if hage==60 & female==0
g pensionw60_sim=pensionw_sim if wage==60 & female==1
