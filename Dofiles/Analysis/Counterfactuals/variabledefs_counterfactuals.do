		g retage=65
		replace retage=60 if counterfactual!=3 & counterfactual!=10 & female==1
		
		g pension=pensionw
			replace pension=pensionh if female==0
			replace pension=pension/1000000
		g penwithdraw = fpenwithdraw
			replace penwithdraw = mpenwithdraw if female==0
			replace penwithdraw=penwithdraw/1000
		g LFP= sww2
			replace LFP = shw if female==0
		g hhage=wage
			replace hhage = hage if female==0
		g earn=earnw
			replace earn = earnh if female==0
			
			
		g hhagegroup5=5*int(hhage/5)
		g hhfexper=wfexper
			replace hhfexper=hfexper if female==0
		g hhfexpergrp=10*int(hhfexper/10)
		
		g sform=sformw
			replace sform=sformh if female==0
		g birthcohort=year-hhage
		g agein2004=2004-birthcohort
		g hhed=wed 
			replace hhed=hed if female==0
		g yeargrp5=5*int(year/5)

		g sex=female+1
		label define sex 1 "Male" 2 "Female"
		label values sex sex
		
	    label define married 0 "Single" 1 "Married"
		label values married married
		
		g systemcosts=spsmcostopt +  spsfcostopt +  fmpgcostopt  + mmpgcostopt +  pasiscostopt + supplement
