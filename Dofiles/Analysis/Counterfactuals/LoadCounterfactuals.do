
infile $varlist1 using "$resultspath\0\outputs\simulations.asc", clear
g counterfactual=0
save "$resultspath\AppendedCounterfactuals", replace

use "$resultspath\AppendedCounterfactuals", clear

forvalues c=10(1)10 {  
if (`c'==2 | `c'==9) {
continue
}
display "appending counterfactual `c'"
infile $varlist1 using "$resultspath\`c'\outputs\simulations.asc", clear

g counterfactual=`c'
append using "$resultspath\AppendedCounterfactuals"
save "$resultspath\AppendedCounterfactuals", replace
}
save "$resultspath\AppendedCounterfactuals", replace

