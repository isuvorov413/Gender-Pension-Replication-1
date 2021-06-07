cd C:\DATA\Bono
clear
infile year month adj_factor using adj_fact.txt
sort year month
save adj_fact.dta, replace

use C:\DATA\Bono\basebono.dta, clear
save temp, replace
drop if bono==""
destring fec_liq, g(fec_liq2)
g year_liq=fec_liq2-10000*int(fec_liq2/10000)
g month_liq=int((fec_liq2-1000000*int(fec_liq2/1000000))/10000)
keep if fec_liq2==.|year_liq>2005|(year_liq==2005&month_liq>6)
destring fecha_afi,replace
g year=int(fecha_afi/100)
g month=fecha_afi-100*year
sort year month
merge year month using adj_fact.dta
destring monto_nomi, replace
g bono2004=monto_nomi/adj_fact
keep folio bono2004
sort folio
drop if folio==.
save bono2004.dta, replace

