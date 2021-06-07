
version 10
capture log close
set more off

*working directory
cd C:\Users\Clement\Dropbox\Genderequity\dofiles\

**********************************************************************************************
*Chilean Pension System: Labor history
******************************************
*by Clement Joubert
*July 16th 2008
*Location: C:\DATA\jointlaborstatus\masterdata

*Object: This dofile extracts labor histories pre 1980 from EPS06:

*IN: C:\DATA\EPS06\historialaboral.dta
*OUT: C:\DATA\jointlaborstatus\masterdata\laborhistorypre1980.dta


***********************************************************************************************


clear
set memory 500000
use C:\DATA\EPS06\entrevistado.dta, clear

save temp, replace

keep folio a9 b38 b39* b40 b41 b43* b44

*Number of years from first job to 1980
g yrs_pre1980=a9-b38-26
replace yrs_pre1980=0 if yrs_pre1980<0
replace yrs_pre1980=0 if a9<45

*Number of years worked before 1980
g exp_pre1980=yrs_pre1980 if b39_1==99 & b40==1
replace exp_pre1980=0.75*yrs_pre1980 if b39_1==99 & b40==2
replace exp_pre1980=0.5*yrs_pre1980 if b39_1==99 & b40==3
replace exp_pre1980=0.25*yrs_pre1980 if b39_1==99 & b40==4
replace exp_pre1980=0 if b39_1==99 & b40==9			/*MISSING OR 0?*/
replace exp_pre1980=0 if b39_1==.
replace exp_pre1980=b39_1+b39_2/12 if b39_1>=0 & b39_1!=99 & b39_1!=.
replace exp_pre1980=b39_1 if b39_2>=12 & (b39_2==b39_1|(b39_1>round(b39_2/12) & b39_1<round(1+b39_2/12)))& b39_1!=99 & b39_1!=.
replace exp_pre1980=yrs_pre1980 if exp_pre1980>yrs_pre1980 & b39_1!=99 & b39_1!=.
replace exp_pre1980=0 if a9<45

*Number of years of pension contributions before 1980
g cont_pre1980=b43_1+b43_2/12 if b41==1 
replace cont_pre1980=0 if b41==2
replace cont_pre1980=0 if b41==9 | (b43_1==99 & b44==9)                       
replace cont_pre1980=exp_pre1980 if b44==1 & b41==1 & b43_1==99
replace cont_pre1980=0.75*exp_pre1980 if b44==2 & b41==1 & b43_1==99
replace cont_pre1980=0.5*exp_pre1980 if b44==3 & b41==1 & b43_1==99
replace cont_pre1980=0.25*exp_pre1980 if b44==4 & b41==1 & b43_1==99
replace cont_pre1980=0.1*exp_pre1980 if b44==5 & b41==1 & b43_1==99
replace cont_pre1980=0 if exp_pre1980==0
replace cont_pre1980=exp_pre1980 if cont_pre1980>exp_pre1980

*Approximate formal experience with experience in a job where pension contributions were paid
g Xformal_pre80=int(cont_pre1980)
g Xinformal_pre80=int(exp_pre1980-cont_pre1980)

keep folio yrs_pre1980 cont_pre1980 exp_pre1980 Xformal_pre80 Xinformal_pre80
save laborhistorypre1980, replace
