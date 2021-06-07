
version 10
capture log close
set more off
cd  "C:\Users\WB452275\OneDrive - WBG\Documents\"


* Set rootpath
glo rootdir "C:\Users\WB452275\OneDrive - WBG\Documents\GitHub\Gender-Pension-Replication"


* Create secondary paths
glo dodir "${rootdir}\Dofiles"
glo workdata "${rootdir}\DATA\WorkingDatasets"
glo rawdata "${rootdir}\DATA\RawData"


*TASK 1: Create initial conditions and data for moments creation
*******
*Step0: Create intermediate datasets for labor histories, household wealth, 
*		marital history, socio demographics from raw EPS 2002, 2004, 2006, 2009.

*		Raw data should be requested from the Subsecretaria de prevision social de Chile at:
* 		https://www.previsionsocial.gob.cl/sps/biblioteca/encuesta-de-proteccion-social/bases-de-datos-eps/	
do "${dodir}\DataConstruction\upstream\Intermediate.do" // REPLICABILITY IN PROGRESS...

*Step1: Assemble survey and admin data into masterdata.dta
do "${dodir}\DataConstruction\upstream\masterdata.do"

*Step2: Create Husband/Wife data set masterdataHW.dta
do "${dodir}\DataConstruction\upstream\masterdataHW.do"

*Step3: Output initialconditions.txt and dataformoments.txt and document sample restrictions
do "${dodir}\Dataconstruction\GenderPension.do"

*TASK 2: Load simulations and output figures and tables
********
* REPLICABILITY IN PROGRESS...
