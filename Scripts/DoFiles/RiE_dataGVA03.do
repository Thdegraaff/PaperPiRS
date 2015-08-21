	****************************************************
	* Created: 		September 11, 2013
	* By: 			Thomas de Graaff
	* For: 			RiE project
	* Last edited: 	September 13, 2013
	****************************************************
	****************************************************
	*************** System parameters ******************
	****************************************************
	cap log close
	clear all
	set linesize 80
	version 12
	set more off
	****************************************************
	*************** Initialisation *********************
	****************************************************
	local EVar "EM"
	****************************************************
	************* Read data ****************************
	****************************************************
	import excel "${datafiles}gamscodering nuts2.xlsx", ///
	sheet(gamscode) firstrow clear
	rename Code_CambridgeEcon Region
	save "${statafiles}RiE_gamscode01.dta", replace
	import excel "${datafiles}GVACE.xls", ///
	sheet(`EVar') cellrange(A1:M379) firstrow clear
	****************************************************
	************* Manage data **************************
	****************************************************			
	forvalues x = 2000/2010 {
		gen `EVar'Y`x' = real(t`x')
		drop t`x'
	}
	replace Region = strupper(Region)
	save "${statafiles}RiE_`EVar'Y03.dta", replace
	****************************************************
	************** Merge data **************************
	****************************************************		
	use "${statafiles}RiE_gamscode01.dta", replace
	merge m:1 Region using "${statafiles}RiE_`EVar'Y03.dta"
	drop if _merge==2
	drop nuts0 NUTS2_06 NUTS2_NAAM06 Regio_CambrigdgeEcon Name _merge
	duplicates drop
	save "${statafiles}RiE_`EVar'Y03.dta", replace
	****************************************************
	*********** Close log file and end do file *********
	****************************************************
	cap log close
	exit
