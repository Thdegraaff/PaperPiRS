	****************************************************
	* Created: 		September 16, 2013
	* By: 			Thomas de Graaff
	* For: 			RiE project
	* Last edited: 	October 18, 2013
	****************************************************
	****************************************************
	*************** System parameters ******************
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
	************* Merge Interest data ******************
	****************************************************	
	import excel "${datafiles}ThomasVA.xlsx", sheet("Sheet1") firstrow
	reshape wide SS*, i(Year Region) j(Value,s)
	destring Region, generate(id) ignore(R)
	save "${statafiles}databasis.dta", replace
	clear
	import excel "${datafiles}InterestRates.xlsx", sheet("gamscode") firstrow
	reshape long I, i(gamscode) j(year)
	drop name nuts0 D NUTS2_NAAM06
	rename gamscode Region
	rename year Year
	save "${statafiles}interest.dta", replace
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year Region using "${statafiles}interest.dta"
	rename Region gamscode
	drop _*
	save "${statafiles}databasis.dta", replace	
	****************************************************
	************* Merge Employment data ****************
	****************************************************
	use "${statafiles}RIE_TotalE01.dta", replace
	reshape long TotalE, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	
	use "${statafiles}RIE_AgrE02.dta", replace
	reshape long AgrE, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace	

	use "${statafiles}RIE_EME03.dta", replace
	reshape long EME, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace	

	use "${statafiles}RIE_ConE04.dta", replace
	reshape long ConE, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace	
	
	use "${statafiles}RIE_DistE05.dta", replace
	reshape long DistE, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	
	use "${statafiles}RIE_ServE06.dta", replace
	reshape long ServE, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	
	use "${statafiles}RIE_NMServE07.dta", replace
	reshape long NMServE, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	****************************************************
	************* Merge Value Added (GVA) data *********
	****************************************************
	use "${statafiles}RIE_TotalY01.dta", replace
	reshape long TotalY, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	
	use "${statafiles}RIE_AgrY02.dta", replace
	reshape long AgrY, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace	

	use "${statafiles}RIE_EMY03.dta", replace
	reshape long EMY, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace	

	use "${statafiles}RIE_ConY04.dta", replace
	reshape long ConY, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace	
	
	use "${statafiles}RIE_DistY05.dta", replace
	reshape long DistY, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	
	use "${statafiles}RIE_ServY06.dta", replace
	reshape long ServY, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	
	use "${statafiles}RIE_NMServY07.dta", replace
	reshape long NMServY, i(gamscode) j(Year)
	save "${statafiles}temp.dta", replace	
	use "${statafiles}databasis.dta", replace
	merge 1:1 Year gamscode using "${statafiles}temp.dta"
	drop _* name Region
	save "${statafiles}databasis.dta", replace
	****************************************************
	************* Merge Human Capital ******************
	****************************************************
	// We changed the EducationV1.xlsx file by removing 
	// the n.a. characters.
	// This data is not yet merged. Too much problems with 
	// NUTS-2 classification
	/* clear
	import excel "${statafiles}EducationV1.xlsx", sheet("Education") firstrow
	fillin Y2006 Y2007 Y2008 Y2009 Y2010
	reshape long Y, i(Region) j(Year)
	sort Year
	drop _fillin
	rename Region NUTS2_06
	save "${statafiles}EducationV1.dta", replace
	use "${statafiles}databasis.dta", replace	
	merge m:1 Year NUTS2_06 using "${statafiles}EducationV1.dta"*/
	****************************************************
	*********** Close log file and end do file *********
	****************************************************
	rm "${statafiles}temp.dta"
	cap log close
	exit	
