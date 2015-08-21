	****************************************************
	* Created: 		September 16 2013
	* By: 			Thomas de Graaff
	* For: 			RiE project
	* Last edited: 	September 17, 2013
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
	************* Aggregate Capital data ***************
	****************************************************	
	use "${statafiles}databasis.dta", replace
	***********play with interest rate *****************
	by gamscode, sort: egen Imean = mean(I)
	replace I = Imean
	sort Year gamscode
	******************* !!!!
	gen TotalK  = SS1CapInc + SS2CapInc + SS3CapInc + SS4CapInc + ///
				 SS5CapInc + SS6CapInc + SS8CapInc + SS9CapInc + ///
				 SS10CapInc +SS11CapInc +SS12CapInc +SS13CapInc + ///
				 SS14CapInc +SS15CapInc 
	gen AgrK 	= SS1CapInc
	gen EMK		= SS2CapInc + SS3CapInc + SS4CapInc + ///
				  SS5CapInc + SS6CapInc + SS8CapInc
	gen ConK	= SS9CapInc
	gen DistK	= SS10CapInc + SS11CapInc + SS12CapInc
	gen ServK	= SS13CapInc + SS14CapInc
	gen NMServK	= SS15CapInc
	****************************************************
	************* Aggregate Capital data ***************
	****************************************************	
	gen TotalYV2  = SS1TotProd + SS2TotProd  + SS3TotProd  + SS4TotProd  + ///
						SS5TotProd  + SS6TotProd  + SS8TotProd  + SS9TotProd  + ///
						SS10TotProd  +SS11TotProd  +SS12TotProd  +SS13TotProd  + ///
						SS14TotProd  +SS15TotProd  
	gen AgrYV2 		= SS1TotProd 
	gen EMYV2		= SS2TotProd  + SS3TotProd  + SS4TotProd  + ///
						SS5TotProd  + SS6TotProd  + SS8TotProd 
	gen ConYV2		= SS9TotProd 
	gen DistYV2		= SS10CapInc + SS11TotProd  + SS12TotProd 
	gen ServYV2		= SS13TotProd  + SS14TotProd 
	gen NMServYV2	= SS15TotProd 	
	****************************************************
	************* Create Ln variables ******************
	****************************************************
	gen lnYTot 		= ln(TotalY)
	label var lnYTot "ln(total GDP) from Cambridge Econometrics"
	gen lnYTotV2 	= ln(TotalYV2)
	label var lnYTotV2 "ln(total GDP) from Mark"	
	gen lnKTot 		= ln(TotalK/(I/100))
	label var lnKTot "ln(total Capital)"	
	gen lnETot 		= ln(TotalE)
	label var lnETot "ln(total Employment)"	
	
	gen lnYAgr	 	= ln(AgrY)
	label var lnYAgr "ln(Agricultural GDP) from Cambridge Econometrics"
	gen lnYAgrV2 	= ln(AgrYV2)
	label var lnYAgrV2 "ln(Agricultural GDP) from Mark"		
	gen lnKAgr 		= ln(AgrK/(I/100))
	label var lnKAgr "ln(Agricultural Capital)"	
	gen lnEAgr 		= ln(AgrE)
	label var lnEAgr "ln(Agricultural Employment)"	
	
	gen lnYEM 		= ln(EMY)
	label var lnYEM "ln(Energy & manufacturing GDP) from Cambridge Econometrics"	
	gen lnYEMV2 	= ln(EMYV2)
	label var lnYEMV2 "ln(Energy & manufacturing GDP) from Mark"		
	gen lnKEM 		= ln(EMK/(I/100))
	label var lnKEM "ln(Energy & manufacturing Capital)"	
	gen lnEEM 		= ln(EME)
	label var lnEEM "ln(Energy & manufacturing Employment)"	
	
	gen lnYCon 		= ln(ConY)
	label var lnYCon "ln(Construction GDP) from Cambridge Econometrics"	
	gen lnYConV2 	= ln(ConYV2)
	label var lnYConV2 "ln(Construction GDP) from Mark"		
	gen lnKCon 		= ln(ConK/(I/100))
	label var lnKCon "ln(Construction Capital)"	
	gen lnECon	 	= ln(ConE)
	label var lnECon "ln(Construction Employment)"	
	
	gen lnYDist 	= ln(DistY)
	label var lnYDist "ln(Distribution GDP)"	
	gen lnYDistV2 	= ln(DistYV2)
	label var lnYDistV2 "ln(Distribution GDP) from Mark"		
	gen lnKDist 	= ln(DistK/(I/100))
	label var lnKDist "ln(Distribution Capital)"
	gen lnEDist		= ln(DistE)
	label var lnEDist "ln(Distribution Employment)"	
	
	gen lnYServ		= ln(ServY)
	label var lnYServ "ln(Services GDP) from Cambridge Econometrics"
	gen lnYServV2	= ln(ServYV2)
	label var lnYServV2 "ln(Services GDP) from Mark"	
	gen lnKServ		= ln(ServK/(I/100))
	label var lnKServ "ln(Services Capital)"
	gen lnEServ		= ln(ServE)
	label var lnEServ "ln(Services Employment)"
	
	gen lnYNMServ	= ln(NMServY)
	label var lnYNMServ "ln(Non-market GDP) from Cambridge Econometrics"	
	gen lnYNMServV2	= ln(NMServYV2)
	label var lnYNMServV2 "ln(Non-market GDP) from Mark"	
	gen lnKNMServ	= ln(NMServK/(I/100))
	label var lnKNMServ "ln(Non-market Capital)"	
	gen lnENMServ 	= ln(NMServE)
	label var lnENMServ "ln(Non-market Employment)"	
	****************************************************
	************* Label other variables  ***************
	****************************************************
	label var id "Identification number"
	label var I "Country-wise interest rate"	
	****************************************************
	*********** Drop unnessary variables ***************
	****************************************************
	drop SS*
	save "${statafiles}databasis.dta", replace
	export delimited using "${statafiles}databasis.csv", replace
	****************************************************
	*********** Close log file and end do file *********
	****************************************************
	cap log close
	exit	
