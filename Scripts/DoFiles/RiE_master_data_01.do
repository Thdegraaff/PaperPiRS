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
	macro drop _all
	set linesize 80
	version 12
	set more off
	****************************************************
	*************** Install packages *******************
	****************************************************
	* net install st0085_1.pkg
	****************************************************
	*************** Initialisation *********************
	****************************************************
	global datafiles "C:\Users\tgf200\Dropbox\Thomas\project\RiE\Data\Src\"
	global statafiles "C:\Users\tgf200\Dropbox\Thomas\project\RiE\Data\Derived\"
	global dofiles "C:\Users\tgf200\Dropbox\Thomas\project\RiE\Scripts\DoFiles\"
	global sysdir set UPDATES  ${DoFiles}  
	global sysdir set PLUS  ${DoFiles} 
	****************************************************
	*************** Do Files ***************************
	****************************************************	
	do "${dofiles}RiE_dataEmp01.do"
	do "${dofiles}RiE_dataEmp02.do"	
	do "${dofiles}RiE_dataEmp03.do"
	do "${dofiles}RiE_dataEmp04.do"
	do "${dofiles}RiE_dataEmp05.do"
	do "${dofiles}RiE_dataEmp06.do"
	do "${dofiles}RiE_dataEmp07.do"
	
	do "${dofiles}RiE_dataGVA01.do"
	do "${dofiles}RiE_dataGVA02.do"
	do "${dofiles}RiE_dataGVA03.do"
	do "${dofiles}RiE_dataGVA04.do"
	do "${dofiles}RiE_dataGVA05.do"
	do "${dofiles}RiE_dataGVA06.do"
	do "${dofiles}RiE_dataGVA07.do"
	
	do "${dofiles}RiE_datamerge08.do"
	do "${dofiles}RiE_dataaggr09.do"
	****************************************************
	*********** Close log file and end do file *********
	****************************************************
	cap log close
	exit
