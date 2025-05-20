cd "~/Dropbox\Honors thesis-Flora"
global destination "~\Dropbox\Honors thesis-Flora\Stata File"

/*
Mac:
cd "/Users/yiwenpeng/Dropbox/Honor thesis-Flora
global destination "/Users/yiwenpeng/Dropbox/Honor thesis-Flora/Stata File"
*/


* 2006-2022
/*
import delimited using "Execucomp_Original.csv", clear

// extract ceo; create indicators ceo=1
gen ceo=1 if ceoann=="CEO"
replace ceo=0 if missing(ceo)
label var ceo "Company CEO"


keep execid year gender naics naicsdesc tdc1 total_curr coname cusip age ceo becameceo joined_co
order execid year coname cusip naics naicsdesc gender age  tdc1 total_curr
drop if year<2012
save "$destination/Execucomp_Original.dta", replace
*/

******** Execucomp ********
// Crosswalk
import excel using "Notes/CPS_NAICS Crosswalk.xlsx", first clear
rename *, lower
gen ind2dig=substr(cpsind,1,2)
destring ind2dig, replace
// Recode same as Step-1
recode ind2dig 14/17=14 3/4=3 24/25=24 26/27=26 31/32=31 35/36=35 37/38=38
replace ind2dig = cpsind2dig if missing(ind2dig) //repalce missing code with manual industry matching code

tempfile ind2dig
save `ind2dig'

use "$destination/Execucomp_Original.dta", clear
merge m:1 naics using `ind2dig', keep(3) nogen // temporary
//NAICS code 999977 & 999990 are unmatched and dropped because these are unspecified industries
drop if ind2dig==. 

rename (tdc1 total_curr) (totcomp currcomp)
replace totcomp=. if totcomp<0  // 1 obs
gen incencomp= totcomp-currcomp
replace incencomp=0 if incencomp<0		// 0 obs

// Demographics
gen female=(gender=="FEMALE") if gender~=""
drop gender

// CEO extraction
gen femceo=1 if ceo==1 & female==1
replace femceo=0 if missing(femceo)

global X "totcomp currcomp incencomp age"
global G "totcomp currcomp incencomp"

preserve
collapse (mean) $X (sum) n_femexe=female n_femceo=femceo n_totceo=ceo (count) n_totexe=female, by(ind2dig year)
tempfile means
save `means'
restore

collapse (mean) $G, by(ind2dig female year)
reshape wide $G, i(ind2dig year) j(female)
foreach v in $G {
	gen gap_`v'=ln(0.001+`v'1)-ln(0.001+`v'0)
	gen rat_`v'=`v'1/`v'0
}
rename *1 *F
rename *0 *M
merge 1:1 ind2dig year using `means', nogen

label var totcomp "Total compensation"
label var currcomp "Current compensation"
label var incencomp "Incentive compensation"

foreach v in $G {
	local lbl_`v' : variable label `v'
	label var `v'M "`lbl_`v'', males"
	label var `v'F "`lbl_`v'', females"
	label var gap_`v' "`lbl_`v'', gap"
	label var rat_`v' "`lbl_`v'', ratio"
	label var `v' "`lbl_`v'', average"
}
label var age "Average age of executives"
label var n_femexe "N of female executives"
label var n_totexe "Total N of executives"
label var n_femceo "N of female CEOs"
label var n_totceo "Total N of CEOs"
save "$destination/exec_by_industry.dta", replace


********** Industry Fundamentals Matching *************
import delimited using "Fundamentals_Original", clear
merge m:1 naics using `ind2dig', keep(3) nogen // All matched
drop if fyear<2012

//NAICS code 999977 & 999990 are dropped because these are unspecified industries
drop if ind2dig==. 

keep fyear act at gp ni re revt teq naics conm tic ind2dig
rename (fyear act at gp ni re revt teq) (year tot_curr_asset tot_asset gross_profit net_income retained_earnings tot_revenue stockholder_equity)
order year ind2dig

// Check summary data of revenue, assets, profit...
// As it is financial data, keep all negative values
global X "tot_curr_asset tot_asset gross_profit net_income retained_earnings tot_revenue stockholder_equity"
sum $X

collapse (mean) $X, by(ind2dig year)

label var tot_curr_asset "Total Current Assets, Average"
label var tot_asset "Total Assets, Average"
label var gross_profit "Gross Profit, Average"
label var net_income "Net Income, Average"
label var retained_earnings "Retained Earnings, Average"
label var tot_revenue "Total Revenue, Average"
label var stockholder_equity "Stockholder Equity, Average"
drop if year==.

tempfile Fundamentals
save `Fundamentals'

// Merge Industry Fundamentals with Execucomp
use "$destination/exec_by_industry.dta", clear
// 52 unmatched appeared because Execucomp doesn't contain 2023 data, and execucomp doesn't have industry 87 (automotive repair) for year 2012, 2016-2019
merge 1:1 ind2dig year using `Fundamentals'
drop if _merge==2
drop _merge
save "$destination/exec_funda_by_industry.dta", replace


****** Merge Industry Fundamentals, Execucomp, CPS together ******
use "$destination/asec_by_industry.dta", clear

// As Execucomp and Industry Fundamentals use fiscal years that start from June 1 through May 31, and because the ASEC CPS data is recorded in March, which falls within the fiscal year that began in the previous June, the way to convert calendar year to fiscal year is: Fiscal year = Calendar year + 1
gen fyear=year + 1
gen cyear = year
drop year
rename fyear year
// The year in asec is in fiscal year now, and the year range is between 2013-2022 fiscal year
drop if year > 2022
order year ind2dig

/* Unmatched appeared because Execucomp data missed certain industries which CPS contained; detailed description:
54: Florists, used merchandise, office supplied
56: vending machine operator, fuel dealers
65: Sound recording, motion picture
83: Individual and family service, community food and housing, vocational rehabilitation
87: automotive related service
88: commercial and industrial repair and maintenance
91&92: religious, political, union organization
93-95: public and government administration
*/ 
merge 1:1 ind2dig year using "$destination/exec_funda_by_industry.dta", keep(1 3)
drop if _merge == 1
drop _merge

gen share_exe=n_femexe/n_totexe
gen share_ceo=n_femceo/n_totceo

// label and save
label var year "Fiscal year; start from June 1"
label var cyear "Calendar year"
label var share_exe "Share of female executives in industry"
label var share_ceo "Share of female CEO in industry"
save "$destination/Exe_Asec_by_industry.dta", replace


**** Test Regression ******
global X "minority college fulltime age gap_totcomp"
global F "tot_asset gross_profit net_income retained_earnings tot_revenue"

xtset ind2dig year
reghdfe lgapearn share_exe share_ceo $X $F, absorb(year) 
reghdfe lgapearn share_exe share_ceo $X $F, absorb(year ind2dig) 
reghdfe lgapearn l.share_exe l.share_ceo $X $F, absorb(year ind2dig) 
?


* Manual coding of industries
* Monthly data (later)
* Fundamentals
* Extract CEOs 
* lagged effect
* Bartik instruments: (share of female executives in industry j in t-1) x (change in share of female executives across all industries)
* Come up with alternative instruments

ivregress 2sls lgapearn (share=l.share) $X i.year i.ind2dig, first



* N of firms
/*
* tenure
foreach v in becameceo joined_co {
	gen `v'_date=date(`v', "YMD")
	format `v'_date %td
}
*/

