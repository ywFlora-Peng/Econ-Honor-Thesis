cd "~/Dropbox\Honors thesis-Flora"
global destination "~\Dropbox\Honors thesis-Flora\Stata File"

// Crosswalk
import excel using "Notes/CPS_NAICS Crosswalk.xlsx", first clear
rename *, lower
gen ind2dig=substr(cpsind,1,2)
destring ind2dig, replace
recode ind2dig 14/17=14 3/4=3 24/25=24 26/27=26 31/32=31 35/36=35 37/38=38 // recode same as Step-1
replace ind2dig = cpsind2dig if missing(ind2dig) // replace missing code with manual industry matching code

gen naics4dig=naics if naics<10000
replace naics4dig=int(naics/10) if naics>10000 & naics<100000
replace naics4dig=int(naics/100) if naics>100000 & naics<.
egen sd=sd(ind2dig), by(naics4dig)
sum sd, d
keep naics4dig ind2dig
duplicates drop
save "$destination/crosswalk.dta", replace

*******************************************************************
*						Execucomp 
*******************************************************************

import delimited using "Execucomp_Original.csv", clear
drop if year<2012
gen naics4dig=naics if naics<10000
replace naics4dig=int(naics/10) if naics>10000 & naics<100000
replace naics4dig=int(naics/100) if naics>100000 & naics<.
merge m:1 naics4dig using "$destination/crosswalk.dta", keep(1 3) nogen 
tab naics4dig if ind2dig==.  // NAICS code 999977 & 999990 dropped
drop if ind2dig==. 

rename (tdc1 total_curr) (totcomp currcomp)
replace totcomp=. if totcomp<0  // 1 obs
gen incencomp= totcomp-currcomp

// Demographics
gen female=(gender=="FEMALE")
drop gender

// CEO extraction
tab ceo female, mis
gen ceo=(ceoann=="CEO")
gen femceo=(ceo==1 & female==1)

global X "totcomp currcomp incencomp age"
global G "totcomp currcomp incencomp"

// Averages/sums by industry-year
preserve
collapse (mean) $X (sum) n_femexe=female n_femceo=femceo n_totceo=ceo (count) n_totexe=female, by(ind2dig year)
tempfile means
save `means'
restore

// Averages by industry-gender-year (to calculate gaps)
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

gen share_exe=n_femexe/n_totexe
gen share_ceo=n_femceo/n_totceo
label var share_exe "Share of female executives in industry"
label var share_ceo "Share of female CEO in industry"

// Change in annual share of female CEOs
preserve
collapse (sum) n_femceo n_totceo, by(year)
tsset year
gen share_ceo=n_femceo/n_totceo
gen dshare_ceo = D.share_ceo
label var dshare_ceo "Change in annual share of female CEOs"
keep year dshare_ceo
tempfile share
save `share'
restore
merge n:1 year using `share', nogen

save "$destination/exec_by_industry.dta", replace


*******************************************************************
*						Industry Fundamentals Matching 
*******************************************************************

import delimited using "Fundamentals_Original", clear

// Dates
gen date=date(datadate, "YMD")
format date %td
gen _year=year(date)
gen _month=month(date)
gen year=_year if _month>=6 & _month<13
replace year=_year-1 if _month<6
drop _*
drop if year<2012 | year==2023

// Remove duplicates
drop if indfmt=="FS"
bys conm year (date): drop if _N>1 & _n<_N
duplicates list conm year

gen naics4dig=naics if naics<10000
replace naics4dig=int(naics/10) if naics>10000 & naics<100000
replace naics4dig=int(naics/100) if naics>100000 & naics<.
merge m:1 naics4dig using "$destination/crosswalk.dta", keep(2 3)
tab naics4dig if _m==2				// !!!6 industries are not in "fundamentals" data -> find match
drop if _m==2
tab naics4dig if ind2dig==. & _m==3	
drop if naics4dig==9999 			  	// NAICS code 9999 dropped
order conm cusip tic year naics4dig ind2dig

keep conm cusip tic year naics naics4dig ind2dig act at gp ni re revt teq   
rename (act at gp ni re revt teq) (tot_curr_asset tot_asset gross_profit net_income retained_earnings tot_revenue stockholder_equity)

// Check summary data of revenue, assets, profit...
// As it is financial data, keep all negative values
global X "tot_curr_asset tot_asset gross_profit net_income retained_earnings tot_revenue stockholder_equity"
sum $X

collapse (mean) $X, by(ind2dig year)		// !!!problematic
label var tot_curr_asset "Total Current Assets, Average"
label var tot_asset "Total Assets, Average"
label var gross_profit "Gross Profit, Average"
label var net_income "Net Income, Average"
label var retained_earnings "Retained Earnings, Average"
label var tot_revenue "Total Revenue, Average"
label var stockholder_equity "Stockholder Equity, Average"
save "$destination/fundam_by_industry.dta", replace

// Merging data sets
use "$destination/asec_by_industry.dta", clear
clonevar survyear=year
replace year=year-1
drop if year==2011
merge 1:1 ind2dig year using "$destination/exec_by_industry.dta"
tab ind2dig if _m==1		// ok
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
drop if _m==1
drop _m
merge 1:1 ind2dig year using "$destination/fundam_by_industry.dta", nogen
label var year "Year"

save "$destination/Exe_Asec_by_industry.dta", replace



