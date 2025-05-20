cd "C:\My Projects\Data-CPS"
global destination "~\Dropbox\Honors thesis-Flora\Stata File"
/*
Mac:
cd "/Users/yiwenpeng/Dropbox/Honor thesis-Flora/Stata File"
global destination "/Users/yiwenpeng/Dropbox/Honor thesis-Flora/Stata File"
*/

**********************************************************************
* 								ASEC
**********************************************************************
	
use asec_2012_2023, clear
recode earnweek 9999.99=.
keep if (empstat==10 | empstat==12) & age>=18 & age<65
drop if ind==0

gen ind2dig=int(ind/100)

/* Industry code with female <30 per year
3, 14, 17, 20, 25, 27, 32 (!), 36, 37

* aggregate ind2dig=32!

3:Mining and oil and gas extraction;  combine 3 with 4 to become "Mining, Quarrying, and Oil and Gas Extraction" (code=3)
14: Fabric manufacture; 17: footwear manufacture; combine 14 with 17 to become "Cloth manufacture" (code=14)

20: Petroleum manufacturing; leave it there as it is completely different from other manufacturing industry
25: Cement, concrete, lime, and gypsum, miscellaneous nonmetallic mineral product manufacturing ; combine with 24 to become "Ceramics, Glass, and Cement Manufacturing" (code=24)
27: Foundries, metal forgings and stampings, cutlery and hand tool manufacturing; combine with 26 to become "Metals Manufacturing and Processing" (code=26)
32: Machinery manufacturing unspecified; combine with 31 to become "Machinery Manufacturing" (code=31)

36: Transportation equipment manufacturing; combine with 35 (motor and aircraft manufacturing) to become "Transportation Equipment Manufacturing" (code=35)
37: Miscellaneous wood products and furniture manufacturing; combine with 38 to become "Woods Product, Processing, and Furniture Manufacturing" (code=38)
*/

// Aggregate industry with small industry size following the above criteria
recode ind2dig 14/17=14 3/4=3 24/25=24 26/27=26 31/32=31 35/36=35 37/38=38

// Hours
recode uhrsworkt 997 999=., gen(hours)
sum hours, d

gen fulltime=(hours>35) if hours<.
label var fulltime "full-time work"

// Demographics
recode hispan 100/700=1

recode race 100=0 200/300 805=1 650/804 806/830=0 999=., gen(minority)
replace minority=1 if hispan==1

recode race 100=0 200 805=1 300 650/804 806/830=0 999=., gen(nhblack)
replace nhblack=0 if hispan==1

// Marital status
recode marst 2=1 3/6 =0 9=., gen(married)
label var married "=1 if married"
label values married yesno

// Education
tab educ
tab educ, nol

label define educcat 1 "[1] None or 1-4 grades" 2 "[2] 5-12 grades, no diploma" 3 "[3] High school diploma" 4 "[4] College" 5 "[5] Graduate degree"
foreach v in educ {
	recode `v' 1=. 2 10=1 20/71=2  73/92=3  111=4  123/125=5
	label values `v' educcat
}

gen college=(educ>=4) if educ<.
gen highsch=(educ==3) if educ<.

global X "earnweek age hispan nhblack minority college highsch married fulltime hours"
global G "earnweek college highsch fulltime hours"  // gap variables
sum $X

// collapse count by year-industry
preserve
collapse (count) $X, by(ind2dig year)
foreach v in $X {
	rename `v' n_`v'
}
tempfile count
save `count'
restore

// collapse variables by year-industry
preserve
collapse (mean) $X [pw=asecwt], by(ind2dig year)
tempfile vars_byyear
save `vars_byyear'
restore

collapse (mean) $G [pw=asecwt], by(ind2dig sex year)
reshape wide $G, i(ind2dig year) j(sex)
foreach v in $G {
	gen gap_`v'=`v'2-`v'1
}
// lgapearn equation: ln(female wage/male wage) = ln(female wage) - ln(male wage)
gen lgapearn=ln(earnweek2)-ln(earnweek1)

drop *2 *1
merge 1:1 ind2dig year using `vars_byyear', nogen
merge 1:1 ind2dig year using `count', nogen

label var earnweek "Weekly earnings"
label var hispan "Hispanic workers"
label var nhblack "Non-Hispanic Black workers"
label var minority "Minority workers"
label var college "College graduates"
label var highsch "High school graduates"
label var married "Married workers"
label var fulltime "Full-time workers"
label var hours "Usual hours of work"
label var lgapearn "Log gap in earnings"
label var ind2dig "2-digit CPS industry"

foreach v in $X {
	local lbl_`v' : variable label `v'
	label var n_`v' "`lbl_`v'', count"
	cap label var gap_`v' "`lbl_`v'', gap"
	label var `v' "`lbl_`v'', share by industry"
}

save "$destination/asec_by_industry.dta", replace

