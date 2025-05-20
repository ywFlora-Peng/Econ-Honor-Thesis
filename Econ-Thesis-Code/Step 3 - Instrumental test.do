cd "/Users/yiwenpeng/Dropbox/Honor thesis-Flora
global destination "/Users/yiwenpeng/Dropbox/Honor thesis-Flora/Stata File"

use "$destination/Exe_Asec_by_industry.dta", replace
xtset ind2dig year

/*
// Calculate change of share of female CEO across all industries
preserve
collapse (mean) share_exe, by(year)
tsset year
gen change_share_exe = D.share_exe
tempfile Share
save `Share'
restore

use "$destination/Exe_Asec_by_industry.dta", clear
merge n:1 year using `Share' , keep(3) nogen
*/


global X "minority college fulltime age gap_totcomp"
global F "tot_asset gross_profit net_income retained_earnings tot_revenue"

// OLS model
xtset ind2dig year
reghdfe lgapearn share_exe share_ceo $X $F, absorb(year) 
reghdfe lgapearn share_exe share_ceo $X $F, absorb(year ind2dig) 
reghdfe lgapearn l.share_exe l.share_ceo $X $F, absorb(year ind2dig) 


*** Option 1 ***
ivregress 2sls lgapearn (share_exe=l.share_exe l.tot_asset) share_ceo minority college fulltime age gap_totcomp gross_profit net_income retained_earnings tot_revenue i.year i.ind2dig, robust
estat endog // small p-value; reject the null >> endogenous
estat overid // large p-value; cannot reject the null; validity of exclusion restriction
estat firststage //>10, strong result

*** Option 2 ***
ivregress 2sls lgapearn share_exe (share_ceo= l.gap_totcomp l.tot_asset l.gross_profit) minority college fulltime age  net_income retained_earnings tot_revenue i.year i.ind2dig, robust
estat endog 
estat overid 
estat firststage 

*** Option 3 ***
ivregress 2sls lgapearn l.share_ceo (share_exe= l.minority l.college l.fulltime) minority college fulltime age  gap_totcomp tot_asset gross_profit net_income retained_earnings tot_revenue i.year i.ind2dig, robust
estat endog 
estat overid 
estat firststage 


*** Option 4***
xtset ind2dig year
gen interact = l.share_exe*change_share_exe
ivregress 2sls lgapearn (share_exe=l.share_exe change_share_exe interact) share_ceo $X $F i.year i.ind2dig, robust
estat endog // drop
estat overid 
estat firststage 


/*
** solve endogeneity; IV
// IV test
ivregress 2sls decision_health (heard_fp= electricity radio television telephone) time hh_num age wealth educ_yrs rural iliteracy first_birth early_preg ideal_num head_sex,robust
estat endog //p-value is large, fail to reject the null of endogeneity; support being engogenous and intend to believe th eresult
estat overid // prove valid exclusion
estat firststage //77 > 10
*/
