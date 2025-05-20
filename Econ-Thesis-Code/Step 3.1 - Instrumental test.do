cd "~/Dropbox\Honors thesis-Flora/Stata File"
/*
cd "/Users/yiwenpeng/Dropbox/Honor thesis-Flora/Stata File"
*/

use "Exe_Asec_by_industry.dta", clear
xtset ind2dig year

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
xtset ind2dig year

gen s=1
replace s=. if ind2dig==57 &  year==2022

gen lnassets=ln(tot_asset)
gen lnprofit=ln(gross_profit)

global X "minority college fulltime age"
global F "lnassets lnprofit"

gen sector=ind2dig
recode sector 1/4=1 7/39=1 40/57=2 60/63=3 5/6=3 64/67=4 68/71=5 72/98=6
label define sector_label 1 "Agriculture, Mining, Construction, Manufacturing" ///
                          2 "Wholesale and Retail" ///
                          3 "Transportation, Warehousing, Utilities" ///
                          4 "Information" ///
                          5 "Finance, Insurance, Rental, Leasing" ///
                          6 "Other services"
label values sector sector_label

gen sharecat_ceo=ceil(share_ceo*10)
recode sharecat_ceo 2/10=2
tab sharecat_ceo, gen(share_)

		***	Generate dummy for female ceo share
		
// shareceo_10 =0 if industry has female ceo share <10%; =1 if industry has female ceo share >=10%
gen shareceo_10 = 1 if sharecat_ceo==2
replace shareceo_10 = 0 if shareceo_10==.

		*** Baseline

note: earnweek could also be included -> no change in results

reg lgapearn
outreg2 using baseline.xls, replace
foreach v in lgapearn {
	reghdfe `v' i.sharecat_ceo $X $F if s==1, absorb(year) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.sharecat_ceo $X $F if s==1, absorb(year sector) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes, Sector FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.sharecat_ceo $X $F if s==1, absorb(year ind2dig) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes, Industry FE, Yes) drop($FE) label dec(3)

	reghdfe `v' gap_totcomp $X $F if s==1, absorb(year) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes) drop($FE) label dec(3)

	reghdfe `v' gap_totcomp $X $F if s==1, absorb(year sector) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes, Sector FE, Yes) drop($FE) label dec(3)

	reghdfe `v' gap_totcomp $X $F if s==1, absorb(year ind2dig) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes, Industry FE, Yes) drop($FE) label dec(3)
}
foreach v in gap_totcomp {
	reghdfe `v' i.sharecat_ceo $X $F if s==1, absorb(year) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.sharecat_ceo $X $F if s==1, absorb(year sector) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes, Sector FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.sharecat_ceo $X $F if s==1, absorb(year ind2dig) 
	outreg2 using baseline.xls, append addtext(Year FE, Yes, Industry FE, Yes) drop($FE) label dec(3) sortvar(*sharecat_ceo gap_totcomp)
}
* reghdfe lgapearn i.sharecat_ceo##c.married $X $F if s==1, absorb(year sector) 
* reghdfe lgapearn i.sharecat_ceo##c.fulltime $X $F if s==1, absorb(year sector) 


		*** Mediating model 
		
*share -> compgap -> earngap (mediating model)
reg lgapearn gap_totcomp share_2 share_3 $X $F i.year i.sector if s==1
est store earngap
reg gap_totcomp share_2 share_3 $X $F i.year i.sector if s==1
est store compgap
suest earngap compgap, vce(robust)
nlcom 	(direct: [earngap_mean]share_3) ///
		(indirect: [earngap_mean]gap_totcomp*[compgap_mean]share_3) ///
		(total: [earngap_mean]share_3+[earngap_mean]gap_totcomp*[compgap_mean]share_3)

		*** Joint estimation using GSEM

* share -> compgap
* share -> earngap
gsem (lgapearn <- i.sharecat_ceo $X $F i.year i.sector) (gap_totcomp <- i.sharecat_ceo $X $F i.year i.sector), vce(cluster ind2dig) covstructure(e.lgapearn e.gap_totcomp, unstructured)		

// with common industry-level random effect
gsem (lgapearn <- i.sharecat_ceo $X $F i.year L[ind2dig]) (gap_totcomp <- i.sharecat_ceo $X $F i.year L[ind2dig]), vce(cluster ind2dig) covstructure(e.lgapearn e.gap_totcomp, unstructured)		
margins i.sharecat_ceo, nose predict(mu outcome(lgapearn))
marginsplot, name(lgapearn)
margins i.sharecat_ceo, nose predict(mu outcome(gap_totcomp))
marginsplot, name(gap_totcomp)
graph combine lgapearn gap_totcomp


		*** Instrumenal varables

* https://www.stata.com/manuals/rivregress.pdf
* drawback of this program is that it does not save first-stage results
* ivreg2 or ivregress2 allow to save first stage results but they do not allow for factor variables (create dummy variables)

// main stage
ivregress 2sls lgapearn (share_2 share_3=l.share_2 l.share_3 l.lnassets) $X $F i.year i.sector if s==1, robust first
gen s2=1 if e(sample)==1
est store ivregress
outreg2 using ivregress.xls, append bdec(3) cttop(IV-2nd) keep(share_2 share_3)

// first stage
reg share_2 l.share_2 l.share_3 l.lnassets $X $F i.year i.sector if s2==1, robust
outreg2 using ivregress.xls, append bdec(3) cttop(IV-1st) keep(l.share_2 l.share_3 l.lnassets)
test l.share_2 l.share_3 l.lnassets
reg share_3 l.share_2 l.share_3 l.lnassets $X $F i.year i.sector if s2==1, robust
outreg2 using ivregress.xls, append bdec(3) cttop(IV-1st) keep(l.share_2 l.share_3 l.lnassets)
test l.share_2 l.share_3 l.lnassets

* Relevance test: Is instrument relevant (correlated with endogeneous regressor)? Do joint test if multiple IVs
test l.share_2 l.share_3 l.lnassets
est restore ivregress
estat firststage, all

* Exogeneity test: Is covariate you believe to be endogenous in fact exogenous?
est restore ivregress
estat endogenous
note:  If the test statistic is significant, then the variables being tested must be treated as endogenous. First test is Wooldridge's (1995) test. Second test is the regression-based test following Hausman (1978), which is equivalent to:
qui reg share_2 l.share_2 l.share_3 l.lnassets $X $F i.year i.sector if s2==1
predict resid2, resid
qui reg share_3 l.share_2 l.share_3 l.lnassets $X $F i.year i.sector if s2==1
predict resid3, resid
qui reg lgapearn share_2 share_3 $X $F i.year i.sector resid2 resid3 if s2==1, robust
test resid2 resid3

* Overidentification test
note: if N of instruments > N of endogenous regressors (i.e., the model is overidentified), we can test if instruments are invalid (significant statistics at the 5% level means that either one or more instruments are invalid or should belong to the main equation)
estat overid 

// 
ivregress 2sls lgapearn (gap_totcomp=l.gap_totcomp l.lnassets) $X $F i.year i.sector if s==1, robust first
estat firststage 
estat endog 
estat overid 

// alternative instrument - Bartik
gen interact2 = l.share_2*change_share_exe
gen interact3 = l.share_3*change_share_exe
ivregress 2sls lgapearn (share_2 share_3=interact2 interact3 l.lnassets) $X $F i.year  i.sector if s==1, robust first
estat endog 
estat overid 
estat firststage 

		*** Lag effects
		
reghdfe lgapearn l(0/2).gap_totcomp $X $F if s==1, absorb(year sector)


******************** Model with two categories (>10% share indicator) as treatment variable ****************
		*** Baseline
reg lgapearn
outreg2 using baseline2.xls, replace
foreach v in lgapearn {
	reghdfe `v' i.shareceo_10 $X $F if s==1, absorb(year) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.shareceo_10 $X $F if s==1, absorb(year sector) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes, Sector FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.shareceo_10 $X $F if s==1, absorb(year ind2dig) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes, Industry FE, Yes) drop($FE) label dec(3)

	reghdfe `v' gap_totcomp $X $F if s==1, absorb(year) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes) drop($FE) label dec(3)

	reghdfe `v' gap_totcomp $X $F if s==1, absorb(year sector) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes, Sector FE, Yes) drop($FE) label dec(3)

	reghdfe `v' gap_totcomp $X $F if s==1, absorb(year ind2dig) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes, Industry FE, Yes) drop($FE) label dec(3)
}
foreach v in gap_totcomp {
	reghdfe `v' i.shareceo_10 $X $F if s==1, absorb(year) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.shareceo_10 $X $F if s==1, absorb(year sector) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes, Sector FE, Yes) drop($FE) label dec(3)

	reghdfe `v' i.shareceo_10 $X $F if s==1, absorb(year ind2dig) 
	outreg2 using baseline2.xls, append addtext(Year FE, Yes, Industry FE, Yes) drop($FE) label dec(3) sortvar(*sharecat_ceo gap_totcomp)
}

// Baseline with interaction term
reghdfe lgapearn i.shareceo_10##c.married $X $F if s==1, absorb(year sector) 
reghdfe lgapearn i.shareceo_10##c.fulltime $X $F if s==1, absorb(year sector)


		*** Instrumental variable
// main stage
ivregress 2sls lgapearn (share_2 share_3=l.share_2 l.share_3 l.lnassets) $X $F i.year i.sector if s==1, robust first
gen s2=1 if e(sample)==1
est store ivregress


