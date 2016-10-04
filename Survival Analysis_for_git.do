/* Eric Haavind-Berman
REIT Survival Analysis For Juli
This do file is a preliminary test of survival analysis of REITS. The firms are 
split into Public and Non-Public and Equity and Mortgage, we want to see what are
the riskiest types of firms and why.

Steps:
1 - Import and Clean Liabilities Data, Import and Clean Assets Data, Merge Data
2 - Visual and Regression Analysis
*/

local wd "H:\Juli"
cd `wd'

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// 1 - Import and Merge ///////////////////////////////
////////////////////////////////////////////////////////////////////////////////

////////////////////////
//Import Liabilities
////////////////////////
import excel "Assets_and_Liabs.xlsx", sheet("Liabs") firstrow clear

// Drop variables where all observations are missing
foreach var of varlist _all {
	capture assert mi(`var')
	if !_rc {
	drop `var'
	}
}
// Dropping all rows with all missing values (No institution name)
drop if InstitutionName == ""

// Reshaping from years in columns to one column with the date
reshape long x, i(InstitutionName Public Type SNLInstitutionKey Ticker DateEstablished) ///
	j(date) string
rename x Liabilities

// make the date variable quarterly from string "yyyyQq" (2015Q2 for example)
gen year = substr(date,1,4)
gen quarter = substr(date,length(date),1)
destring year quarter, replace
drop date
gen date = yq(year, quarter)
drop year quarter
format date %tq

//Save
save "LiabsLong.dta", replace


////////////////////////
//Import Assets
////////////////////////
import excel "Assets_and_Liabs.xlsx", sheet("Assets") firstrow clear

// Reshaping from years in columns to one column with the date
reshape long x, i(InstitutionName Public Type SNLInstitutionKey Ticker DateEstablished) ///
	j(date) string
rename x Assets

// Same date modification as above
gen year = substr(date,1,4)
gen quarter = substr(date,length(date),1)
destring year quarter, replace
drop date
gen date = yq(year, quarter)
drop year quarter
format date %tq


//Merge with liabilities
merge 1:1 SNLInstitutionKey date using "LiabsLong.dta" //need perfect match
/* Perfect Merge! (better be)
    Result                           # of obs.
    -----------------------------------------
    not matched                             0
    matched                            38,350  (_merge==3)
    -----------------------------------------
*/
drop _merge

/////////////////////////////////////////////
// Just a bit more cleaning/set up
/////////////////////////////////////////////

// We don't care about it if we don't have data
drop if Assets == . //(24,670 observations deleted)

// Generate failure variable 
// (this will be the most recent available observation in the dataset)
sort SNLInstitutionKey date // need this so the most recent date is last
by SNLInstitutionKey: gen last = _n == _N
gen fail = 1 if date != yq(2015,2) & last == 1

// Survival date variable
gen survDate = date - 162

// Need to encode for ease of use
gen Equity = Type == "e" // dummy variable for equity vs mortgage REIT (equtiy = 1)
// label type variables
label define Equity 1 "Equity" 0 "Morgtage"
label define Public 1 "Public" 0 "NonPublic"
label value Equity Equity
label value Public Public

// Generate leverage variable
gen Leverage = Liabilities/Assets

// Tell Stata that this is survival-time
stset survDate, id(SNLInstitutionKey) failure(fail==1)

// GENERATE IMPORTANT VARIABLES
// Logged variables for % change rather than unit change
gen lLiab = ln(Liabilities)
gen lAss = ln(Assets)
gen lLev = ln(Leverage)
// Generate interaction term for public equity
gen PT = Public*Equity
// Generate number of fails per date for graphing
bysort date: egen num = count(fail) if fail == 1
bysort date: egen numFails = max(num) if fail == 1


//Save
save "REITclean.dta", replace


////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// 2 - Analysis ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
/* 
cd "H:\Juli"
use "REITclean.dta, clear 
*/
graph drop _all
//////////////////////////////////
// Descriptive Statistics and Graphs
//////////////////////////////////
// Tables
stdescribe
stsum
sts list, survival
sts test Public Equity
// Graphing
hist date if fail == 1, percent w(1) name(failures) // failures over time as % of total
sts graph, by(Equity) hazard name(hazard) 			// hazard estimates
sts graph, cumhaz name(cumhazard) 					// cumulative hazard 
sts graph, by(Public Equity) survival name(KapMei)	// Kaplan-Meier
graph bar numFails, over(date) by(Equity Public)	// UNFINISHED AND UGLY

// Regressions 
// TODO: code output with esttab
stcox lLev 
stcox lLiab lAss
stcox lLev Public Equity
stcox lLev lAss Public Equity PT, vce(robust)
stcox lLiab lAss Public Equity


////////////////////////////////////////////////////////////////////////////////
// Single observation survival analysis 
/* 1 - take median (or some other stat) of Assets, Liabs, and Leverage
   2 - find length of life (Failure Date - Established Date)
   3 - keep one observation per REIT
   4 - run analysis         */
////////////////////////////////////////////////////////////////////////////////

preserve // we don't want to mess with the old data so we will preserve/restore

// keep only observations with established date
drop if DateEstablished == . //(7,350 observations deleted)

// 1 - take stats
egen medAssets = median(Assets), by(SNLInstitutionKey)
egen medLiabs = median(Liabilities), by(SNLInstitutionKey)
egen medLev = median(Leverage), by(SNLInstitutionKey)

// 2 - find length of life
replace DateEstablished = qofd(DateEstablished)
egen lifetime = max(date - DateEstablished), by(SNLInstitutionKey)

// 3 - keep one observation per REIT and only variables we need
keep if last == 1
keep medAssets medLiabs medLev lifetime SNLInstitutionKey Public Equity fail

// tell Stata that this is survival data
stset lifetime, failure(fail)

// Run analysis
// TODO: code output with esttab
stcox medLev
stcox medLiab medAssets
stcox medLev Public Equity
stcox medLev medAssets Public Equity
stcox medLiab medAssets Public Equity

restore



