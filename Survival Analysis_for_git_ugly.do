/* Eric Haavind-Berman
REIT Survival Analysis For Juli
This do file is a preliminary test of survival analysis of REITS. The firms are 
split into Public and Non-Public and Equity and Mortgage, we want to see what are
the riskiest types of firms and why.

Steps:
1 - Import and Clean Liabilities Data, Import and Clean Assets Data, Merge Data
2 - Visual and Regression Analysis
*/

clear all
cd "H:\Juli"

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// 1 - Import and Merge ///////////////////////////////
////////////////////////////////////////////////////////////////////////////////


//Import Liabilities
import excel "Assets_and_Liabs.xlsx", sheet("Liabs") firstrow clear
drop BN BO BP BQ BR BS BT BU BV BW

save "LiabsWide.dta", replace


//Changing from years in columns to one column with the date
local vars x2015Q2 x2015Q1 x2014Q4 x2014Q3 x2014Q2 x2014Q1 x2013Q4 x2013Q3 ///
	x2013Q2 x2013Q1 x2012Q4 x2012Q3 x2012Q2 x2012Q1 x2011Q4 x2011Q3 x2011Q2 ///
	x2011Q1 x2010Q4 x2010Q3 x2010Q2 x2010Q1 x2009Q4 x2009Q3 x2009Q2 x2009Q1 ///
	x2008Q4 x2008Q3 x2008Q2 x2008Q1 x2007Q4 x2007Q3 x2007Q2 x2007Q1 x2006Q4 ///
	x2006Q3 x2006Q2 x2006Q1 x2005Q4 x2005Q3 x2005Q2 x2005Q1 x2004Q4 x2004Q3 ///
	x2004Q2 x2004Q1 x2003Q4 x2003Q3 x2003Q2 x2003Q1 x2002Q4 x2002Q3 x2002Q2 ///
	x2002Q1 x2001Q4 x2001Q3 x2001Q2 x2001Q1 x2000Q4

local l = 1
foreach v in `vars' {
	use "LiabsWide.dta"
	keep InstitutionName Public Type SNLInstitutionKey Ticker DateEstablished `v'
	rename `v' Liabilities
	gen date = "`v'"
	if `l' == 1 {
		save "LiabsLong.dta", replace
		local l = 2
	}
	else {
		append using "LiabsLong.dta"
		save "LiabsLong.dta", replace
	}
}
//Date stuff
gen year = substr(date,2,4)
gen quarter = substr(date, 7,1)
destring year quarter, replace
drop date
gen date = yq(year, quarter)
drop year quarter
format date %tq

//For some reason there were a ton of empty cells 
// (probably from the bottom of the excel file)
drop if SNLInstitutionKey == .

//Save
save "LiabsLong.dta", replace






//Import Assets
import excel "Assets_and_Liabs.xlsx", sheet("Assets") firstrow clear

save "AssetsWide.dta", replace

//Changing from years in columns to one column with the date
local vars x2015Q2 x2015Q1 x2014Q4 x2014Q3 x2014Q2 x2014Q1 x2013Q4 x2013Q3 ///
	x2013Q2 x2013Q1 x2012Q4 x2012Q3 x2012Q2 x2012Q1 x2011Q4 x2011Q3 x2011Q2 ///
	x2011Q1 x2010Q4 x2010Q3 x2010Q2 x2010Q1 x2009Q4 x2009Q3 x2009Q2 x2009Q1 ///
	x2008Q4 x2008Q3 x2008Q2 x2008Q1 x2007Q4 x2007Q3 x2007Q2 x2007Q1 x2006Q4 ///
	x2006Q3 x2006Q2 x2006Q1 x2005Q4 x2005Q3 x2005Q2 x2005Q1 x2004Q4 x2004Q3 ///
	x2004Q2 x2004Q1 x2003Q4 x2003Q3 x2003Q2 x2003Q1 x2002Q4 x2002Q3 x2002Q2 ///
	x2002Q1 x2001Q4 x2001Q3 x2001Q2 x2001Q1 x2000Q4

local l = 1
foreach v in `vars' {
	use "AssetsWide.dta"
	keep InstitutionName Public Type SNLInstitutionKey Ticker DateEstablished `v'
	rename `v' Assets
	gen date = "`v'"
	if `l' == 1 {
		save "AssetsLong.dta", replace
		local l = 2
	}
	else {
		append using "AssetsLong.dta"
		save "AssetsLong.dta", replace
	}
}
//Date stuff
gen year = substr(date,2,4)
gen quarter = substr(date, 7,1)
destring year quarter, replace
drop date
gen date = yq(year, quarter)
drop year quarter
format date %tq

//Save
save "AssetsLong.dta", replace

//Merging

merge 1:1 SNLInstitutionKey date using "LiabsLong.dta" //need perfect match

drop _merge

//A bit more cleaning
drop if Assets == .

bysort SNLInstitutionKey: gen last = _n == _N

gen fail = 1 if date != yq(2015,2) & last == 1

gen survDate = date - 162

gen Type2 = 1 if Type == "e"
replace Type2 = 0 if Type == "m"
drop Type
rename Type2 Type

gen Leverage = Liabilities/Assets


stset survDate, id(SNLInstitutionKey) failure(fail==1)

//Saving
save "REITclean.dta", replace



////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// 2 - Analysis ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


//On to the survival analysis

//Descriptive Statistics and Graphs

hist date if fail == 1, percent w(1)
stdescribe
stsum
egen PT_cat = group(Public Type)
sts graph, by(Type) hazard
sts graph, cumhaz
sts graph, by(Public Type) survival
sts list, survival
sts test Public Type



//log variables, this is the way it should actually be done, think about a 
// change in a % of liabilities, assets, or leverage rather than 1 unit.
gen lLiab = ln(Liabilities)
gen lAss = ln(Assets)
gen lLev = ln(Leverage)

gen PT = Public*Type

stcox lLev 
stcox lLiab lAss
stcox lLev Public Type
stcox lLev lAss Public Type PT, vce(robust)
stcox lLiab lAss Public Type





bysort date: egen num = count(fail) if fail == 1
bysort date: egen numFails = max(num) if fail == 1
label define Type 1 "Equity" 0 "Morgtage"
label define Public 1 "Public" 0 "NonPublic"
label value Type Type
label value Public Public
graph bar numFails, over(date) by(Type Public)





/////////////////////////////SINGLE OBS/////////////////////////////////////////
// not as useful.


drop if DateEstablished == .

egen medAssets = median(Assets), by(SNLInstitutionKey)
egen medLiabs = median(Liabilities), by(SNLInstitutionKey)
egen medLev = median(Leverage), by(SNLInstitutionKey)

replace DateEstablished = qofd(DateEstablished)
format DateEstablished %tq

egen lifetime = max(date - DateEstablished), by(SNLInstitutionKey)

keep if last == 1

keep medAssets medLiabs medLev lifetime SNLInstitutionKey Public Type fail

stset lifetime, failure(fail)


stcox medLev

stcox medLiab medAssets

stcox medLev Public Type

stcox medLev medAssets Public Type

stcox medLiab medAssets Public Type





