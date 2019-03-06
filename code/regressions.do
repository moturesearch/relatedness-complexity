* REGRESSIONS.DO
*
* This script tabulates our main and subsample regression results, and exports
* the transformed data used in our analysis.
*
* Ben Davies
* August 2018


** INITIALISATION

* Define globals
global DIR "${PROJECTS}/relatedness-complexity"
global DATA "${DIR}/data"
global TABS "${DIR}/tables/regressions"

* Open log
set linesize 160
log using "${DIR}/logs/regressions.log", replace

* Prepare for battle
set matsize 5000


** DATA IMPORT

* Import mixed pair attributes
use "${DATA}/observations/mixed-pairs", clear
rename local_share LS_acy
rename location_quotient LQ_acy
rename mean_local_relatedness MLR_acy
rename size E_acy
bysort year: egen E_y = total(E_acy)

* Import city attributes
merge m:1 ua year using "${DATA}/observations/cities", nogen
rename ua_complexity C_cy
rename ua_size E_cy

* Import activity attributes
merge m:1 act year using "${DATA}/observations/activities", nogen
rename act_complexity C_ay
rename act_size E_ay


** WEIGHTED AND TRANSFORMED REGRESSION DATA

* Drop residual activities
drop if ind == "XX"

* Create city and activity dummies
egen a = group(act)
egen c = group(ua)
egen ac = group(act ua)

* Identify time variation and declare panel
drop if inlist(year, 1986, 1996, 2006)  // Use (approx.) decadal data
egen y = group(year)
egen ay = group(act year)
egen cy = group(ua year)
xtset ac y

* Identify complex and simple activities
egen max_C_ay = max(C_ay), by(act)
egen min_C_ay = min(C_ay), by(act)
gen complex_act = min_C_ay > 0
gen simple_act = max_C_ay < 0

* Identify complex and simple cities
egen max_C_cy = max(C_cy), by(ua)
egen min_C_cy = min(C_cy), by(ua)
gen complex_ua = min_C_cy > 0
gen simple_ua = max_C_cy < 0

* Compute growth rates and relatedness density
gen G_acy = 100 * ((E_acy / L.E_acy) ^ (1 / (year - L.year)) - 1)
gen RD_acy = MLR_acy - LS_acy

* Generate lagged values
foreach v in LS_acy RD_acy C_cy C_ay LQ_acy {
	gen `v'_L1 = L.`v'
}

* Initialise weighted data
gen W_acy = L.E_acy / L.E_y
global WT [aw = W_acy]
drop if missing(G_acy)

* Define program for performing within-data weighted transformations
capture program drop transform
program define transform
	syntax [if]
	
	* Demean local shares and convert from decimals to percentage points
	capture drop LS_acy_L1_dm 
	summ LS_acy_L1 ${WT} `if'
	gen LS_acy_L1_dm = 100 * (LS_acy_L1 - r(mean))

	* Perform within-data standardisations
	foreach v in RD_acy C_cy C_ay {
		capture drop `v'_L1_std 
		summ `v'_L1 ${WT} `if'
		gen `v'_L1_std = (`v'_L1 - r(mean)) / r(sd)
	}

	* Generate interaction terms
	capture drop LSxC_?y RDxC_?y
	gen LSxC_ay = LS_acy_L1_dm * C_ay_L1_std
	gen LSxC_cy = LS_acy_L1_dm * C_cy_L1_std
	gen RDxC_ay = RD_acy_L1_std * C_ay_L1_std
	gen RDxC_cy = RD_acy_L1_std * C_cy_L1_std
end

* Perform weighted transformations
transform

* Print descriptive statistics for transformed data
global VARS E_acy G* *_dm *_std
summ ${VARS}
summ ${VARS} ${WT}

* Print weighted correlation matrix
corr G_acy LS_acy_L1_dm RD_acy_L1_std C_ay_L1_std C_cy_L1_std ${WT}


** MAIN REGRESSIONS

* Generate regression table
eststo clear
eststo: qui reg G_acy LS_acy_L1_dm RD_acy_L1_std C_ay_L1_std LSxC_ay RDxC_ay ${WT}, robust
eststo: qui reg G_acy LS_acy_L1_dm RD_acy_L1_std C_ay_L1_std LSxC_ay RDxC_ay C_cy_L1_std LSxC_cy RDxC_cy ${WT}, robust
eststo: qui areg G_acy LS_acy_L1_dm RD_acy_L1_std LSxC_cy RDxC_cy LSxC_ay RDxC_ay i.cy ${WT}, a(ay) robust
estadd local ay_fe "Yes", replace
esttab, b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs"))
esttab using "${TABS}/full.csv", b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs")) replace

* Define preferred model specification
global SPEC qui areg G_acy LS_acy_L1_dm RD_acy_L1_std LSxC_ay RDxC_ay LSxC_cy RDxC_cy i.cy ${WT}


** COMPLEXITY SUBSAMPLE REGRESSIONS

* Tabulate complexity subsamples
table act year if complex_act, c(count G_acy) row col
table act year if simple_act, c(count G_acy) row col
table ua year if complex_ua, c(count G_acy) row col
table ua year if simple_ua, c(count G_acy) row col

* Clear regression table
eststo clear

* Regress on full data
transform
eststo: ${SPEC}, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on complex activities
transform if complex_act
eststo: ${SPEC} if complex_act, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on simple activities
transform if simple_act
eststo: ${SPEC} if simple_act, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on other activities
transform if !complex_act & !simple_act
eststo: ${SPEC} if !complex_act & !simple_act, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on complex cities
transform if complex_ua
eststo: ${SPEC} if complex_ua, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on simple cities
transform if simple_ua
eststo: ${SPEC} if simple_ua, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on other cities
transform if !complex_ua & !simple_ua
eststo: ${SPEC} if !complex_ua & !simple_ua, a(ay) robust
estadd local ay_fe "Yes", replace

* Generate regression table
esttab, b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs")) mtitles("Full" "Comp acts" "Simp acts" "Other acts" "Comp UAs" "Simp UAs" "Other UAs")
esttab using "${TABS}/complexity.csv", b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs")) mtitles("Full" "Comp acts" "Simp acts" "Other acts" "Comp UAs" "Simp UAs" "Other UAs") replace


** OVER-REPRESENTATION SUBSAMPLE REGRESSIONS

* Identify over-representation subsamples
gen overrep = cond(LQ_acy_L1 >= 1, 1, 0)
table year overrep, c(count G_acy) row col
table ua year if overrep, c(count G_acy) row col
table ind year if overrep, c(count G_acy) row col
table occ year if overrep, c(count G_acy) row col

* Clear regression table
eststo clear

* Regress on full data
transform
eststo: ${SPEC}, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on over-represented activities
transform if overrep == 1
eststo: ${SPEC} if overrep == 1, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on under-represented activities
transform if overrep == 0
eststo: ${SPEC} if overrep == 0, a(ay) robust
estadd local ay_fe "Yes", replace

* Generate regression table
esttab, b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs")) mtitles("Full" "L.RCA = 1" "L.RCA = 0")
esttab using "${TABS}/overrep.csv", b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs")) mtitles("Full" "L.RCA = 1" "L.RCA = 0") replace


** URBAN AREA TYPE SUBSAMPLE REGRESSIONS

* Identify urban area type subsamples
gen ua_type = cond(ua <= 100, "main", cond(ua <= 200, "secondary", "minor"))
replace ua_type = "auckland" if inlist(ua, 2, 3, 4, 5)
table ua_type year, c(count G_acy) row col

* Clear regression table
eststo clear

* Regress on full data
transform
eststo: ${SPEC}, a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on Auckland
transform if ua_type == "auckland"
eststo: ${SPEC} if ua_type == "auckland", a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on main UAs
transform if ua_type == "main"
eststo: ${SPEC} if ua_type == "main", a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on secondary UAs
transform if ua_type == "secondary"
eststo: ${SPEC} if ua_type == "secondary", a(ay) robust
estadd local ay_fe "Yes", replace

* Regress on minor UAs
transform if ua_type == "minor"
eststo: ${SPEC} if ua_type == "minor", a(ay) robust
estadd local ay_fe "Yes", replace

* Generate regression table
esttab, b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs")) mtitles("Full" "AKL" "Main eAKL" "Sec" "Minor")
esttab using "${TABS}/urban-area.csv", b(3) se r2 indicate("cy FEs = *.cy") s(ay_fe N r2, label("ay FEs" "Obs")) mtitles("Full" "AKL" "Main eAKL" "Sec" "Minor") replace


** SUBSAMPLE DESCRIPTIVE STATS

* Initialise matrix
matrix mat = J(26, 5, .)
matrix rownames mat = full . complex_activity . simple_activity . other_activity . overrep . underrep . complex_city . simple_city . other_city . auckland . main . secondary . minor .
matrix colnames mat = G_acy LS_acy_L1_dm RD_acy_L1_std C_ay_L1_std C_cy_L1_std
transform

* Generate descriptive statistics by subsample
local c = 1
foreach v in G_acy LS_acy_L1_dm RD_acy_L1_std C_ay_L1_std C_cy_L1_std {

	qui summ `v' ${WT}
	matrix mat[1, `c'] = r(mean)
	matrix mat[2, `c'] = r(sd)
	
	qui summ `v' ${WT} if complex_act
	matrix mat[3, `c'] = r(mean)
	matrix mat[4, `c'] = r(sd)
	
	qui summ `v' ${WT} if simple_act
	matrix mat[5, `c'] = r(mean)
	matrix mat[6, `c'] = r(sd)
	
	qui summ `v' ${WT} if !complex_act & !simple_act
	matrix mat[7, `c'] = r(mean)
	matrix mat[8, `c'] = r(sd)
	
	qui summ `v' ${WT} if overrep == 1
	matrix mat[9, `c'] = r(mean)
	matrix mat[10, `c'] = r(sd)
	
	qui summ `v' ${WT} if overrep == 0
	matrix mat[11, `c'] = r(mean)
	matrix mat[12, `c'] = r(sd)
	
	qui summ `v' ${WT} if complex_ua
	matrix mat[13, `c'] = r(mean)
	matrix mat[14, `c'] = r(sd)
	
	qui summ `v' ${WT} if simple_ua
	matrix mat[15, `c'] = r(mean)
	matrix mat[16, `c'] = r(sd)
	
	qui summ `v' ${WT} if !complex_ua & !simple_ua
	matrix mat[17, `c'] = r(mean)
	matrix mat[18, `c'] = r(sd)
	
	qui summ `v' ${WT} if ua_type == "auckland"
	matrix mat[19, `c'] = r(mean)
	matrix mat[20, `c'] = r(sd)
	
	qui summ `v' ${WT} if ua_type == "main"
	matrix mat[21, `c'] = r(mean)
	matrix mat[22, `c'] = r(sd)
	
	qui summ `v' ${WT} if ua_type == "secondary"
	matrix mat[23, `c'] = r(mean)
	matrix mat[24, `c'] = r(sd)
	
	qui summ `v' ${WT} if ua_type == "minor"
	matrix mat[25, `c'] = r(mean)
	matrix mat[26, `c'] = r(sd)
	
	local c = `c' + 1
}
matrix list mat


** POSTSCRIPT

* Export data
eststo clear
drop max* min*
save "${DATA}/regressions", replace
export delimited "${DATA}/regressions.csv", replace

* Close log
log close
