## Analysis Plan and Project Code

### Estimate baseline racial disproportionality in child welfare referrals and acceptances from July 2014 to June 2017.

1. Using data from the most recent [American Community Survey 5-year estimates (2012-2016)](https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t), generate population estimates for children in Charlottesville by race and calculate proportions by race, incorporating the uncertainty represented by the 90% margins of error. See [`prep_basedisp.R`](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataprep/prep_basedisp.R) for details. The detailed tables in the 5-year estimates were estimates for sex by age for
  * The combined population (B01001)
  * White alone population (B01001A)
  * Black or African-American alone population (B01001B)
  * American Indian and Alaska Native alone population (B01001C)
  * Asian alone population (B01001D)
  * Native Hawaiian and Other Pacific Islander alone population (B01001E)
  * Some other race alone population (B01001F)
  * Two or more races population (B01001G)
  
2. Using the CWS referral data from June 2014-July 2016 from DSS, generate counts of children referred to CWS by race for each year (with years defined as June-July). Calculate proportions by race. See [`prep_basedisp.R`](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataprep/prep_basedisp.R) for details.

3. Generate visualization of proportions of children by race in Charlottesville and proportions of children referred to CWS (see [`analysis_basedisp.R`](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/analysis_basedisp.R)).

4. Calculate racial disproportionatilty by race, the ratio of % of [racial cateogry] children referred to CWS to % of [racial category] children in population (e.g., % referred to CWS who are white/% children in Cville who are white). Generate point estimates and estimates for lower and upper bounds of estimated population proportions (see [`analysis_basedisp.R`](https://github.com/datafordemocracy/PublicInterestData2018/blob/master/code/dataanalysis/analysis_basedisp.R)).
  
### Estimate racial disparity in post-referral decision points, modeling decision outcomes as a function of race and other characteristics.

1. Reporter relation -- racial proportions by reporter relation
2. Screened in -- referred children screened in (accepted as a case) as a function of race and covariates
3. Investigation -- accepted/screened in children assigned to investigation or assessment (alternative response) as a function of race and covaraites
4. Substantiation -- outcome of investigations for investigated cases as a function of race and covariates
5. Time to contact -- length of time between referral and contact by DSS for accepted cases

### Estimate racial disparity in foster care outcomes, modeling outcomes as a function of race and other characteristics.

1. Initial placement type -- among children placed in out-of-home care, type of placement by race and other characteristics
2. Number of placements -- among children placed in out-of-home care, number of placements by race and other characteristics
3. Time in care -- among children placed in out-of-home care, duration of time in care by race and other characteristics
4. Exit  -- among children placed in out-of-home care more than a year prior to June 30, 2018, exit from the system within year by race and other characteristics
5. Nature of exit -- among children in out-of-home care who have exited, how/why they exit by race and other characteristics
