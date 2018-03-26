## Linking clients across data sources

* We can link 116/117 out of 118 unique foster care cases since 7/1/2014 with referral data (using anonymized client id/using anonymized sssn)
* We can link 220/215 out of 313/282 unique ongoing clients since 7/1/2014 with referral data (using anonymized client id/using anonymized ssn)
* The ongoing cases to foster care cases don't effectively match up (only 14/15 cases)

## Conclusions

* Might be worth linking referral data to foster care data if the additional covariates it provides are of interest (tracts, level of findings from investigation)
* For analysis that uses ongoing cases as the primary data (time in the system, number of face to face interactions), might be worth linking referral data if the additional covariates are of interest (again, tract, assess/investigate?)

## Code to generate these counts

```
# Referrals to Active cases: client id
n_distinct(referral$client_id) # 1325 unique clients referred
referral %>% filter(accept == "Y") %>% summarize(n_distinct(client_id)) # 972 unique accepted referrals
n_distinct(active$`Client ID`) # 365 unique active clients
active %>% filter(`Case Type Begin Date` > as.Date("2014-06-30")) %>%
  summarize(n_distinct(`Client ID`)) # 313 unique active clients since 7/1/2014
sum(unique(active$`Client ID`) %in% unique(referral$client_id)) 
# 220 cases match (220 of 313 unique active clients since 7/1/2014 match referral)

# Referrals to Active cases: ssn
n_distinct(referral$ssn) # 1121 unique clients referred
n_distinct(active$SSN) # 332 unique active clients
active %>% filter(`Case Type Begin Date` > as.Date("2014-06-30")) %>%
  summarize(n_distinct(SSN)) # 282 unique active clients since 7/1/2014
sum(unique(active$SSN) %in% unique(referral$ssn)) 
# 215 cases match (215 of 282 unique active clients since 7/1/2014 match referral)

# Active cases to foster care caes: client id
n_distinct(active$`Client ID`) # 365 unique active clients
n_distinct(foster$CL_ID) # 223 unique foster care clients
foster %>% filter(`Enter Care Date` > as.Date("2014-06-30")) %>% 
  summarize(n_distinct(CL_ID)) # 118 unique foster care clients since 7/1/2014
sum(unique(foster$CL_ID) %in% unique(active$`Client ID`)) 
# 14 cases match

# Active cases to foster care caes: ssn
n_distinct(active$SSN) # 332 unique active clients
n_distinct(foster$SSN) # 222 unique foster care clients
foster %>% filter(`Enter Care Date` > as.Date("2014-06-30")) %>% 
  summarize(n_distinct(SSN)) # 118 unique foster care clients since 7/1/2014
sum(unique(foster$SSN) %in% unique(active$SSN)) 
# 15 cases match

# Referral cases to foster care cases: client id
n_distinct(referral$client_id) # 1325 unique clients referred
n_distinct(foster$CL_ID) # 223 unique foster care clients
foster %>% filter(`Enter Care Date` > as.Date("2014-06-30")) %>% 
  summarize(n_distinct(CL_ID)) # 118 unique foster care clients since 7/1/2014
sum(unique(foster$CL_ID) %in% unique(referral$client_id)) 
# 116 cases match (116 of 118 unique foster care clients since 7/1/14 match referral)

# Referral cases to foster care cases: ssn
n_distinct(referral$ssn) # 1121 unique clients referred
n_distinct(foster$SSN) # 222 unique foster care clients
foster %>% filter(`Enter Care Date` > as.Date("2014-06-30")) %>% 
  summarize(n_distinct(SSN)) # 118 unique foster care clients since 7/1/2014
sum(unique(foster$SSN) %in% unique(referral$ssn)) 
# 117 cases match (117 of 118 unique foster care clients match referral)
```
