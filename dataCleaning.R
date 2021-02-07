# AUA Analysis

# library loads

library(data.table)
library(readxl)
library(dplyr)
library(stringr)

########## BEGIN DATA PREP ########## 


# remember to setwd("AUA_Analysis")

# data source: 
# table on larger side, so reading with data.table package
chargeData_2018 <- fread(file = "MEDICARE_PROVIDER_CHARGE_INPATIENT_DRGALL_FY2018.CSV") 

# data source: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/
# direct download: https://www.ers.usda.gov/webdocs/DataFiles/53241/RUCA2010zipcode.xlsx?v=3182.2
# note: metropolitan areas = RUCA1 with values [1,3]; and non-metropolitan = RUCA1 w/ values [4,10], omit values = 99
ruca3.1_zipCodes <- read_xlsx(path = "RUCA2010zipcode.xlsx", sheet = 2)

# convert character values to numeric
ruca3.1_zipCodes$ZIP_CODE <- as.integer(ruca3.1_zipCodes$ZIP_CODE)

# select important columns; filter out RUCA1 values == 99 (NA)
# TODO : redo analysis including 99 values in nonmetro group, or merging metro and micropolitan regions
ruca3.1_zipCodes <- ruca3.1_zipCodes %>%
  dplyr::select(ZIP_CODE, ZIP_TYPE, RUCA1, STATE) %>%
  dplyr::filter(RUCA1 != 99)

# merge tables by zip code; create new variable for metropolitan status T/F; filter out non NY state data
mergedData_2018 <- chargeData_2018 %>%
  left_join(ruca3.1_zipCodes, by = c("FACILITY_ZIP_CODE" = "ZIP_CODE") ) %>%
  mutate(METROPOLITAN = ( RUCA1 %in% 1:3 ) ) %>% 
  dplyr::filter(STATE_DESC == "NY" | STATE == "NY") %>% # to catch errors in chargeData state data entry (states wont match)
  mutate(MEAN_OUTOFPOCKET = (MEAN_MEDICARE_PAYMENTS - MEAN_MEDICARE_REIMBURSEMENT) ) %>% # total - medicare_reimbursement 
  mutate(MEAN_OUTOFPOCKET_PER_DISCHARGE = (MEAN_MEDICARE_PAYMENTS - MEAN_MEDICARE_REIMBURSEMENT)/ DISCHARGE_COUNT_SUM ) %>% 
  mutate(RATIO_MEAN_OUTOFPOCKET_OVER_MEAN_MEDICARE_PAYMENT = ( (MEAN_OUTOFPOCKET) / MEAN_MEDICARE_PAYMENTS) ) %>%
  mutate(RATIO_MEAN_REIMBURSEMENT_OVER_MEAN_PAYMENTS = ( (MEAN_OUTOFPOCKET) / MEAN_MEDICARE_PAYMENTS) )


# to double check that state desc matches zip code of state 
which(mergedData_2018$STATE_DESC != mergedData_2018$STATE) # no entry errors

# splitting drg_code and drg_desc into two columns
splitDRG_DESC <-  str_split_fixed( mergedData_2018$DRG_DESC, " - ", n = 2)
colnames(splitDRG_DESC) <- c("DRG_CODE", "DRG_DESC") # rename columns
row.names(splitDRG_DESC) <- 1:dim(splitDRG_DESC)[1]  # rename row id's
splitDRG_DESC <- as.data.frame(splitDRG_DESC) 
splitDRG_DESC$DRG_CODE <- as.integer(splitDRG_DESC$DRG_CODE)

# deleting duplicate DRG description column
mergedData_2018$DRG_DESC <- NULL

# concatonating data frames
mergedData_2018 <- bind_cols(splitDRG_DESC, mergedData_2018)

# removes extraneous data
rm(splitDRG_DESC)
rm(chargeData_2018)
rm(ruca3.1_zipCodes)

# see TODO 1 & 2 about DRG numbers after I read about CRM documentation
kidneyUrinaryMccData_2018 <- mergedData_2018 %>%
  filter(DRG_CODE == 346)


# second way to filter (to double check) as I noticed the data has some non-standardization text errors (some descriptions start or end with quotations)

# str_detect ouputs TRUE or FALSE if the DRG_DESC contains any of the quoted words; then the multiplication of binary values returns FALSE if any one of the str_detect's fail, thus removing rows missing any key words (and their variations given after the "|" operator)
kidneyUrinaryMccData_2018_cleaned <- mergedData_2018 %>%
  filter(
    str_detect(DRG_DESC, "KIDNEY|\"KIDNEY|\"\"KIDNEY")* # note:  \" is a special character that indicates a literal quotation mark
      str_detect(DRG_DESC, "URINARY" )*
      str_detect(DRG_DESC, "INFECTION|INFECTIONS" )*
      str_detect(DRG_DESC, "W MCC|W MCC\"|W MCC\"\"" ) == TRUE
  )

# tests that datatables are exact matches (should return 0)
sum( kidneyUrinaryMccData_2018 != kidneyUrinaryMccData_2018_cleaned) 

kidneyUrinaryMccData_2018 <- kidneyUrinaryMccData_2018_cleaned

rm(kidneyUrinaryMccData_2018_cleaned)

# split data into two groups

# metro
kidneyUrinaryMccData_2018_metro <- kidneyUrinaryMccData_2018 %>%
  filter(METROPOLITAN == TRUE)

# nonmetro
kidneyUrinaryMccData_2018_nonmetro <- kidneyUrinaryMccData_2018 %>%
  filter(METROPOLITAN == FALSE)


########## END DATA PREP ########## 





########## BEGIN PRE-ANAYSIS NOTES ########## 


# before doing any analysis I: 
# read CMS documentation on Inpatient PUF; https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Methodology.pdf
# read FAQ for Inpatient PUF ; https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Outpatient_FAQ.pdf

# TODO : Ask team if the 2020 variables are similar to the 2018 methods. Otherwise I would need to fish for the original paper and cite it
# SOLVED : They are not. https://www.splitgraph.com/cms-gov/inpatient-prospective-payment-system-ipps-provider-yekz-wzdr
# TODO : Double check with team that this source is accurate. 
# TODO : Also still need to find original paper for 2018 data to verify methods are not different as well.

# questions: were the DRG_DESC codes updated to reflect the codes in 2020, or do they reflect the codes at the time 2018? If so the values may need to be adjusted, (or at least visually inspected to further subset the data).
# TODO : check documentation for charge data regarding above comment. 
# Status: unclear

# used this DRG code as source: https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MedicareFeeforSvcPartsAB/downloads/DRGdesc08.pdf
# indicated the key DRG code = 689 KIDNEY AND URINARY TRACT INFECTIONS WITH MCC
# however I noticed the DRG code for KIDNEY & URINARY TRACT INFECTIONS W MCC is 346 in our data
# it appears that DRG is not the same as MS-DRG
# TODO : Normally if I had communication with my team, this is where I would ask why these values are different and ensure that the selected data is correct

# did some reading on health care finance terminology
# https://www.fah.org/blog/words-matter-defining-hospital-charges-costs-and-payments-and-the-numbers-t

# did some reading on DRG
# https://www.verywellhealth.com/how-does-a-drg-determine-how-much-a-hospital-gets-paid-1738874

# TODO : is mean_medicare_payments averaged across all providers? Or only across all discharged patients for each provider x MS-DRG group?


# mean_medicare_payments =   averaged_groupedby_MS-DRG-&-Provider( filter(total paid to provider ))



########## END PRE-ANALYSIS NOTES ########## 





########## BEGIN ANALYSIS ########## 

hist(kidneyUrinaryMccData_2018_metro$MEAN_MEDICARE_PAYMENTS, breaks = 40)

hist(kidneyUrinaryMccData_2018_nonmetro$MEAN_MEDICARE_PAYMENTS, breaks = 20)


########## END ANALYSIS ########## 





######### BEGIN VISUALIZATION ######### 


######### END VISUALIZATION ######### 





######### BEGIN SUMMARY NOTES ON DATA ######### 

# overview: Data consists of aggregations of financial data (insurance charges) grouped by Medicare-Severity-Diagnosis-Related-Groups (MS-DRG) and medical facility. In other words, each row function as the totals of a-la-carte "health-products" that healthcare workers of a specific facility provide. The primary data source for these data is CMS’s Medicare Provider Analysis and Review (MEDPAR) inpatient data based on fiscal year (October 1st through September 30th). Over 3,000 hospitals that get medicare/medicaid patients (FFS) contributed to this data.

# Each row is an aggregate column for each provider x MS-DRG 
# Any aggregation with fewer than 10 patients, is excluded from analysis
# -- aside: This implies there might be an interesting project where you look at patterns in missing data (low incidence rates) and plot it geographically to see if there are any interesting patterns (when adjusting for *SUSCEPTIBLE* population) with relation to other key variables (may need to use PCA/regularized regression to reduce number of features if using massive data-join/data-shotgun approach)
# -- second aside: use HRR (hospital referral region) to plot the various regions geographically and observe spatial relations.

# discharge_count_sum = sum of patients discharged from facility in that year

# Methodology:
# see pg 7-9: https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Methodology.pdf

# 1) Limit CMS MedPAR discharge data to fee-for-service short-stay hospital discharges associated with ‘IPPS’ hospitals using the following criteria:




######### END SUMMARY NOTES ON DATA ######### 


######### BEGIN OTHER NOTES ######### 

# OpenPayments dataset 2018: https://www.cms.gov/OpenPayments/Explore-the-Data/Dataset-Downloads
# direct download: https://download.cms.gov/openpayments/PGYR18_P012221.ZIP

# OpenPayments documentation 2020
# https://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf


# refer to below for data collection information (for writeup)
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Methodology.pdf

# find additional geographic data on each HRR to see differences by various demographic groups

######### END OTHER NOTES ######### 
