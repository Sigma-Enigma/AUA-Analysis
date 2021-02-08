
# dataCleaning.R v0.0.1

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
  mutate(MEAN_OUTOFPOCKET = (MEAN_MEDICARE_PAYMENTS - MEAN_MEDICARE_REIMBURSEMENT) ) %>% 
  mutate(TOTAL_MEDICARE_PAYMENTS = (MEAN_MEDICARE_PAYMENTS * DISCHARGE_COUNT_SUM ) ) %>% 
  mutate(TOTAL_MEDICARE_REIMBURSEMENT = (MEAN_MEDICARE_REIMBURSEMENT * DISCHARGE_COUNT_SUM ) ) %>% 
  mutate(TOTAL_OUTOFPOCKET = (MEAN_MEDICARE_PAYMENTS - MEAN_MEDICARE_REIMBURSEMENT) * DISCHARGE_COUNT_SUM ) %>% 
  mutate(RATIO_OUTOFPOCKET_OVER_MEDICARE_PAYMENTS = ( (MEAN_OUTOFPOCKET) / MEAN_MEDICARE_PAYMENTS) ) %>%
  mutate(RATIO_MEDICARE_REIMBURSEMENT_OVER_MEDICARE_PAYMENTS = ( (MEAN_MEDICARE_REIMBURSEMENT) / MEAN_MEDICARE_PAYMENTS) )


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

# key variable descriptions:
# Average Covered Charges: The provider's average charge for services covered by Medicare for all discharges in the MS-DRG. These will vary from hospital to hospital because of differences in hospital charge structures.

# !!!!!!!!!! IMPORTANT !!!!!!!!!!
# note: after reading things later (in next 3 dozen lines), I believe the above value is not useful, as it has nothing to do with the real costs paid by Medicare. My understanding is the charges are a sort of antiquated system
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# mean_medicare_payments =   averaged_groupedby_MS-DRG-&-Provider( filter(total paid to provider ))


# After reading the documents I noticed there are differences betwen my data and the ones described in the document, at which point I realized they forwarded the 2020 documentation. I realized I may need to do more digging to ensure there were no significant differences.

# Furthermore, I realized the payment calculation process is quite complex and seems to intentionally pay different amounts to different hospitals, depending on a number of important factors. This suggested I must do more reading before I have any hope of making meaningful policy suggestions.

# Also, I was a bit confused about the medical finance terminology used for the CMS documentation so I did some googling and read this:
# https://www.fah.org/blog/words-matter-defining-hospital-charges-costs-and-payments-and-the-numbers-t


# normally if I had a team I would have done the following around this point:
# TODO : Ask team if the 2020 variables are similar to the 2018 methods. Otherwise I would need to fish for the original paper and cite it
# SOLVED : They are not. https://www.splitgraph.com/cms-gov/inpatient-prospective-payment-system-ipps-provider-yekz-wzdr
# TODO : Double check with team that this source is accurate, or find original paper for 2018 data to understand methods
# SOLVED: the below file has all the detailed breakdowns of the adjustment system in csv formats. 
# https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/AcuteInpatientPPS/Acute-Inpatient-Files-for-Download-Items/FY2018-Final-Rule-Correction-Notice-Files

# Above files are a bit long and highly technical, I am short on time so I attempted to find shorter summaries of the system

#  reading on DRG
# https://www.verywellhealth.com/how-does-a-drg-determine-how-much-a-hospital-gets-paid-1738874

# read on how DRG rates are used for PPS system
# https://oig.hhs.gov/oei/reports/oei-09-00-00200.pdf 
# note: above file is old 2001, but gives good historical context
# note: better updated version below but no time to re-read in full
# https://www.cms.gov/Outreach-and-Education/Medicare-Learning-Network-MLN/MLNMattersArticles/downloads/MM10273.pdf

# my summary of things that influrence total payments per provider per year (medicare_reimbursement variable)
# fixed things
# -DRG base rates [diagnostic related group] (set at start of fiscal year)
# -DRG base rate severity multiplier [the S in MS-DRG] (depends on patinet, but not relevant for us as we focus on one MS-DRG)

# variable things
# -number of patients that enter a hospital (not relevent because standardized cost per patient by averaging)
# -provider specific costs
# ----fixed costs per institution [equipment/rent] (PARTIALLY ACCOUNTED by ading metro variable; better proxies are regional cost-of-living, real estate indexes, and equipment/utility costs)
# ----labor costs per institution ( PARTIALLY ACCOUNTED for by adding metro variable; cities have higher wage index multiplers; consider using BLS/Census regional wage data as a better proxy)
# -is low income area? (UNACCOUNTED disproporionate share; add census data)
# -is cancer hospital (UNACCOUNTED or psychiatric/long-term-care/childrens/rehab)?
# -is teaching hospital? (UNACCOUNTED and confounded with the metro variable; merge with other tables to find this info)

# to do meaningful work I would need to pull some more data from open sources.
# I will begin by doing the minimum required work (tests of difference) and then add more to it if I have time to find the data.


## Next steps: 
# two policy questions come to mind
# After you account for each cost adjustment, do they match the actual incurred costs to providers? [fix system inefficiencies]
# Are there any bad incentives (gaming of system) that could be corrected for with policy decisions?

# Note: In order to see discrepencies between payments and real costs, I would need cost data from each provider. (Think about where you may find that for later open source projects). If I had the REAL costs to each provider (instead of "charges") I could possibly make some substantive policy suggestions.


########## END PRE-ANALYSIS NOTES ########## 



########## BEGIN ANALYSIS ########## 

# Goals:
# test difference of groups (by metro status)
# test differences in group while accounting for heirarcical structure (by metro status AND using HRR as sub-groups)
# ANOVA 


#### BEGIN EDA ####

hist(kidneyUrinaryMccData_2018_metro$MEAN_MEDICARE_PAYMENTS, breaks = 40)

hist(kidneyUrinaryMccData_2018_nonmetro$MEAN_MEDICARE_PAYMENTS, breaks = 20)

table(kidneyUrinaryMccData_2018$HRR_DESC)
table(kidneyUrinaryMccData_2018$FACILITY_CITY)
table(kidneyUrinaryMccData_2018$FACILITY_ZIP_CODE)

#### END EDA ####

# required analysis

# F-test of difference of variances for two samples
var.test(kidneyUrinaryMccData_2018_metro$MEAN_COVERED_CHARGES, kidneyUrinaryMccData_2018_nonmetro$MEAN_COVERED_CHARGES)
# Welch's t-test of diffirence of means for two samples with unequal variances
t.test(kidneyUrinaryMccData_2018_metro$MEAN_COVERED_CHARGES, kidneyUrinaryMccData_2018_nonmetro$MEAN_COVERED_CHARGES)

# metro regins have double the average charge rate, and double the std_dev charge rate

########## END ANALYSIS ########## 


# extra analysis
# F-test of difference of variances for two samples
var.test(kidneyUrinaryMccData_2018_metro$MEAN_OUTOFPOCKET, kidneyUrinaryMccData_2018_nonmetro$MEAN_OUTOFPOCKET)
# Welch's t-test of diffirence of means for two samples with unequal variances
t.test(kidneyUrinaryMccData_2018_metro$MEAN_OUTOFPOCKET, kidneyUrinaryMccData_2018_nonmetro$MEAN_OUTOFPOCKET)

# why are these values so different? shouldn't the payment be standardized (to a ceiling) by medicare?

######### BEGIN VISUALIZATION ######### 


######### END VISUALIZATION ######### 





######### BEGIN SUMMARY NOTES ON DATA ######### 

# Methodology:
# see pg 7-9: https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Methodology.pdf

# overview: Data consists of aggregations of financial data (insurance charges) grouped by Medicare-Severity-Diagnosis-Related-Groups (MS-DRG) and medical facility. In other words, each row function as the totals of a-la-carte "health-products" that healthcare workers of a specific facility provide. The primary data source for these data is CMS’s Medicare Provider Analysis and Review (MEDPAR) inpatient data based on fiscal year (October 1st through September 30th). Over 3,000 hospitals that get medicare/medicaid patients (FFS) contributed to this data.

# Each row is an aggregate column for each provider x MS-DRG 
# Any aggregation with fewer than 10 patients, is excluded from analysis
# -- aside: This implies there might be an interesting project where you look at patterns in missing data (low incidence rates) and plot it geographically to see if there are any interesting patterns (when adjusting for *SUSCEPTIBLE* population) with relation to other key variables (may need to use PCA/regularized regression to reduce number of features if using massive data-join/data-shotgun approach)
# -- second aside: use HRR (hospital referral region) to plot the various regions geographically and observe spatial relations.

# discharge_count_sum = sum of patients discharged from facility in that year

# example DRG based payment calculation
# https://en.wikipedia.org/wiki/Diagnosis-related_group#Example_calculation
# medicare_payment = hospital_base_pay_rate * DRG_specific_weight
# hospital_base_pay_rate = labor_portion + nonlabor_portion
# labor_portion = constant * regional_wage_index_multiplier
# nonlabor_portion = constant_based_on_equipment +/- cost_of_living_adjustment {except for alaska and hawaii states} 


######### END SUMMARY NOTES ON DATA ######### 


######### BEGIN OTHER NOTES ######### 

# OpenPayments dataset 2018: https://www.cms.gov/OpenPayments/Explore-the-Data/Dataset-Downloads
# direct download: https://download.cms.gov/openpayments/PGYR18_P012221.ZIP

# OpenPayments documentation 2020
# https://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf


# refer to below for data collection information (for writeup)
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Methodology.pdf

# articles on DRG and how hospitals get paid
# https://www.google.com/search?q=relative+weight+ms-drg&oq=relative+weight+ms&aqs=chrome.1.69i57j0i22i30l2j0i10i22i30j0i390l2.5046j0j4&sourceid=chrome&ie=UTF-8
# https://acphospitalist.org/archives/2019/05/coding-corner-the-abcs-of-drgs.htm#:~:text=Over%2014%2C000%20ICD%2D10%2DCM,cost%20of%20care%20during%20hospitalization.
# https://www.verywellhealth.com/how-does-a-drg-determine-how-much-a-hospital-gets-paid-1738874

# documentation on how DRG rates are calculated (PPS)
# https://oig.hhs.gov/oei/reports/oei-09-00-00200.pdf

# find additional geographic data on each HRR to see differences by various demographic groups

######### END OTHER NOTES ######### 


# TODO : Sun afternoon
# finish up visuals in gg_plot (for tests of difference)
# finish write up
# clean up code to make simplified answer
# include alternative detailed answer in google drive file as well (to show other abilities)
# break up code into components

# TODO : Sun night to Fri night
# add geospatial branch to github
# build shiny app on top of geospatial data
# deploy app to AWS
# send link to ASA team
