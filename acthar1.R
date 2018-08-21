library(tidyverse)

acthar_pay <- read_csv("acthar_pay.csv")

acthar_rx <- read_csv("acthar_rx.csv")


#group total payments by each npi
bynpi_payments <- acthar_pay %>% 
  group_by(npi) %>% 
  summarise(sum(total_amount_of_payment_usdollars))
colnames(bynpi_payments) <- c("npi", "payments")

#group total prescriptions by each npi
bynpi_prescriptions <- acthar_rx %>% 
  group_by(NPI) %>% 
  summarise(sum(TOTAL_CLAIM_COUNT))
colnames(bynpi_prescriptions) <- c("npi", "rxclaims")

#count of records - thus doctors -- in rx table per year
acthar_rx %>% 
  group_by(YEAR) %>%
  tally()


#full join on the two tables to create a master npi lookup
#for payments and claims
combined <- full_join(bynpi_payments, bynpi_prescriptions)
combined$npi <- as.character(combined$npi)
combined <- filter(combined, !is.na(npi))


#### CALCULATING PERCENTILES ----

#percentiles
# quantile(combined$payments, na.rm = TRUE)
# quantile(combined$rxclaims, na.rm = TRUE)

#percentiles by tenths - deciles
qp <- quantile(combined$payments, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5)
qc <- unique(quantile(combined$rxclaims, na.rm = TRUE, prob = seq(0, 1, length = 11), type = 5))


#create new column tagging each record with percentiles
combined$percentile.payments <- cut(combined$payments, qp)
levels(combined$percentile.payments) <- c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%") 

combined$percentile.rxclaims <- cut(combined$rxclaims, qc)
levels(combined$percentile.rxclaims) <- c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%") 
#won't be a 0 percentile for results above since both 0 and 10 pctile were both value of 11 claims

#add logical columns for payment and rx true/false flags
combined$flag.payments <- ifelse(!is.na(combined$payments),TRUE,FALSE)
combined$flag.rxclaims <- ifelse(!is.na(combined$rxclaims),TRUE,FALSE)

View(combined)


#### ANALYSIS BY PERCENTILE GROUP ----

#payment percentiles - total payments summed
combined %>% 
  group_by(percentile.payments) %>% 
  summarise(sum(payments))

#prescription percentiles - total prescriptions summed
combined %>% 
  group_by(percentile.rxclaims) %>% 
  summarise(sum(rxclaims))


#prescriber percentiles, sum of PAYMENTS they received
combined %>% 
  group_by(percentile.rxclaims) %>% 
  summarise(sum.of.payments = sum(payments, na.rm = TRUE))


#payments percentiles, sum of PRESCRIPTIONS they wrote
combined %>% 
  group_by(percentile.payments) %>% 
  summarise(sum.of.claims = sum(rxclaims, na.rm = TRUE)) 

# write.csv(combined, "combined.csv")

### ANALYSIS BY YES/NO VARIABLES ----

combined %>% 
  group_by(percentile.rxclaims, flag.payments) %>% 
  summarise(claims = sum(rxclaims, na.rm = TRUE)) 



#### ADDING NPI INFO BY JOINING NPI LOOKUP TABLES ----

#***note: code to complete the joining stored in separate
#         file at acthar2_npijoindetail.R
combined_npidetails <- readRDS("combined_npidetails.rds")
head(combined_npidetails)
