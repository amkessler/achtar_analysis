library(tidyverse)

combined <- readRDS("combined_npidetails_withcost.rds")


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


#payments percentiles, sum of PRESCRIPTIONS they wrote -- THIS IS PARTICULARLY STRIKING
combined %>% 
  group_by(percentile.payments) %>% 
  summarise(sum.of.claims = sum(rxclaims, na.rm = TRUE)) 




