library(tidyverse)

acthar_pay <- read_csv("acthar_pay.csv")

acthar_rx <- read_csv("acthar_rx_2015only.csv")


#group total payments by each npi
bynpi_payments <- acthar_pay %>% 
  group_by(npi) %>% 
  summarise(sum(total_amount_of_payment_usdollars))
colnames(bynpi_payments) <- c("npi", "payments")

#group total prescriptions by each npi, as well as dollar amounts of those prescriptions
bynpi_prescriptions <- acthar_rx %>% 
  group_by(NPI) %>% 
  summarise(sum(TOTAL_CLAIM_COUNT), 
            sum(TOTAL_DRUG_COST))
colnames(bynpi_prescriptions) <- c("npi", "rxclaims", "rxclaims.drugcost")

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

# View(combined)


#### ADDING NPI INFO BY JOINING NPI LOOKUP TABLES ----

npi_lookup <- readRDS("npi_lookup.rds")

#join to npi lookup data
combined_npi_join1 <- left_join(combined, npi_lookup)
combined_npi_join1$taxonomy_code <- combined_npi_join1$healthcare_provider_taxonomy_code_1

npi_taxonomycodes <- readRDS("npi_taxonomycodes.rds")

#join to taxonomy codes
combined_npidetails <- left_join(combined_npi_join1, npi_taxonomycodes)

#select only relevant columns
combined_npidetails <- combined_npidetails %>% 
  select(c(1:6,8:12,29:35,38:41))

colnames(combined_npidetails) <- c(
  "npi",                                                         
  "payments",                                                    
  "rxclaims",     
  "rxclaims.drugcost",
  "percentile.payments",                                         
  "percentile.rxclaims",  
  "last.name",                                          
  "first.name",                                         
  "middle.name",                                        
  "prefix",                                   
  "suffix",                                   
  "practice.address",     
  "practice,city",       
  "practice.state",      
  "practice.zip",     
  "practice.countycode",    
  "practice.phone",
  "gender.code",                                        
  "taxonomy.code",                                               
  "taxonomy.description",                                        
  "taxonomy.classification",                                              
  "taxonomy.specialization"  
)

head(combined_npidetails)

saveRDS(combined_npidetails, "combined_npidetails_withcost.rds")

write.csv(combined_npidetails, "combined_npidetails_withcost.csv")

