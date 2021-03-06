###############################################################################
# Annualizing Zillow & Cleaning
# Vaasavi Unnava
# 
# Zillow's data is not in real 2016 US dollars, which is what the rest of our
# dataset uses. So we have to deflate the prices. Since our census data is on
# an annual level, we want to take the simple means of our monthly property 
# data to annualize it so that we have the appropriate census data to input 
# into the model.
# 
# I'll also be appending the county adjacency data onto this dataset as well.
# 
###############################################################################

# intializing the environment
rm(list = ls())

library(data.table)
library(stringr)
library(dplyr)
library(tidyr)

home_dir <- "/Users/vunnava/Dropbox/FRB/PSS Summer School/final project/"

# First import OECD CPI data

setwd(paste0(home_dir, "/data/input"))

cpi <- fread("oecd_cpi.csv") %>%
  rename(date = DATE, cpi_2010 = CPALTT01USM661S) %>%
  filter(year(cpi$date) > 2010, year(cpi$date) <= 2016) 

cpi$cpi_2010 <- as.numeric(cpi$cpi_2010)

cpi$cpi_2016 <- cpi$cpi_2010/(cpi[cpi$date == as.Date("2016-01-01"),]$cpi_2010)

# CPI's been normalized to 2016 $. Now let's make the zillow data in real 2016 $

setwd(paste0(home_dir, "/data/output"))

zillow_home_dat <- fread("zillow_homes_with_FIPS.csv")
zillow_home_dat <- zillow_home_dat[,1:71]

# we want to loop through the each month that Zillow has and divide it by the 
# relevant CPI scale for that month. 
# because looping hurts and would take forever, perform matrix operation. we know
# matrices are actually just vectors, so I treat the matrix of property values
# as one long vector to be divided by the cpi for each obs

cpi_repped <- rep(cpi$cpi_2016, nrow(zillow_home_dat))

zillow_home_dat[,12:ncol(zillow_home_dat)] <- zillow_home_dat[,12:ncol(zillow_home_dat)]/cpi_repped

# finally, time to annualize data.

# holding id data I don't want to mess around with right now
id_data <- zillow_home_dat[,1:11]

# getting just property value data
zillow_raw <- zillow_home_dat[,11:length(zillow_home_dat)]

# turn to long from wide so I can get yearly averages
zillow_long <- gather(zillow_raw, month, property_value, 2:length(zillow_raw))

zillow_long <- zillow_long %>% 
  mutate(year = substr(month, 1,4),
         year = as.numeric(year)) %>%
  group_by(SizeRank, year) %>%
  summarise(property_value = mean(property_value, na.rm = T)) %>%
  filter(year < 2016)

zillow_wide <- zillow_long %>% spread(year, property_value)

# merge back onto original ID data

home_annualized <- inner_join(id_data, zillow_wide, 
                                by = c("SizeRank" = "SizeRank")) %>%
  select(-c("V1"))

# making sure that leading zeroes stay in the FIPS codes
home_annualized$CountyFIPS <- home_annualized$CountyFIPS %>% as.character()
home_annualized$StateFIPS <- home_annualized$StateFIPS %>% as.character()
home_annualized$MunicipalCodeFIPS <- home_annualized$MunicipalCodeFIPS %>% 
  as.character()

home_annualized$CountyFIPS <- home_annualized$CountyFIPS %>%
  sapply(str_pad, width = 3, side = "left", pad = "0")
home_annualized$StateFIPS <- home_annualized$StateFIPS %>%
  sapply(str_pad, width = 2, side = "left", pad = "0")
home_annualized$MunicipalCodeFIPS <- home_annualized$MunicipalCodeFIPS %>%
  sapply(str_pad, width = 2, side = "left", pad = "0")
home_annualized$FIPS <- home_annualized$FIPS %>%
  sapply(str_pad, width = 5, side = "left", pad = "0")

# Again, but with rental data: 

zillow_rent_dat <- fread("zillow_rents_with_FIPS.csv")
zillow_rent_dat <- zillow_rent_dat[,1:71]

zillow_rent_dat[,12:ncol(zillow_rent_dat)] <- zillow_rent_dat[,12:ncol(zillow_rent_dat)]/cpi_repped

id_data <- zillow_rent_dat[,1:11]

zillow_raw <- zillow_rent_dat[,11:length(zillow_rent_dat)]

zillow_long <- gather(zillow_raw, month, rental_value, 2:length(zillow_raw))

zillow_long <- zillow_long %>% 
  mutate(year = substr(month, 1,4),
         year = as.numeric(year)) %>%
  group_by(SizeRank, year) %>%
  summarise(rental_value = mean(rental_value, na.rm = T)) %>%
  filter(year < 2016)

zillow_wide <- zillow_long %>% spread(year, property_value)

rent_annualized <- inner_join(id_data, zillow_wide, 
                                by = c("SizeRank" = "SizeRank")) %>%
  select(-c("V1"))

rent_annualized$CountyFIPS <- rent_annualized$CountyFIPS %>% as.character()
rent_annualized$StateFIPS <- rent_annualized$StateFIPS %>% as.character()
rent_annualized$MunicipalCodeFIPS <- rent_annualized$MunicipalCodeFIPS %>% 
  as.character()

rent_annualized$CountyFIPS <- rent_annualized$CountyFIPS %>%
  sapply(str_pad, width = 3, side = "left", pad = "0")
rent_annualized$StateFIPS <- rent_annualized$StateFIPS %>%
  sapply(str_pad, width = 2, side = "left", pad = "0")
rent_annualized$MunicipalCodeFIPS <- rent_annualized$MunicipalCodeFIPS %>%
  sapply(str_pad, width = 2, side = "left", pad = "0")
rent_annualized$FIPS <- rent_annualized$FIPS %>%
  sapply(str_pad, width = 5, side = "left", pad = "0")

# combining the two datasets:

rent_annualized$tag <- "rent"
home_annualized$tag <- "home"

zillow_annualized <- bind_rows(home_annualized, rent_annualized)

# size rank will vary b/w datasets & we don't need it, so let's get it out of here
zillow_annualized <- zillow_annualized %>% select(-c("SizeRank"))
zillow_annualized <- zillow_annualized[,c(1:9, length(zillow_annualized),
                                          10:(length(zillow_annualized)-1))]

zillow_prop_annualized <- zillow_annualized %>% gather(year, val, 
                                                       11:length(zillow_annualized)) %>%
  spread(tag, val) %>%
  filter(year > 2011) %>%
  rename(home_val = home, rent_val = rent)

zillow_delta_annualized <- zillow_annualized %>% gather(year, val, 
                                                        11:length(zillow_annualized)) %>%
  group_by(FIPS, tag) %>%
  mutate(d_val = (val-lag(val))/lag(val)) %>%
  select(-val) %>%
  filter(year > 2011) %>%
  spread(tag, d_val) %>%
  rename(home_d_val = home, rent_d_val = rent)

zillow_annualized <- full_join(zillow_prop_annualized, zillow_delta_annualized)

# WE DID IT YEAH LET'S ALL GO HAVE A PARTY

# saving the data....

setwd(paste0(home_dir, "/data/output"))

write.csv(zillow_annualized, "zillow_annualized.csv")