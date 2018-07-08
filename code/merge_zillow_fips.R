###############################################################################
# MERGING ZILLOW/FIPS
# Vaasavi Unnava
# 
# Zillow has extensive property value data, and we're going to map those Zillow
# id's on the Census county id codes so that we can append some county level
# fixed effects data onto our dataset.
# 
# I'll also be appending the county adjacency data onto this dataset as well.
# 
###############################################################################

library(data.table)
library(tidyverse)

home_dir <- "/Users/vunnava/Dropbox/FRB/PSS Summer School/final project/"

# Getting data ready for merge ----
# Reading in the datasets

# had to subset the data for zillow, as it wasn't reading in all the columns 
# we needed o/w

zillow_home_dat <- fread(paste0(home_dir,
                           "/data/input/County_Zhvi_AllHomes_subsetted.csv"))
zillow_rent_dat <- fread(paste0(home_dir,
                                "/data/input/County_Zri_AllHomesPlusMultifamily_subsetted.csv"))

adjacencies <- fread(paste0(home_dir, 
                            "/data/input/county_adjacency.txt"))
crosswalk <- fread(paste0(home_dir, 
                          "/data/input/CountyCrosswalk_Zillow.csv"))

# Adding var names to our bb txt file

colnames(adjacencies) <- c("locale_name", "fips", 
                           "neighbor_name", "neighbor_fips")

# Merging crosswalk on zillow

zillow_homes_crossed <- right_join(crosswalk, zillow_home_dat, 
                             by = c("CountyRegionID_Zillow" = "RegionID"))

zillow_rent_crossed <- right_join(crosswalk, zillow_rent_dat, 
                                   by = c("CountyRegionID_Zillow" = "RegionID"))

# Cleaning up this crosswalked data

zillow_homes_crossed <- zillow_homes_crossed %>% select(-c(CountyRegionID_Zillow, 
                                               MetroRegionID_Zillow, 
                                               CBSACode,
                                               RegionName,
                                               State, StateCodeFIPS))

zillow_rent_crossed <- zillow_rent_crossed %>% select(-c(CountyRegionID_Zillow, 
                                                           MetroRegionID_Zillow, 
                                                           CBSACode,
                                                           RegionName,
                                                           State, StateCodeFIPS))

write.csv(zillow_homes_crossed, paste0(home_dir, "/data/output/zillow_homes_with_FIPS.csv"))
write.csv(zillow_rent_crossed, paste0(home_dir, "/data/output/zillow_rents_with_FIPS.csv"))