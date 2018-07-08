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

zillow_dat <- fread(paste0(home_dir,
                           "/data/input/County_Zhvi_AllHomes_subsetted.csv"))
adjacencies <- fread(paste0(home_dir, 
                            "/data/input/county_adjacency.txt"))
crosswalk <- fread(paste0(home_dir, 
                          "/data/input/CountyCrosswalk_Zillow.csv"))

# Adding var names to our bb txt file

colnames(adjacencies) <- c("locale_name", "fips", 
                           "neighbor_name", "neighbor_fips")

# Merging crosswalk on zillow

zillow_crossed <- right_join(crosswalk, zillow_dat, 
                             by = c("CountyRegionID_Zillow" = "RegionID"))

# Cleaning up this crosswalked data

zillow_crossed <- zillow_crossed %>% select(-c(CountyRegionID_Zillow, 
                                               MetroRegionID_Zillow, 
                                               CBSACode,
                                               RegionName,
                                               State, StateCodeFIPS))

write.csv(zillow_crossed, paste0(home_dir, "/data/output/zillow_with_FIPS.csv"))
