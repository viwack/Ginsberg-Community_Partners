# Data Processing/Manip


library(leaflet) #mapping package
library(shiny) #builds web application
library(bslib) #interactive widgets
library(readr) #helps read the files
library(stringr) #supports with cleaning the files
library(sf) #dataframe objects
library(dplyr) #data manip
library(ggplot2) #data viz
library(scales) #supports data viz
library(shinydashboard) #supports web UI
library(htmltools) #when building the website makes it easier
library(tigris) #contains geographical information
library(readxl)
library(tidyr)
library(tidygeocoder)

# Read in the Community Partner Network
accounts_with_categories <- read_xlsx("Mapping CP Network.xlsx", sheet = "Accounts with categories")
accounts_with_categories$`Billing Zip/Postal Code` <- as.integer(accounts_with_categories$`Billing Zip/Postal Code`)

# mhvillage_df$Sites <- as.integer(mhvillage_df$Sites)
# pivot wider accounts w/ categories

#Remove unique IDs that would mess up with the Unique-ness of Account_ID
accounts_cat_select <- accounts_with_categories %>%
  select(-(`Category: Record ID`),-(`Account Category: Account Categories Name`))

acc_cat_wide <- accounts_cat_select %>%
  filter(!is.na(`Category: Category Name`)) %>%
  group_by(`Account Name`) %>%
  mutate(cat_num = paste0("Category ", row_number())) %>%
  ungroup() %>%
  pivot_wider(
    names_from  = cat_num,
    values_from = `Category: Category Name`
  )

# Create csv file to start off. Accounts are currently missing Matches.
write.csv(acc_cat_wide, "Accounts with Categories Wide.csv")

## NEXT STEPS: Add Matches/Initiatives, 
## Combine Address Columns and then GEOSM them for Coordinates
acc_cat_wide <- acc_cat_wide %>%
  mutate(Full_Address = paste0(`Billing Address Line 1`, ", ",
                              `Billing Address Line 2`, ", ",
                              `Billing City`, ", ",
                              `Billing State/Province`, ", ",
                              `Billing Zip/Postal Code`))

# Commented out as this takes forever and only needs to be ran once
acc_cat_geo <- acc_cat_wide %>%
  geocode(
    address = Full_Address,
    method = "arcgis",
    lat = latitude,
    long = longitude
  )

write.csv(acc_cat_geo, "Accounts_wideCategories_Geocoded.csv")
