library(tidyverse)
library(openintro)

sen_2022 <- read.csv("senate_2022.csv")
sen_general <- read.csv("1976-2020-senate.csv")
house_general <- read.csv("1976-2022-house (1).csv")
pres_general <- read.csv("countypres_2000-2020.csv")

# Presidential -- Mia

view(pres_general)

# keep year, state name, state abb, county_name, office, party, candidatevotes, totalvotes
# make 2 df: one with state wide results, one with county results (pres_county, pres_states)
# pres_states: sum vote share data

# House

view(house_general)

# filter to be 2000-2022
# keep year, state name, state abb, office, district, party, candidatevotes, candidate
# need 2 party vote share for each state
# make binary variable for incumbency
# make binary variable for contestation
# sum statewide vote shares

# check additional sources to confirm deaths/appointments/term related changes

# Senate -- Molly

view(sen_2022)
view(sen_general)

# sum county vote shares for 2022
# need to join these into one data frame
# filter to be 2000-2022

# make new df where there is county level info
# presidential, sentorial returns go here

# make new df where there is statewide info
# presidential, senatorial sums go here
# congressional sums go here as well

## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

