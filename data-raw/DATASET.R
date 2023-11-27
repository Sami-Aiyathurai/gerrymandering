library(tidyverse)
library(openintro)

sen_2022 <- read.csv("senate_2022.csv")
sen_general <- read.csv("1976-2020-senate.csv")
house_general <- read.csv("1976-2022-house (1).csv")
pres_general <- read.csv("countypres_2000-2020.csv")

# Presidential

view(pres_general)

# keep year, state name, state abb, county_name, office, party, candidatevotes, totalvotes
# need to sum vote shares for each state total
# make columns for 2 party vote shares

# House

view(house_general)

# filter to be 2000-2022
# keep year, state name, state abb, office, district, party, candidatevotes, candidate
# need 2 party vote share for each state
# make binary variable for incumbency
# make binary variable for contestation
# sum statewide vote shares

# check additional sources to confirm deaths/appointments/term related changes

# Senate
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
