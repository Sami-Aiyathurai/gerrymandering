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

house_general <- house_general %>%
  filter(year >= 2000) %>%
  select(year, state, office, district, candidate, party, candidatevotes, totalvotes)

for (district in house_general) {
  house_general$contest_dem <- ifelse(house_general$party == "DEMOCRAT", 1, 0)
  house_general$contest_rep <- ifelse(house_general$party == "REPUBLICAN", 1, 0)
}

# this loop doesn't work but we need to create some sort of function like this that is gonna go into the data and tally up 2 party vote
for (district in house_general) {
  if (house_general$contest_dem == 1 & house_general$contest_rep == 1) {
    if (house_general$party == "DEMOCRAT") {
      house_general$state_demvotes == candidatevotes
    }
    if (house_general$party == "REPUBLICAN") {
      house_general$state_repvotes == candidatevotes
    }
  }
}

# need to build in some sort of thing because in Minnesota Dems are Democratic-farmer-labor not just dem



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

## modify

sen_2022_county <- sen_2022 %>%
  select(year, state, county_name, office, candidate, party_detailed, party_simplified, candidatevotes, totalvotes)

sen_general <- sen_general %>%
  select(year, state, office, candidate, party_detailed, candidatevotes, totalvotes, party_simplified) %>%
  filter(year >= 2000)

# write a function that will go through sen_2022_county and for each sum the total votes per state and the total party votes per state
# but ideally this function should work for ANY data, not just senatorial, not just house or pres. State level data (if found) would be ideal

# then take the 2022 data and mutate it into the sen_general so that sen_general is 2000-2022

# make new df where there is county level info
# presidential, sentorial returns go here

# make new df where there is statewide info
# presidential, senatorial sums go here
# congressional sums go here as well

## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

