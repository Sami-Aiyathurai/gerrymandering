## NEVADA

## NEVADA

library(rio)

nv_cand <- import("nv_cand.csv")
nv_cand <- nv_cand %>%
  dplyr::select(year, office, district, candidate, party)
nv_cand$year <- as.numeric(nv_cand$year)
nv_precincts <- import("nv_precincts_statewide.xlsx")

## LOADING DATA

open_elections_factory_nv <- function(state) {
  dates = c("2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "nv"){ # changed the state and __precinct
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  }
  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- utils::read.csv(url)
    #for (district in data) {
      if (as.numeric(year) < 2016) {
        data <- nv_prep(data, year)
      }

    #}
    data
  }
}



generate_data_nv <- function(oe_data){
  dfs <- list()
  for(i in seq(2004, 2022, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

nv_data <- open_elections_factory_nv("nv")
nv_data <- generate_data_nv(nv_data)


nv_prep <- function(nv_year, year) {
  yearc <- as.character(year)
  nv_year$year <- as.numeric(year)
  cands_year <- nv_cand %>%
    filter(year == yearc)
  nv_year <- nv_year %>%
    inner_join(cands_year, by=c("year", "office", "district", "candidate"))
  nv_year$contest_dem <- ifelse(nv_year$party == "DEM", 1, 0)
  nv_year$contest_rep <- ifelse(nv_year$party == "REP", 1, 0)
  nv_year$candidate <- str_to_upper(nv_year$candidate)
  return(nv_year)
}

sa_contest_all_nv <- function(data){ #original sa_contest_all, can also just use the CO one as they both start 2004
  sa_contest_dfs<- list()
  for(i in seq(2004, 2014, 2)){ #mod range to 2004-2014 for now
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_co(year_data)
  }
  return(sa_contest_dfs)
}

nv_contested <- sa_contest_all_co(nv_data)

## copy paste new form of sa_contest_all_nv

contested_nv <- sa_contest_all_nv(nv_data)

## dealing with the precincts not being specific

nv_2010 <- nv_data[[4]]

nv_2010_sh <- nv_2010 %>%
  filter(office == "State House")

## formatting NV precincts to get it to work

names(nv_precincts)[names(nv_precincts) == "...1"] <- "county_code"
names(nv_precincts)[names(nv_precincts) == "...3"] <- "county_name"
names(nv_precincts)[names(nv_precincts) == "...6"] <- "registered_precinct"
names(nv_precincts)[names(nv_precincts) == "...7"] <- "congressional_district"
names(nv_precincts)[names(nv_precincts) == "...8"] <- "senate_district"
names(nv_precincts)[names(nv_precincts) == "...10"] <- "house_district"

nv_precincts <- nv_precincts %>%
  select(county_code, county_name, registered_precinct, congressional_district, senate_district, house_district) %>%
  filter(!is.na(registered_precinct))
nv_precincts <- nv_precincts[-1,]

nv_precincts$county_name <- str_to_upper(nv_precincts$county_name)
nv_precincts$registered_precinct <- as.numeric(nv_precincts$registered_precinct)

for (i in nv_precincts) {
  nv_precincts$precinct <- paste0(nv_precincts$county_name, nv_precincts$registered_precinct)
}

## Clark 2010 precinct 130 assigned to districts 22 and 21

hd21_22 <- nv_precincts %>%
  filter(house_district == 21 | house_district == 22)

nv_2010_hd21_22 <- nv_2010 %>%
  filter(office == "State House") %>%
  filter(district == 21 | district == 22)

p2004 <- data.frame(unique(nv_2004$precinct)) # 598 observations
p2006 <- data.frame(unique(nv_2006$precinct)) #
p2008 <- data.frame(unique(nv_2008$precinct))
p2010 <- data.frame(unique(nv_2010$precinct))
p2012 <- data.frame(unique(nv_2012$precinct))
p2014 <- data.frame(unique(nv_2014$precinct))
p2016 <- data.frame(unique(nv_2016$precinct))
p2018 <- data.frame(unique(nv_2018$precinct))
p2020 <- data.frame(unique(nv_2020$precinct))
p2022 <- data.frame(unique(nv_2022$precinct))



## How am I going to deal with 2018, 2020, 2022
nv_2004 <- nv_data[[1]]
nv_2006 <- nv_data[[2]]
nv_2008 <- nv_data[[3]]
nv_2012 <- nv_data[[5]]
nv_2014 <- nv_data[[6]] # names formatted as proper nouns, first name last name
nv_2016 <- nv_data[[7]] # can set to all caps, remove commas between
nv_2018 <- nv_data[[8]] # can set to all caps, remove commas between
nv_2020 <- nv_data[[9]] # can set to all caps, remove commas between
nv_2022 <- nv_data[[10]] # can set to all caps, remove commas between

cand_2018 <- as.data.frame(unique(nv_2018$candidate))
cand_2020 <- as.data.frame(unique(nv_2020$candidate))
cand_2022 <- as.data.frame(unique(nv_2022$candidate))

for (i in seq_along(cand_2018)) {
  cand_2018[i] <- str_to_lower(cand_2018[i])
  return(cand_2018)
}

## this worked for 2018 but not 2020 nor 2022 :/

## I think I'm going to want to grepl this; anything before the space gets moved to front
## anything after the space stays in the back, saving strings

## GOAL MAKE ALL OF IT CAPPED

for (i in seq_along(cand_2020)) {
  cand_2020[i] <- str_to_upper(cand_2020[i])
  return(cand_2020)
}

for (i in seq_along(cand_2022)) {
  cand_2022[i] <- str_to_lower(cand_2022[i])
  return(cand_2022)
}

## OTHER MODS
nv_2008 <- access_state_year("2008", nv_data)
statewide_nv08 <- filter_statewide_mi(nv_2008)

nv08d4 <- nv_2008 %>%
  filter(office == "State House") %>%
  filter(precinct == 130)

