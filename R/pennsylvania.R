## Pennsylvania

library(berryFunctions)
library(tidyverse)
library(stringr)
library(forstringr)

open_elections_factory_pa <- function(state) {
  dates = c("2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="2016", "2018"="2018", "2020"="2020", "2022"="2022")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "pa"){ # changed the state and __precinct
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  }
  read <- function(year){
  date <- dates[year]
  temp3 <- paste(date,temp2, sep = "")
  temp4 <- paste("https://www.dos.pa.gov/VotingElections/BEST/stats/Documents/ElectionReturns_",date, "_General_PrecinctReturns.txt", sep="")
    url <- file.path(temp1 , year, temp3)
    url2 <- file.path(temp4)
    if (year <= 2014) {
      data <- utils::read.csv(url)
        if (year == 2004) {
          data <- pa_2004_function(data)
          data <- selecting_function(data)
        }
        if (year == 2006) {
          data <- pa_2006_function(data)
          data <- selecting_function(data)
        }
        if (year == 2008) {
          data <- pa_2008_function(data)
          data <- selecting_function(data)
        }
        if (year == 2010) {
          data <- pa_2010_function(data)
          data <- selecting_function(data)
        }
        if (year == 2012) {
          data <- pa_2012_function(data)
          data <- selecting_function(data)
          data <- filter_2012(data)
        }
        if (year == 2014) {
          data <- pa_2014_function(data)
          data <- selecting_function(data)
          data <- filter_2014(data)
        }
    }
    if (year >= 2016) {
      data <- utils::read.csv(url2)
      if (year == 2016) {
        data <- pa_2016_function(data)
        data <- selecting_function(data)
        data <- filter_2016(data)
      }
      if (year == 2018) {
        data <- pa_2018_function(data)
        data <- selecting_function(data)
        data <- filter_2018(data)
      }
      if (year == 2020) {
        data <- pa_2020_function(data)
        data <- selecting_function(data)
        data <- filter_2020(data)
      }
      if (year == 2022) {
        data <- pa_2022_function(data)
        data <- selecting_function(data)
      }
    }
    data
  }
}

generate_data <- function(oe_data){
  dfs <- list()
  for(i in seq(2004, 2022, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}

pa_data <- open_elections_factory_pa("pa")
pa_data <- generate_data_pa(pa_data)

## ORGANIZING AND TIDYING PA 4 aka 2008

## 2004

pa_2004_function <- function(pa_year) {
  r2 <- as.vector(c(2004, "G", 1, 10, 1, 0, 1, 1, "USP", "DEM", "X40800", "KERRY", "JOHN", "F.", "", 105, 19, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 19, 33, 91))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2004"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USP"] <- "office"
  names(mod_year)[names(mod_year) == "KERRY"] <- "last_name"
  names(mod_year)[names(mod_year) == "JOHN"] <- "first_name"
  names(mod_year)[names(mod_year) == "DEM"] <- "party"
  names(mod_year)[names(mod_year) == "X"] <- "suffix"
  names(mod_year)[names(mod_year) == "X1"] <- "vote_office"
  names(mod_year)[names(mod_year) == "X"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X105"] <- "votes"
  names(mod_year)[names(mod_year) == "X19"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.1"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.2"] <- "other_mun_id"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mun_id2"
  names(mod_year)[names(mod_year) == "X.4"] <- "other_mun_id3"

  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(123340)

  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_id, sep="")
  }

  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()

  # final_year <- mod_year %>%
  #   dplyr::select(county_code, precinct_code, district, office, party, votes, precinct_name, candidate, year, contest_dem, contest_rep)

  return(mod_year)
}


## 2006

pa_2006 <- pa_data[[4]]

pa_2006_function <- function(pa_year) {
  r2 <- as.vector(c(2006, "G", 1, 10, 2, 0, 1, 1, "USS", "DEM", "X60070", "CASEY", "BOB", "X", "JR", 93, 19, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 19, 33, 91))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2006"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USS"] <- "office"
  names(mod_year)[names(mod_year) == "CASEY"] <- "last_name"
  names(mod_year)[names(mod_year) == "BOB"] <- "first_name"
  names(mod_year)[names(mod_year) == "DEM"] <- "party"
  names(mod_year)[names(mod_year) == "JR"] <- "suffix"
  names(mod_year)[names(mod_year) == "X2"] <- "vote_office"
  names(mod_year)[names(mod_year) == "X"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X93"] <- "votes"
  names(mod_year)[names(mod_year) == "X19"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.1"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.2"] <- "other_mun_identifier"

  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(123340)

  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_identifier, sep="")
  }

  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()

  return(mod_year)
}


## 2008

pa_2008 <- pa_data[[5]]

pa_2008_function <- function(pa_year) {
  r2 <- as.vector(c(2008, "G", 1, 10, 1, 0, 1, 1, "USP", "DEM", "X80486", "OBAMA", "BARACK", "", "", 154, 19, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 19, 33, 91))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2008"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USP"] <- "office"
  names(mod_year)[names(mod_year) == "OBAMA"] <- "last_name"
  names(mod_year)[names(mod_year) == "BARACK"] <- "first_name"
  names(mod_year)[names(mod_year) == "DEM"] <- "party"
  names(mod_year)[names(mod_year) == "X"] <- "suffix"
  names(mod_year)[names(mod_year) == "X1"] <- "vote_office"
  names(mod_year)[names(mod_year) == "x"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X154"] <- "votes"
  names(mod_year)[names(mod_year) == "X19"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.2"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mun_identifier"

  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(226304)

  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_identifier, sep="")
  }

  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()
  return(mod_year)
}

## this is necessary to take whatever the header is because otherwise the values will be lost

pa_2010_function <- function(pa_year) {
  r2 <- as.vector(c(2010, "G", 1, 10, 2, 0, 1, 1, "USS", "DEM", 639, "SESTAK", "JOE", "", "", 85, 19, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 19, 33, 91))
  mod_2010 <- insertRows(pa_year, 3, new=r2)
  names(mod_2010)[names(mod_2010) == "X2010"] <- "year"
  names(mod_2010)[names(mod_2010) == "G"] <- "election_type"
  names(mod_2010)[names(mod_2010) == "X1"] <- "county_code"
  names(mod_2010)[names(mod_2010) == "X10"] <- "precinct_code"
  names(mod_2010)[names(mod_2010) == "USS"] <- "office"
  names(mod_2010)[names(mod_2010) == "SESTAK"] <- "last_name"
  names(mod_2010)[names(mod_2010) == "JOE"] <- "first_name"
  names(mod_2010)[names(mod_2010) == "DEM"] <- "party"
  names(mod_2010)[names(mod_2010) == "X.1"] <- "suffix"
  names(mod_2010)[names(mod_2010) == "X2"] <- "vote_office"
  names(mod_2010)[names(mod_2010) == "X"] <- "extra_name_id"
  names(mod_2010)[names(mod_2010) == "X0"] <- "district"
  names(mod_2010)[names(mod_2010) == "X85"] <- "votes"
  names(mod_2010)[names(mod_2010) == "X19"] <- "congressional_district"
  names(mod_2010)[names(mod_2010) == "X33"] <- "state_senate_district"
  names(mod_2010)[names(mod_2010) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_2010)[names(mod_2010) == "X193"] <- "state_house_district"
  names(mod_2010)[names(mod_2010) == "X.2"] <- "type_of_mun"
  names(mod_2010)[names(mod_2010) == "X.3"] <- "other_mun_identifier"
  mod_2010$office[mod_2010$office == "USP"] <- "President"
  mod_2010$office[mod_2010$office == "USS"] <- "U.S. Senate"
  mod_2010$office[mod_2010$office == "GOV"] <- "Governor"
  mod_2010$office[mod_2010$office == "ATT"] <- "Attorney General"
  mod_2010$office[mod_2010$office == "STS"] <- "State Senate"
  mod_2010$office[mod_2010$office == "STH"] <- "State House"
  mod_2010$votes <- as.numeric(mod_2010$votes)
  mod_2010$votes <- as.integer(mod_2010$votes)
  mod_2010$district <- as.numeric(mod_2010$district)
  mod_2010$district <- as.integer(mod_2010$district)
  mod_2010$party <- as.character(mod_2010$party)
  mod_2010$year <- as.numeric(mod_2010$year)
  mod_2010$candidate <- as.character(121579)

  for (row in mod_2010) {
    mod_2010$candidate <- paste(mod_2010$first_name, mod_2010$last_name, sep=" ")
    mod_2010$precinct <- paste(mod_2010$municipality_name, mod_2010$type_of_mun, mod_2010$other_mun_identifier, sep="")
  }
  mod_2010$precinct <- str_replace_all(string=mod_2010$precinct, pattern=" ", repl="")
  mod_2010$contest_dem <- ifelse(mod_2010$party == "DEM", 1, 0)
  mod_2010$contest_rep <- ifelse(mod_2010$party == "REP", 1, 0)

  mod_2010 <- mod_2010 %>%
    distinct()

  return(mod_2010)
}

pa_2012 <- pa_data[[5]]

pa_2012_function <- function(pa_year) {
  r2 <- as.vector(c(2012, "G", 1, 10, 1, 0, 1, 2, "USP", "REP", "X2012C0418", "ROMNEY", "MITT", "", "", 195, 4, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 19, 33, 91))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2012"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USP"] <- "office"
  names(mod_year)[names(mod_year) == "ROMNEY"] <- "last_name"
  names(mod_year)[names(mod_year) == "MITT"] <- "first_name"
  names(mod_year)[names(mod_year) == "REP"] <- "party"
  names(mod_year)[names(mod_year) == "X.1"] <- "suffix"
  names(mod_year)[names(mod_year) == "X2"] <- "vote_office"
  names(mod_year)[names(mod_year) == "X"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X195"] <- "votes"
  names(mod_year)[names(mod_year) == "X19"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33.1"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X91"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.2"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mun_identifier"

  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(205268)

  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_identifier, sep="")
  }
  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()
  return(mod_year)
}

filter_2012 <- function(pa_year) {
  pa_year$party[pa_year$candidate == "BRAD ROAE"] <- "REP"
  pa_year$party[pa_year$candidate == "BRIAN ELLIS"] <- "REP"
  pa_year$party[pa_year$candidate == "JIM MARSHALL"] <- "REP"
  pa_year$party[pa_year$candidate == "MATT SMITH"] <- "DEM"
  pa_year$party[pa_year$candidate == "JESSE WHITE"] <- "DEM"
  pa_year$party[pa_year$candidate == "DONNA OBERLANDER"] <- "REP"
  pa_year$party[pa_year$candidate == "MATTHEW BAKER"] <- "REP"
  pa_year$party[pa_year$candidate == "CARL METZGAR"] <- "REP"
  pa_year$party[pa_year$candidate == "DICK HESS"] <- "REP"
  pa_year$party[pa_year$candidate == "SID KAVULICH"] <- "DEM"
  pa_year$party[pa_year$candidate == "KAREN BOBACK"] <- "REP"
  pa_year$party[pa_year$candidate == "NICK MICCARELLI"] <- "REP"
  return(pa_year)
}

## 2014

pa_2014 <- pa_data[[8]]

pa_2014_function <- function(pa_year) {
  r2 <- as.vector(c(2014, "G", 1, 10, 1, 0, 1, 2, "GOV", "REP", "X2014C1183", "CORBETT", "THOMAS", "W.", "JR", 124, 4, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 33, 193))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2014"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "GOV"] <- "office"
  names(mod_year)[names(mod_year) == "CORBETT"] <- "last_name"
  names(mod_year)[names(mod_year) == "THOMAS"] <- "first_name"
  names(mod_year)[names(mod_year) == "REP"] <- "party"
  names(mod_year)[names(mod_year) == "JR"] <- "suffix"
  names(mod_year)[names(mod_year) == "X2"] <- "vote_office"
  names(mod_year)[names(mod_year) == "W."] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X124"] <- "votes"
  names(mod_year)[names(mod_year) == "X4"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.1"] <- "other_mun_identifier"

  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(119502)
  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_identifier, sep="")
  }
  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()
  return(mod_year)
}

filter_2014 <- function(pa_year) {
  pa_year$party[pa_year$candidate == "JAMES MARSHALL"] <- "REP"
  pa_year$party[pa_year$candidate == "CARL METZGAR"] <- "REP"
  pa_year$party[pa_year$candidate == "GEROME KNOWLES"] <- "REP" # GERRY
  pa_year$party[pa_year$candidate == "THOMAS SANKEY"] <- "REP" # TOMMY
  pa_year$party[pa_year$candidate == "NICHOLAS MICCARELLI"] <- "REP" # NICK IN PRIOR
  pa_year$party[pa_year$candidate == "TARAH TOOHIL"] <- "REP"
  pa_year$party[pa_year$candidate == "MARK LONGIETTI"] <- "DEM"
  pa_year$party[pa_year$candidate == " WRITE-IN"] <- "Other"
  return(pa_year)
}

## 2016 mods

pa_2016 <- pa_data[[9]]

pa_2016_function <- function(pa_year) {
  r2 <- as.vector(c(2016, "G", 1, 10, 1, 0, 1, 1, "USP", "DEM", "2016c0483", "CLINTON", "HILLARY", "", "", 119, 4, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 19, 33, 193))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2016"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USP"] <- "office"
  names(mod_year)[names(mod_year) == "CLINTON"] <- "last_name"
  names(mod_year)[names(mod_year) == "HILLARY"] <- "first_name"
  names(mod_year)[names(mod_year) == "DEM"] <- "party"
  names(mod_year)[names(mod_year) == "X"] <- "suffix"
  names(mod_year)[names(mod_year) == "X.1"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X119"] <- "votes"
  names(mod_year)[names(mod_year) == "X4"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.2"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mun_identifier"
  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(204012)
  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_identifier, sep="")
  }
  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)
  mod_year <- mod_year %>%
    distinct()
  return(mod_year)
}

filter_2016 <- function(pa_year) {
  pa_year$party[pa_year$candidate == "MARK LONGIETTI"] <- "DEM"
  pa_year$party[pa_year$candidate == "CHRISTOPHER SAINATO"] <- "DEM" # instead of Chris
  pa_year$party[pa_year$candidate == "JAMES MARSHALL"] <- "REP"
  pa_year$party[pa_year$candidate == "HAROLD ENGLISH"] <- "REP" # HAROLD NOT HAL
  pa_year$party[pa_year$candidate == "ANITA ASTORINO KULIK"] <- "DEM"
  pa_year$party[pa_year$candidate == "MATTHEW BAKER"] <- "REP"
  pa_year$party[pa_year$candidate == "CARL METZGAR"] <- "REP"
  pa_year$party[pa_year$candidate == "H CONKLIN"] <- "DEM" # scott conklin
  pa_year$party[pa_year$candidate == "RONALD MARSICO"] <- "REP"
  pa_year$party[pa_year$candidate == "KAREN BOBACK"] <- "REP"
  pa_year$party[pa_year$district == 124] <- "REP" # GERRY
  pa_year$party[pa_year$candidate == "GARY DAY"] <- "REP"
  return(pa_year)
}

## 2018 mods

pa_2018_function <- function(pa_year) {
  r2 <- as.vector(c(2018, "G", 1, 10, 1, 0, 1, 1, "USS", "DEM", "2018C0950", "CASEY", "ROBERT", "P", "JR", 120, 0, 0, 13, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, "", "", 0, 13, 33, 193))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2018"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USS"] <- "office"
  names(mod_year)[names(mod_year) == "CASEY"] <- "last_name"
  names(mod_year)[names(mod_year) == "ROBERT"] <- "first_name"
  names(mod_year)[names(mod_year) == "DEM"] <- "party"
  names(mod_year)[names(mod_year) == "JR"] <- "suffix"
  names(mod_year)[names(mod_year) == "X1.1"] <- "vote_office"
  names(mod_year)[names(mod_year) == "P"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X120"] <- "votes"
  names(mod_year)[names(mod_year) == "X13"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.1"] <- "other_mun_identifier"
  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(114233)
  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, sep="")
  }
  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)
  mod_year <- mod_year %>%
    distinct()
  return(mod_year)
}

filter_2018 <- function(pa_year) {
  pa_year$party[pa_year$candidate == "MARK LONGIETTI"] <- "DEM"
  pa_year$party[pa_year$candidate == "AARON BERNSTINE"] <- "DEM"
  pa_year$party[pa_year$candidate == "THOMAS SANKEY"] <- "REP"
  pa_year$party[pa_year$candidate == "MATTHEW GABLER"] <- "REP"
  return(pa_year)
}

## 2020 mods

pa_2020 <- pa_data[[11]]

pa_2020_function <- function(pa_year) {
  r2 <- as.vector(c(2020, "G", 1, 10, 1, 0, 1, 2, "USP", "DEM", "2020C0962", "BIDEN", "JOSEPH", "ROBINETTE", "JR", 137, 0, 0, 13, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, "", "", 0, 13, 33, 193))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2020"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USP"] <- "office"
  names(mod_year)[names(mod_year) == "BIDEN"] <- "last_name"
  names(mod_year)[names(mod_year) == "JOSEPH"] <- "first_name"
  names(mod_year)[names(mod_year) == "DEM"] <- "party"
  names(mod_year)[names(mod_year) == "JR"] <- "suffix"
  names(mod_year)[names(mod_year) == "P"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X137"] <- "votes"
  names(mod_year)[names(mod_year) == "X13"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.1"] <- "other_mun_identifier"
  names(mod_year)[names(mod_year) == "X.2"] <- "other_mun_id_2"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mud_id_3"
  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(179507)
  for (row in mod_year) {
     mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
     mod_year$precinct <- paste(mod_year$municipality_name, sep="")
   }
  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)
  mod_year <- mod_year %>%
    distinct()

  return(mod_year)
}

filter_2020 <- function(pa_year) {
  pa_year$party[pa_year$candidate == "MARK LONGIETTI"] <- "DEM"
  return(pa_year)
}

## 2022 mods

pa_2022 <- pa_data[[12]]

pa_2022_function <- function(pa_year) {
  r2 <- as.vector(c(2022, "G", 1, 10, 1, 0, 1, 4, "USS", "DEM", "2022C0203", "FETTERMAN", "JOHN", "K", "", 109, 0, 0, 13, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, "", "", 0, 13, 33, 193))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2022"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USS"] <- "office"
  names(mod_year)[names(mod_year) == "FETTERMAN"] <- "last_name"
  names(mod_year)[names(mod_year) == "JOHN"] <- "first_name"
  names(mod_year)[names(mod_year) == "DEM"] <- "party"
  names(mod_year)[names(mod_year) == ""] <- "suffix"
  names(mod_year)[names(mod_year) == "X1.2"] <- "vote_office"
  names(mod_year)[names(mod_year) == "K"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X109"] <- "votes"
  names(mod_year)[names(mod_year) == "X13"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.1"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.2"] <- "other_mun_identifier"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mun_id2"
  names(mod_year)[names(mod_year) == "X.4"] <- "other_mun_id3"

  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(131796)

  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, sep="")
  }
  mod_year$precinct <- str_replace_all(string=mod_year$precinct, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()

  return(mod_year)
}

## selection function

selecting_function <- function(pa_year) {
  pa_year <- pa_year %>%
    dplyr::select(county_code, year, office, district, candidate, party, precinct, contest_dem, contest_rep, votes)
  return(pa_year)
}

## misc



## WORK TO ISOLATE PRECINCT NAMES NOT NECESSARY ANYMORE

bellevue12 <- pa_data[[7]] %>%
  filter(precinct_name == "BELLEVUEW1" | precinct_name == "BELLEVUEW2" | precinct_name == "BELLEVUEW3")

unique(bellevue12$state_house_district)

bb16 <- pp %>%
  filter(precinct_name == "BELLACRES" | precinct_name == "COLLIERW1" | precinct_name == "BELLEVUEW1-1" | precinct_name == "BELLEVUEW1-2" | precinct_name == "BELLEVUEW2-1" |
           precinct_name == "BELLEVUEW2-2" | precinct_name == "BELLEVUEW3-1" | precinct_name == "BELLEVUEW3-2")

unique(bellevue16$district)

for (precint in bb16) {
  ifelse(grepl("-", bb16$precinct_name), )
}

grepl("-", bb16$precinct_name) # if there's a - then TRUE if not FALSE

gsub("\\-.*", "", bb16$precinct_name) # remove everything after the -

## need to grab every precinct_name with the chr before the dash and THEN group_by and summarize and reattach that


bellevue16 %>%
  group_by(precinct_name, district, office, candidate, party) %>%
  summarize(votes = sum(votes))

### initialize empty DF

cols = c("county", "precinct_name", "office", "district", "candidate", "party", "votes", "contest_dem", "contest_rep")

new_pa2016 <- data.frame(matrix(nrow=0, ncol=length(cols)))

colnames(new_pa2016)= cols

## will need to write an ifelse()
## write a function for the TRUE and write a function for FALSE

## let's start by making this work one line at a time then we can function it

## bb16 has BELLACRES, COLLIERW1, BELLEVUEW1-1, BELLEVUEW1-2, BELLEVUEW2-1, BELLEVUEW2-2, BELLEVUEW3-1, BELLEVUEW3-2

bbs <- bb16 %>%
  filter(precinct_name == "BELLEVUEW1-1" | precinct_name == "BELLEVUEW1-2")

datalist <- list()

for (i in seq_along(unique(bb16$precinct_name))) {
  str <- bb16$precinct_name[i]
  print(str)
  ifelse(grepl("-", str), true_function(bb16), false_function(bb16))
}

for (i in seq_along(pa_2016$precinct_name)) {
  str <- pa_2016$precinct_name[i]
  #print(str)
  ifelse(grepl("-", str), true_function(pa_2016), false_function(pa_2016))
  # if (grepl("-", str) == TRUE) {
  #   str1 <- str_extract_part(bbs$precinct_name[i], before=TRUE, pattern="-")
  #   print(str1)
  #   only_str1 <- bbs %>%
  #     dplyr::filter(grepl(str1, precinct_name))
  #   county_holder <- bbs$county[i]
  #   only_str1m <- only_str1 %>%
  #     filter(grepl(str1, precinct_name)) %>%
  #     group_by(district, office, candidate, party, contest_dem, contest_rep) %>%
  #     summarize(votes = sum(votes)) %>%
  #     add_column(precinct_name = str1, .before = "district") %>%
  #     add_column(county = county_holder, .before = "precinct_name")
  #   only_str1_reordered <- only_str1m %>%
  #     dplyr::select(county, precinct_name, district, office, candidate, party, votes, contest_dem, contest_rep)
  #   new2016pa <- rbind(new_pa2016, only_str1_reordered)
  # }
  # else {
  #   df_temp <- bb16 %>%
  #     dplyr::filter(grepl(str, precinct_name))
  #   new2016pa <- rbind(new_pa2016, df_temp)
  # }
  return(datalist)
}

true_function(bb16)

true_function <- function(data, ...) {
  str1 <- str_extract_part(data$precinct_name[i], before=TRUE, pattern="-")
  #print(str1)
  only_str1 <- data %>%
    dplyr::filter(grepl(str1, precinct_name))
  county_holder <- data$county[i]
  only_str1m <- only_str1 %>%
    filter(grepl(str1, precinct_name)) %>%
    group_by(district, office, candidate, party, contest_dem, contest_rep) %>%
    summarize(votes = sum(votes)) %>%
    add_column(precinct_name = str1, .before = "district") %>%
    add_column(county = county_holder, .before = "precinct_name")
  only_str1_reordered <- only_str1m %>%
    dplyr::select(county, precinct_name, district, office, candidate, party, votes, contest_dem, contest_rep)
  #new_pa2016 <- rbind(new_pa2016, only_str1_reordered)
  datalist[[i]] <- only_str1_reordered

  #print(new_pa2016)
  return(datalist)
}

true_function(bb16)
false_function(bb16)

false_function <- function(data, ...) {
  df_temp <- data %>%
    dplyr::filter(grepl(str, precinct_name))
  datalist[[i]] <- df_temp
  # new_pa2016 <- rbind(new_pa2016, df_temp)
  # print(new_pa2016)
  return(datalist)
}



ifelse(grepl("-", str), true_function(bb16), false_function(bb16))

bell <- bb16$precinct_name[1]
b2 <- bb16$precinct_name[2]
eb2 <- str_extract_part(bb16$precinct_name[2], before=TRUE, pattern="-")
grepl("-", bell)
county_holder <- bb16$county[2]

## need to make sure all candidates are CAPPED

b11 <- bb16 %>%
  filter(grepl(eb2, precinct_name)) %>%
  group_by(district, office, candidate, party, contest_dem, contest_rep) %>%
  summarize(votes = sum(votes)) %>%
  add_column(precinct_name = eb2, .before = "district") %>%
  add_column(county = county_holder, .before = "precinct_name") %>%
  dplyr::select(county, precinct_name, district, office, candidate, party, votes, contest_dem, contest_rep)

rbind(new_pa2016, b11)

grep <- grepl("-", bb16$precinct_name)
new16 <- list(bb16$precinct_name[grep == TRUE]) ## list with all the separated ones
unique(new16)

for (i in seq_along(new16[[1]])) {
  str <- str_extract_part(bb16$precinct_name, before = TRUE, pattern="-")
  df <- bb16[str == TRUE]
  print(df)
}

string <- str_extract_part(bb16$precinct_name[2], before = TRUE, pattern="-")
bb16$precinct_name[str]

old16 <- list(bb16$precinct_name[grep == FALSE])


grep_function <- function(data, ...) {
  for (row in data) {
    grep <- grepl("-", data$precinct_name)
    new <- data$precinct_name[grep]
    #print(grep)
    print(new)
    ifelse(grep, true_function(data), print("no"))
  }
}

grep_function(bb16)


### true function

bbstr <- bb16 %>%
  filter(precinct_name == "BELLEVUEW1-1" | precinct_name == "BELLEVUEW1-2")


## REJECT YEARS

pa_2000 <- pa_data[[1]]

pa_2000_function <- function(pa_year) {
  r2 <- as.vector(c(2000, "G", 1, 10, 1, 0, 1, 1, "USP", "REP", "844", "BUSH", "GEORGE", "W.", "", 153, 19, 33, 91, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 0, 0, 0))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2000"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "USP"] <- "office"
  names(mod_year)[names(mod_year) == "BUSH"] <- "last_name"
  names(mod_year)[names(mod_year) == "GEORGE"] <- "first_name"
  names(mod_year)[names(mod_year) == "REP"] <- "party"
  names(mod_year)[names(mod_year) == "X"] <- "suffix"
  names(mod_year)[names(mod_year) == "W."] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X153"] <- "votes"
  names(mod_year)[names(mod_year) == "X19"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.1"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.2"] <- "other_mun_id"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mun_id2"
  names(mod_year)[names(mod_year) == "X.4"] <- "other_mun_id3"

  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(377648)

  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_id,
                               mod_year$other_mun_id2, mod_year$other_mun_id3, sep="")
  }

  mod_year$precinct <- str_replace_all(string=mod_year$precinct_name, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()

  # final_year <- mod_year %>%
  #   dplyr::select(county_code, precinct_code, district, office, party, votes, precinct_name, candidate, year, contest_dem, contest_rep)

  return(mod_year)
}

## 2002

pa_2002 <- pa_data[[2]]

pa_2002_function <- function(pa_year) {
  r2 <- as.vector(c(2002, "G", 1, 10, 3, 0, 1, 1, "GOV", "REP", "20934", "FISHER", "MIKE", "", "", 114, 19, 33, 193, 6, "ABBOTTSTOWN", "", "", "", "", 0, 5, 1, 0010, 0, 19, 33, 91))
  mod_year <- insertRows(pa_year, 3, new=r2)
  names(mod_year)[names(mod_year) == "X2002"] <- "year"
  names(mod_year)[names(mod_year) == "G"] <- "election_type"
  names(mod_year)[names(mod_year) == "X1"] <- "county_code"
  names(mod_year)[names(mod_year) == "X10"] <- "precinct_code"
  names(mod_year)[names(mod_year) == "GOV"] <- "office"
  names(mod_year)[names(mod_year) == "FISHER"] <- "last_name"
  names(mod_year)[names(mod_year) == "MIKE"] <- "first_name"
  names(mod_year)[names(mod_year) == "REP"] <- "party"
  names(mod_year)[names(mod_year) == "X"] <- "suffix"
  names(mod_year)[names(mod_year) == "X.1"] <- "extra_name_id"
  names(mod_year)[names(mod_year) == "X0"] <- "district"
  names(mod_year)[names(mod_year) == "X114"] <- "votes"
  names(mod_year)[names(mod_year) == "X19"] <- "congressional_district"
  names(mod_year)[names(mod_year) == "X33"] <- "state_senate_district"
  names(mod_year)[names(mod_year) == "X193"] <- "state_house_district"
  names(mod_year)[names(mod_year) == "X.2"] <- "type_of_mun"
  names(mod_year)[names(mod_year) == "ABBOTTSTOWN"] <- "municipality_name"
  names(mod_year)[names(mod_year) == "X.3"] <- "other_mun_id"
  names(mod_year)[names(mod_year) == "X.4"] <- "other_mun_id2"
  names(mod_year)[names(mod_year) == "X.5"] <- "other_mun_id3"


  mod_year$office[mod_year$office == "USP"] <- "President"
  mod_year$office[mod_year$office == "USS"] <- "U.S. Senate"
  mod_year$office[mod_year$office == "GOV"] <- "Governor"
  mod_year$office[mod_year$office == "ATT"] <- "Attorney General"
  mod_year$office[mod_year$office == "STS"] <- "State Senate"
  mod_year$office[mod_year$office == "STH"] <- "State House"
  mod_year$votes <- as.numeric(mod_year$votes)
  mod_year$votes <- as.integer(mod_year$votes)
  mod_year$district <- as.numeric(mod_year$district)
  mod_year$district <- as.integer(mod_year$district)
  mod_year$party <- as.character(mod_year$party)
  mod_year$year <- as.numeric(mod_year$year)
  mod_year$candidate <- as.character(115098)

  for (row in mod_year) {
    mod_year$candidate <- paste(mod_year$first_name, mod_year$last_name, sep=" ")
    mod_year$precinct_name <- paste(mod_year$municipality_name, mod_year$type_of_mun, mod_year$other_mun_id,
                                    mod_year$other_mun_id2, mod_year$other_mun_id3, sep="")
  }

  mod_year$precinct_name <- str_replace_all(string=mod_year$precinct_name, pattern=" ", repl="")
  mod_year$contest_dem <- ifelse(mod_year$party == "DEM", 1, 0)
  mod_year$contest_rep <- ifelse(mod_year$party == "REP", 1, 0)

  mod_year <- mod_year %>%
    distinct()

  # final_year <- mod_year %>%
  #   dplyr::select(county_code, precinct_code, district, office, party, votes, precinct_name, candidate, year, contest_dem, contest_rep)

  return(mod_year)
}



