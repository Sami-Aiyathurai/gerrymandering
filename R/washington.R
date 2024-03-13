## Washington

# no precinct data in 2000-2004, 2008
# I modified it to pull only 2010-2020 but I'm sure there's a way to pull 2006 as well
# and general results from 2000-2008 where available (ask Sami)
# this means I can only calculate EGs for 2014-2020

## WARD and State Assembly
## Goes in the WI camp

open_elections_factory_wa <- function(state) { # mod date range
  dates = c("2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "wa"){ # changed the state and __precinct
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  }

  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- utils::read.csv(url)
    for (district in data) {
      data <- variable_prep(data)
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)
    }
    data
  }
}

generate_data_wa <- function(oe_data){
  dfs <- list()
  for(i in seq(2010, 2020, 2)){ # changed to 2006, WA has no data available before 2006 for precincts
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}

wa_2010 <- access_state_year("2010", wa_data)

wa_data <- open_elections_factory_wa("wa")
wa_data <- generate_data_wa(wa_data)
wa_contested <- sa_contest_all_wa(wa_data)

# something in here isn't working...

## uncontested step 1 functions modified

# can use contest_di

# takes dataframe from access_state_year
contest_di_wa <- function(year_data){
  sa_data <- year_data %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    dplyr::filter(.data[["office"]] == "State House")
  di_data_sa <- sa_data %>%
    dplyr::group_by(district)  %>%
    dplyr::summarize(contest_d = sum(.data[["contest_dem"]]), contest_r = sum(.data[["contest_rep"]]))
  for (district in di_data_sa) {
    di_data_sa$contested <- ifelse((di_data_sa$contest_d == 0) | (di_data_sa$contest_r == 0), "uncontested", "contested")
  }
  full_sa_di <- sa_data %>%
    dplyr::left_join(di_data_sa, by=c('district'))
  return(full_sa_di)
}
# param is full data from generate data
sa_contest_all_wa <- function(data){
  sa_contest_dfs<- list()
  for(i in seq(2010, 2020, 2)){ # changed to include 2020, changed to 2010
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di(year_data)
  }
  return(sa_contest_dfs)
}

