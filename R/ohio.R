# OHIO

open_elections_factory_oh <- function(state) {
  dates = c("2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "oh"){
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
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)
    }
    data
  }
}

generate_data_oh <- function(oe_data){
  dfs <- list()
  for(i in seq(2010, 2020, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

## No OH precinct level data for 2000, 2002, 2004, 2008 but starting at 2010 just because for the functions

## OH 2012 is SEPARATED INTO different files (presidential, house, senate, state house/senate)
## OH 2010 does not have party labels but everything from 2012 on DOES

oh_data <- open_elections_factory_oh("oh")

oh_data <- generate_data_oh(oh_data)



