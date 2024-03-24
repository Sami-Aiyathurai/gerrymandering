## MISSIPPI

open_elections_factory_ms <- function(state) {
  dates = c("2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "ms"){
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

generate_data_ms <- function(oe_data){
  dfs <- list()
  for(i in seq(2008, 2022, 2)){ # changed to include 2022
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

ms_data <- open_elections_factory_ms("ms")
ms_data <- generate_data_ms(ms_data)

## ALERT: MS ONLY HAS STATE HOUSE FOR 2 YEARS. NOT WORTH TIME.
