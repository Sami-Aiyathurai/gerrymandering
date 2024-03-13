## Pennsylvania

open_elections_factory_pa <- function(state) {
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "pa"){
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
      #data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      #data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)
    }
    data
  }
}

generate_data_pa <- function(oe_data){
  dfs <- list()
  for(i in seq(2000, 2020, 2)){ # changed to 2020
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

# this is going to take a lot and is not today task
