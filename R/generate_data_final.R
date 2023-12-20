#' @export
open_elections_factory <- function(state) {
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "wi"){
    temp2 <- paste("__",state,"__general__ward.csv", sep = "")
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


oe_data <- open_elections_factory("wi")

#' Retrieves election data for Michigan and Wisconsin from the years 2000-2020
#'
#' If given either Michigan or Wisconsin and a year, this function returns all
#' election data from the openelections github for that state and year
#'
#' @param state A character vector representing either WI or MI
#' @param year A character vector identifying the requested year
#' @return A list of eleven dataframes, each of election data from Wisconsin and
#'  year with the following columns
#' * county
#' * ward
#' * office
#' * district
#' * total.votes
#' * party
#' * candidate
#' * votes
#' * contest_dem
#' * contest_rep
#' * year
#'
#' Note that many of the fields may be an empty string
#'
#' @import tidyverse

#' @export
generate_data <- function(){
  dfs <- list()
  for(i in seq(2000, 2020, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}


#' Retrieves election data for Michigan and Wisconsin from the years 2000-2020
#'
#' If given a year, this function returns all election data from the o
#' penelections github for Wisconsin and that year
#'
#' @param data A dataframe of eleven years of election data for Wisconsin
#' @param year A character vector identifying the requested year
#' @return A dataframe of election data from year with the following columns
#' * county
#' * ward
#' * office
#' * district
#' * total.votes
#' * party
#' * candidate
#' * votes
#' * contest_dem
#' * contest_rep
#' * year
#'
#'
#' @import tidyverse
#' @export

access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}
