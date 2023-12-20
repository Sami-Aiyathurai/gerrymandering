#' Retrieves election data for Wisconsin from the years 2000-2020
#'
#' If given Wisconsin and a year, this function returns all
#' election data from the OpenElections github for that state and year
#'
#' @param state A character vector representing either WI or MI
#' @return A list of eleven data frames, each of election data from Wisconsin and
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

#' Retrieves election data for Wisconsin from the years 2000-2020
#' If given Wisconsin and a year, this function returns all
#' election data from the OpenElections github for that state and year
#' @return A list of eleven data frames, each of election data from Wisconsin and
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
#' @param oe_data A character vector representing wi
#' @export
generate_data <- function(oe_data){
  dfs <- list()
  for(i in seq(2000, 2020, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

#' Retrieves election data for Wisconsin from the years 2000-2020
#'
#' If given Wisconsin and a year, this function returns all
#' election data from the OpenElections github for that state and year

#' @return A list of eleven data frames, each of election data from Wisconsin and
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
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join

#' @param year A character vector identifying the requested year
#' @param data the primary data frame with all of the information for the given year
#' @export
access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}
