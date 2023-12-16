#' @export
open_elections_factory <- function(state) {
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "mi"){
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    temp2 <- paste("__",state,"__general__ward.csv", sep = "")
  }

  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- utils::read.csv(url)
    for (district in data) {
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year) # need to create this because it doesn't save year as variable bc each set is separate
    }
    data
  }
}
#' @export
oe_data_WI <- open_elections_factory("wi")

#' @export
oe_data_MI <- open_elections_factory("mi")

#' @export

generate_data <- function(){
  dfs_wi <- list()
  dfs_mi <- list()
  for(i in seq(2000, 2020, 2)){
    year <- toString(i)
    dfs_wi[[year]] <- oe_data_WI(year)
    dfs_mi[[year]] <- oe_data_MI(year)
  }
  dfs <- list()
  dfs[["wi"]] <- dfs_wi
  dfs[["mi"]] <- dfs_mi
  return(dfs)
}

#' @export
data <- generate_data()

#' Retrieves election data for Michigan and Wisconsin from the years 2000-2020
#'
#' If given either Michigan or Wisconsin and a year, this function returns all
#' election data from the openelections github for that state and year
#'
#' @param state A character vector representing either WI or MI
#' @param year A character vector identifying the requested year
#' @return A dataframe of election data from the given state and year with the following columns
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


access_state_year <- function(state, year, data){
  state_year <- data[[state]][[year]]
  return(state_year)
}

