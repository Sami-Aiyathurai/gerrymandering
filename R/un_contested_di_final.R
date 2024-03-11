#' @param year_data data frame as returned from access_state_year
contest_di <- function(year_data){
  sa_data <- year_data %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    dplyr::filter(.data[["office"]] == "State Assembly")
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


#' This makes use of the data generated in generate_data_final and the
#' access_state_year functiond defined there to generate a list of 11 elements
#' with every even year's election data on State assembly
#'
#' Each year consists of a list of the contested districts and uncontested
#' districts for the state assembly election
#'
#' currently the function only works for Wisconsin
#'
#'
#' @return an eleven element list of the major election years from 2000-2020.
#' Each year returns a list of two dataframes with the contested and
#' uncontested districts for state assembly
#' Each dataframe has the following columns
#' * county
#' * ward
#' * office (always state assembly)
#' * district
#' * total.votes
#' * party
#' * candidate
#' * votes
#' * contest_dem
#' * contest_rep
#' * year
#' * contested
#'
#' @import tidyverse
#' @param data the full list of data frames from generate data
#' @export

sa_contest_all<- function(data){
  sa_contest_dfs<- list()
  for(i in seq(2000, 2022, 2)){ # changed to include 2022
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di(year_data)
  }
  return(sa_contest_dfs)
}



