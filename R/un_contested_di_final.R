#' @export
contest_di <- function(year_data){
  sa_data <- year_data %>%
    filter(party == "DEM" | party == "REP") %>%
    filter(office == "State Assembly")
  di_data_sa <- sa_data %>%
    group_by(district)  %>%
    summarize(contest_d = sum(contest_dem), contest_r = sum(contest_rep))
  for (district in di_data_sa) {
    di_data_sa$contested <- ifelse((di_data_sa$contest_d == 0) | (di_data_sa$contest_r == 0), "uncontested", "contested")
  }
  full_sa_di <- sa_data %>% left_join(di_data_sa, by=c('district'))

  return(full_sa_di)
}


#' This makes use of the data generated in generate_data_final and the access_state_year functiond defined there
#' to generate a list of 11 elements with every even year's election data on State assembly
#'
#' Each year consists of a list of the contested districts and uncontested districts for the state assembly election
#'
#' currently the function only works for Wisconson
#'
#'
#' @return an eleven element list of the major election years from 2000-2020.
#' Each year returns a list of two dataframes with the contested and uncontested districts for state assembly
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
#' Note that many of the fields may be an empty string
#'
#' @import tidyverse
#' @export

sa_contest_all_wi <- function(){
  sa_contest_dfs_wi <- list()
  for(i in seq(2000, 2020, 2)){
    year <- toString(i)
    year_data <- access_state_year("wi", year, data)
    sa_contest_dfs_wi[[year]] <- contest_di(year_data)
  }
  return(sa_contest_dfs_wi)
}


