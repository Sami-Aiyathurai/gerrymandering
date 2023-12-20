#' Retrieves election data for Wisconsin from the years 2000-2020
#'
#' If given Wisconsin and a year, this function returns all
#' election data from the OpenElections github for that state and year
#'
#' @param state A character vector representing either WI or MI
#' @param year A character vector identifying the requested year
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
year_baseline_data <- function(year, data) {
  districts_full <- data.frame(District = 1:99,
                               Dem_votes = integer(length(1:99)),
                               Rep_votes = integer(length(1:99)),
                               Contested = character(length(1:99)))

  myear <- as.character(year)
  myearm2 <- as.character((year-2))
  myearm4 <- as.character((year-4))

  full_sa_di <- sa_contest_all(data)
  main_year <- full_sa_di[[myear]]
  main_year_list <- split(main_year, main_year$contested)
  uncon_main_year <- main_year_list[["uncontested"]]

  main_year_state <- access_state_year(myear, data)
  main_minus_two <- access_state_year(myearm2, data)
  main_minus_four <- access_state_year(myearm4, data)

  statewide_main_year <- statewide_master(main_year_state)
  statewide_main_minus_two <- statewide_master(main_minus_two)
  statewide_main_minus_four <- statewide_master(main_minus_four)

  contested_main_year <- main_year %>% filter(contested == "contested")
  con_districts_main_year <- check_districts(contested_main_year)

  for (i in con_districts_main_year) {
    temp <- contested_main_year %>%
      filter(district == i) %>%
      group_by(district, party, contested) %>%
      summarize(cand_votes = sum(votes)) %>%
      pivot_wider(names_from = "party", values_from = cand_votes) %>%
      select(district, DEM, REP, contested)
    districts_full[i, ] <- temp
  }

  un_districts_main_year <- check_districts(uncon_main_year)
  districts <- list()
  ve_list <- list()

  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      filter(district == i) %>%
      subset(select=-c(contest_r, contest_d, contested))
    dis_name <- as.character(i)
    main_year <- district_func(temp, statewide_main_year)
    mainyearminus2 <- district_func(temp, statewide_main_minus_two)
    mainyearminus4 <- district_func(temp, statewide_main_minus_four)
    districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
    districts[[dis_name]][["estimates"]] <- dis_baseline_ve(i, districts[[dis_name]][["data"]])
    districts_full[i, ] <- districts[[dis_name]][["estimates"]]
    }
  return(districts_full)
}


