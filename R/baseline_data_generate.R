#' @export
wi_full_sa_di <- sa_contest_all_wi()

#'@export
districts_full <- data.frame(District = 1:99,
                           Dem_votes = integer(length(1:99)),
                           Rep_votes = integer(length(1:99)),
                           Contested = character(length(1:99)))

#' Title
#'
#' @param year
#'
#' @return
#' @export
year_baseline_data <- function(year) {
  myear <- as.character(year)
  myearm2 <- as.character((year-2))
  myearm4 <- as.character((year-4))

  main_year <- wi_full_sa_di[[myear]]
  main_year_list <- split(main_year, main_year$contested)
  uncon_main_year <- main_year_list[["uncontested"]]

  main_year_state <- access_state_year("wi", myear, data)
  main_minus_two <- access_state_year("wi", myearm2, data)
  main_minus_four <- access_state_year("wi", myearm4, data)

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

#' Title
#'
#' @param year
#'
#' @return
#' @export
#'
#' @examples
dis_baseline_ve <- function(dis_num, data){
  base_sa <- baseline_function(data)
  trim_sa <- slicing_func(base_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_num)
  ve <- vote_estimate(trimmed_sa)
  return(ve)
}


#' @export
votes_2010 <- year_baseline_data(2010)

#' @export
votes_2014 <- year_baseline_data(2014)
#' @export
votes_2016 <- year_baseline_data(2016)
#' @export
votes_2018 <- year_baseline_data(2018)
