## CO baseline mods

## get it to work for CO 2012 -> need CO 2010, CO 2008

co_2012 <- co_data[[5]]
co_2010 <- co_data[[4]]
co_2008 <- co_data[[3]]

## deconstruct YBD_CO

COyear_baseline_data <- function(year, data) {
  districts_full <- data.frame(District = 1:65, # changed from 1:99 to 1:65 for all of these
                               Dem_votes = integer(length(1:65)),
                               Rep_votes = integer(length(1:65)),
                               Contested = character(length(1:65)))
  myear <- as.character(2012)
  myearm2 <- as.character((2010))
  myearm4 <- as.character((2008))

  full_sa_di <- sa_contest_all_co(co_data) #mod

  main_year <- full_sa_di[[myear]]
  main_year_list <- split(main_year, main_year$contested)
  uncon_main_year <- main_year_list[["uncontested"]]

  main_year_state <- access_state_year(myear, co_data)
  main_minus_two <- access_state_year(myearm2, co_data)
  main_minus_four <- access_state_year(myearm4, co_data)

  statewide_main_year <- statewide_master(main_year_state)
  statewide_main_minus_two <- statewide_master(main_minus_two)
  statewide_main_minus_four <- statewide_master(main_minus_four)

  contested_main_year <- main_year %>%
    dplyr::filter(.data[["contested"]] == "contested")
  con_districts_main_year <- check_districts(contested_main_year)

  for (i in con_districts_main_year) {
    temp <- contested_main_year %>%
      dplyr::filter(.data[["district"]] == i) %>%
      dplyr::group_by(.data[["district"]], .data[["party"]], .data[["contested"]]) %>%
      dplyr::summarize(cand_votes = sum(.data[["votes"]])) %>%
      tidyr::pivot_wider(names_from = "party", values_from = .data[["cand_votes"]]) %>%
      dplyr::select(.data[["district"]], .data[["DEM"]], .data[["REP"]], .data[["contested"]])
    districts_full[i, ] <- temp
  }
  dis_est <- districts_full %>% ## this produces an estimate of avg voters per CONTESTED district
    dplyr::filter(Contested == "contested") %>%
    mutate(total_votes = Dem_votes + Rep_votes) %>%
    summarize(Avg_dem_votes = as.integer(mean(Dem_votes)),
              Avg_rep_votes = as.integer(mean(Rep_votes)),
              Avg_total_votes = as.integer(mean(total_votes)))

  un_districts_main_year <- check_districts(uncon_main_year) # this is producing a list like it should (yay)
  districts <- list()
  ve_list <- list()

  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      dplyr::filter(.data[["district"]] == i) %>%
      dplyr::select(-c("contest_r", "contest_d", "contested"))
    dis_name <- as.character(i)
    main_year <- district_func_precincts(temp, statewide_main_year) # district func mi
    mainyearminus2 <- district_func_precincts(temp, statewide_main_minus_two) # district_func_mi
    mainyearminus4 <- district_func_precincts(temp, statewide_main_minus_four) # district_func_mi
    ifelse(mainyearminus2$year[1] == as.numeric(myear) | mainyearminus4$year[1] == as.numeric(myear),
           l1 <- COprecincts_not_found(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                       statewide_main_minus_four, dis_name),
           l1 <- precincts_found(main_year, mainyearminus2, mainyearminus4, dis_name, districts))

    districts_full[i, ] <- as.data.frame(l1)
  }
  return(districts_full)
}

COyear_baseline_data(2012, co_data)

## INSERT THIS INTO YBDCO

hd1412 <- co_2012 %>%
  filter(office == "State House" & district == 14) ## this district is UNCONTESTED

