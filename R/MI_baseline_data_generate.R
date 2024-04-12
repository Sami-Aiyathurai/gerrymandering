## NEW MICHIGAN
mi_2022 <- mi_data[[12]]
mi_2020 <- mi_data[[11]]
mi_2018 <- mi_data[[10]]

MIyear_baseline_data <- function(year, data) {
  districts_full <- data.frame(District = 1:110, # changed from 1:99 to 1:110 for all of these
                               Dem_votes = integer(length(1:110)),
                               Rep_votes = integer(length(1:110)),
                               Contested = character(length(1:110)))
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

  un_districts_main_year <- check_districts(uncon_main_year)
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
           l1 <- precincts_not_found(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                     statewide_main_minus_four, dis_name),
           l1 <- precincts_found(main_year, mainyearminus2, mainyearminus4, dis_name, districts))
    districts_full[i, ] <- as.data.frame(l1)
  }
  return(districts_full)
}

year_baseline_data_mi(2022, mi_data)
MIyear_baseline_data(2020, mi_data)
