## PA pt 2

## let's ignore the fact that they're different for now... let's get the functions working!!

contest_di_pa <- function(year_data){
  sa_data <- year_data %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    dplyr::filter(.data[["office"]] == "State House")
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

sa_contest_all_pa <- function(data){ #mod function name
  sa_contest_dfs<- list()
  for(i in seq(2004, 2022, 2)){ #mod range to 2004
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_pa(year_data)
  }
  return(sa_contest_dfs)
}

# x data frame of state assembly by district, as established by for loops
#' @param y data frame of statewide data for the given year

district_func_pa(pashd175, statewide_pa)

district_func_pa <- function(x, y) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    dplyr::filter(.data[["office"]] == "State House")
  wards_sax_year <- data.frame(precinct = check_precincts(x))
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "precinct") # changed to precinct
  statewide_x_year <- statewide_x_year[-(11:12)] # changed the indices
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") #%>%
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year)
  return(district_x_year)
}

year_baseline_data_pa <- function(year, data) {
  districts_full <- data.frame(District = 1:203, # changed from 1:99 to 1:203 for all of these
                               Dem_votes = integer(length(1:203)),
                               Rep_votes = integer(length(1:203)),
                               Contested = character(length(1:203)))
  myear <- as.character(year)
  myearm2 <- as.character((year-2))
  myearm4 <- as.character((year-4))

  full_sa_di <- sa_contest_all_pa(data) #mod

  main_year <- full_sa_di[[myear]]
  main_year_list <- split(main_year, main_year$contested)
  uncon_main_year <- main_year_list[["uncontested"]]

  main_year_state <- access_state_year(myear, data)
  main_minus_two <- access_state_year(myearm2, data)
  main_minus_four <- access_state_year(myearm4, data)

  statewide_main_year <- statewide_master_mi(main_year_state)
  statewide_main_minus_two <- statewide_master_mi(main_minus_two)
  statewide_main_minus_four <- statewide_master_mi(main_minus_four)

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

  un_districts_main_year <- check_districts(uncon_main_year) # this is producing a list like it should (yay)
  districts <- list()
  ve_list <- list()

  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      dplyr::filter(.data[["district"]] == i) %>%
      dplyr::select(-c("contest_r", "contest_d", "contested"))
    dis_name <- as.character(i)
    main_year <- district_func_pa(temp, statewide_main_year) # district func mi
    mainyearminus2 <- district_func_pa(temp, statewide_main_minus_two) # district_func_mi
    mainyearminus4 <- district_func_pa(temp, statewide_main_minus_four) # district_func_mi
    districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
    districts[[dis_name]][["estimates"]] <- dis_baseline_ve(i, districts[[dis_name]][["data"]])
    districts_full[i, ] <- districts[[dis_name]][["estimates"]]
  }
  return(districts_full)
}

efficiency_gap_pa <- function(full_votes, year) { #changed the default table
  mi_sa <- data.frame(Year = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(203, 203, 203, 203, 203, 203, 203, 203),
                      Dem_seats = c(104, 91, 93, 84, 82, 93, 90, 102),
                      Rep_seats = c(99, 112, 110, 119, 121, 110, 113, 101)
  )
  myear <- as.character(year)
  wi_sa_year <- mi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  amended <- vote_prep(full_votes) %>%
    dplyr::mutate(PA = "PA") %>% #changed from wi to mi
    dplyr::group_by(.data[["PA"]]) %>%
    dplyr::summarize(total_dem = sum(.data[["Dem_votes"]]),
                     total_rep = sum(.data[["Rep_votes"]]),
                     total_total = sum(.data[["total_votes"]]))
  Vrep <- amended$total_rep
  Vdem <- amended$total_dem
  Vtotal <- amended$total_total
  Vmargin <- ((Vrep - Vdem) / Vtotal)
  Smargin <- 0.5 * ((Srep - Sdem) / Stotal)
  EG <- as.numeric(Vmargin - Smargin)
  return(EG)
}

efficiency_gap_contested_pa <- function(full_votes, year) {
  mi_sa <- data.frame(Year = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(203, 203, 203, 203, 203, 203, 203, 203),
                      Dem_seats = c(104, 91, 93, 84, 82, 93, 90, 102),
                      Rep_seats = c(99, 112, 110, 119, 121, 110, 113, 101)
  )
  myear <- as.character(year)
  wi_sa_year <- mi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  full_votes_cont <- full_votes %>%
    dplyr::filter(.data[["Contested"]] == "contested")
  amended <- vote_prep(full_votes_cont) %>%
    dplyr::mutate(PA = "PA") %>%
    dplyr::group_by(.data[["PA"]]) %>%
    dplyr::summarize(total_dem = sum(.data[["Dem_votes"]]),
                     total_rep = sum(.data[["Rep_votes"]]),
                     total_total = sum(.data[["total_votes"]]))
  Vrep <- amended$total_rep
  Vdem <- amended$total_dem
  Vtotal <- amended$total_total
  Vmargin <- ((Vrep - Vdem) / Vtotal)
  Smargin <- 0.5 * ((Srep - Sdem) / Stotal)
  EG <- as.numeric(Vmargin - Smargin)
  return(EG)
}

