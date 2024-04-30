## let's see what we can do for OH 2010-2022

#access state year works! Great

OHsa_contest_all <- function(data){
  sa_contest_dfs<- list()
  for(i in seq(2006, 2022, 2)){ # changed to include 2022
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di(year_data)
  }
  return(sa_contest_dfs)
}

contest_di <- function(year_data){
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

district_func_concat <- function(dis_data, statewide_data) {
  print(unique(dis_data$district))
  statewide_na_votes <- statewide_data %>%
    filter(is.na(votes))
  #print(statewide_na_votes)
  tv_dis_year <- total_vote_func(dis_data)
  tv2p_dis_year <- total_2p_vote_func(dis_data)
  dis_year <- vote_join(dis_data, tv_dis_year, tv2p_dis_year) %>%
    dplyr::filter(party == "DEM" | party == "REP")
  #print(dis_year)
  concat_year <- data.frame(cw_concat = check_concat(dis_data)) # equivalent of wards_sax_year
  #print(unique(concat_year))
  statewide_concat_year <- statewide_data %>%
    dplyr::right_join(concat_year, by = "cw_concat")
  #print(unique(statewide_concat_year$cw_concat))
  statewide_dis_year <- statewide_concat_year %>%
    dplyr::select(-c(total_votes, total_votes_2p))
  #print(head(statewide_dis_year)) # prints all of the statewide races in that district that year
  # print(unique(statewide_dis_year$office))
  tv_statewide_dis_year <- statewide_dis_year %>%
    group_by(office) %>%
    summarize(total_votes = sum(votes)) %>%
    filter(!is.na(total_votes))
  #print(tv_statewide_dis_year)
  tv2p_statewide_dis_year <- total_2p_vote_func(statewide_dis_year)
  #print(tv2p_statewide_dis_year)
  statewide_dis_year <- vote_join(statewide_dis_year, tv_statewide_dis_year, tv2p_statewide_dis_year) %>%
    dplyr::filter(party == "DEM" | party == "REP")
  #print(head(statewide_dis_year))
  #print(head(dis_year))
  district_year <- rbind(statewide_dis_year, dis_year)
  district_year <- candidate_function(district_year)
  #return(district_year)
}
district_func_concat(d9612, state_mym4)
district_func_concat(d122, state_mym2)
district_func_concat(d122, state_myear)

contested_oh <- OHsa_contest_all(oh_data)

s2010 <- statewide_master(oh2010) # works, only has the relevant statewide offices for the years
s2012 <- statewide_master(oh2012)
s2014 <- statewide_master(oh2014)
s2016 <- statewide_master(oh2016)
s2018 <- statewide_master(oh2018)
s2020 <- statewide_master(oh2020)
s2022 <- statewide_master(oh2022)

df2010 <- district_func_precincts(contested_oh, oh2010)

OHyear_baseline_data <- function(year, data) { # year is num
  districts_full <- data.frame(District = 1:99, # changed from 1:99 to 1:65 for all of these
                               Dem_votes = integer(length(1:99)),
                               Rep_votes = integer(length(1:99)),
                               Contested = character(length(1:99)))
  myear <- as.character(year) # now here are the issues
  myearm2 <- as.character(year-2)
  myearm4 <- as.character(year-4)

  full_sa_di <- OHsa_contest_all(oh_data) #mod

  main_year <- full_sa_di[[myear]]
  main_year_list <- split(main_year, main_year$contested)
  uncon_main_year <- main_year_list[["uncontested"]]

  uncon_main_year %>%
    filter(is.na(district))

  asy_myear <- access_state_year(myear, oh_data) # consistent with the years. Yay!
  asy_mym2 <- access_state_year(myearm2, oh_data)
  asy_mym4 <- access_state_year(myearm4, oh_data)
  state_myear <- statewide_master(asy_myear)
  state_mym2 <- statewide_master(asy_mym2)
  state_mym4 <- statewide_master(asy_mym4)

  contested_main_year %>%
    filter(is.na(district))

  contested_main_year <- main_year %>%
    dplyr::filter(.data[["contested"]] == "contested")
  con_districts_main_year <- check_districts(contested_main_year)
  print(con_districts_main_year)

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
  print(dis_est)
  uncon_main_year %>%
    filter(is.na(district))

  un_districts_main_year <- check_districts(uncon_main_year) # this is producing a list like it should (yay)
  districts <- list()
  ve_list <- list()
  print(un_districts_main_year)
  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      dplyr::filter(.data[["district"]] == i) %>% # with district 1
      dplyr::select(-c("contest_r", "contest_d", "contested"))
    dis_name <- as.character(i)
    main_year <- district_func_concat(temp, state_myear) # district func mi
    #print(head(main_year))
    mainyearminus2 <- district_func_concat(temp, state_mym2) # district_func_mi
    #print(head(mainyearminus2))
    mainyearminus4 <- district_func_concat(temp, state_mym4) # district_func_mi
    #print(head(mainyearminus4))
    ifelse(mainyearminus2$year[1] == as.numeric(myear) | mainyearminus4$year[1] == as.numeric(myear),
           l1 <- COprecincts_not_found(temp, main_year, state_myear, state_mym2,
                                       state_mym4, dis_name, dis_est),
           l1 <- precincts_found(main_year, mainyearminus2, mainyearminus4, dis_name, districts))
    districts_full[i, ] <- as.data.frame(l1)
  }
  return(districts_full)
}

OHyear_baseline_data(2010, oh_data)
ohybd14 <- OHyear_baseline_data(2014, oh_data)
ohybd16 <- OHyear_baseline_data(2016, oh_data)
ohybd2022 <- OHyear_baseline_data(2022, oh_data)

OHefficiency_gap(ohybd14, 2014)

OHefficiency_gap <- function(full_votes, year) { #changed the default table
  mi_sa <- data.frame(Year = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(99, 99, 99, 99, 99, 99, 99, 99),
                      Dem_seats = c(53, 40, 39, 34, 33, 38, 35, 32),
                      Rep_seats = c(46, 59, 60, 65, 66, 61, 64, 67)
  )
  myear <- as.character(year)
  wi_sa_year <- mi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  amended <- vote_prep(full_votes) %>%
    dplyr::mutate(OH = "OH") %>% #changed from wi to mi
    dplyr::group_by(.data[["OH"]]) %>%
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


COprecincts_not_found <- function(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                  statewide_main_minus_four, dis_name, dis_est) {
  df_org <- main_year %>%
    filter(office != "State House")
  print("precinct not found")

  str_county <- unique(main_year$county)
  print(str_county)
  list_grp <- list()
  list_grp2 <- list()
  for (n in seq_along(str_county)) {
    str <- str_county[n]
    grp <- grep(as.character(str), statewide_main_minus_two$county, value=FALSE)
    grp2 <- grep(as.character(str), statewide_main_minus_four$county, value=FALSE)
    list_grp = append(list_grp, grp)
    list_grp2 = append(list_grp2, grp2)
    for (j in seq_along(list_grp)) {
      if (j == 1) {
        index <- list_grp[[j]]
        setting_row <- statewide_main_minus_two[index, ]
        df1 <- as.data.frame(setting_row)
      }
      else {
        index <- list_grp[[j]]
        new_row <- statewide_main_minus_two[index, ]
        df1[nrow(df1) + 1,] <- new_row
        #print(j)
      }
    }
    for (k in seq_along(list_grp2)) {
      if (k == 1) {
        index <- list_grp2[[k]]
        setting_row <- statewide_main_minus_four[index, ]
        df2 <- as.data.frame(setting_row)
        #print(df2)
      }
      else {
        index <- list_grp2[[k]]
        new_row <- statewide_main_minus_four[index, ]
        df2[nrow(df2) + 1,] <- new_row
        #print(k)
      }
    }
  }
  df3 <- rbind(df1, df2)
  df4 <- vote_prep_uncon(df3)
  df5 <- rbind(df_org, df4)
  obj <- COdis_baseline_ve(dis_name, df5, dis_est)
  return(obj)
}
