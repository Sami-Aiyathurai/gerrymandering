## PA baseline 2.0

PAyear_baseline_data <- function(year, data) {
  districts_full <- data.frame(District = 1:203,
                               Dem_votes = integer(length(1:203)),
                               Rep_votes = integer(length(1:203)),
                               Contested = character(length(1:203)))

  myear <- as.character(year)
  myearm2 <- as.character((year-2))
  myearm4 <- as.character((year-4))
  print(myear)
  full_sa_di <- sa_contest_all_pa(data)
  main_year <- full_sa_di[[myear]]
  main_year_list <- split(main_year, main_year$contested)
  uncon_main_year <- main_year_list[["uncontested"]]

  print(head(uncon_main_year))
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
  print(districts_full)
  un_districts_main_year <- check_districts(uncon_main_year)
  print(un_districts_main_year)
  districts <- list()
  ve_list <- list()

  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      dplyr::filter(.data[["district"]] == i) %>%
      dplyr::select(-c("contest_r", "contest_d", "contested"))
    dis_name <- as.character(i)
    main_year <- district_func_precincts(temp, statewide_main_year)
    mainyearminus2 <- district_func_precincts(temp, statewide_main_minus_two)
    mainyearminus4 <- district_func_precincts(temp, statewide_main_minus_four)
    ifelse(mainyearminus2$year[1] == as.numeric(myear) | mainyearminus4$year[1] == as.numeric(myear),
           l1 <- precincts_not_found(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                     statewide_main_minus_four, dis_name),
           l1 <- precincts_found(main_year, mainyearminus2, mainyearminus4, dis_name, districts))
    #print("hello")
    #print(l1)
    districts_full[i, ] <- as.data.frame(l1)
    # districts_full[i, ] <- l1[[dis_name]][["estimates"]]
  }
  return(districts_full)
}

pybd2012 <- PAyear_baseline_data(2012, pa_data)
pybd2010 <- PAyear_baseline_data(2010, pa_data)
pybd2022 <- PAyear_baseline_data(2022, pa_data)
ybd2012 <- year_baseline_data_pa(2012, pa_data)
ybd2010 <- year_baseline_data_pa(2010, pa_data)

## will prob need to make a PA form of precincts_not_found

PAprecincts_not_found <- function(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                statewide_main_minus_four, dis_name,...) {
  df_org <- main_year %>%
    filter(office != "State House")
  print(head(df_org))
  cc <- check_concat(temp)
  str_mod <- unique(str_extract_part(cc, before=TRUE, pattern=c(" d", " p", " x", " w")))
  list_grp <- list()
  list_grp2 <- list()
  for (n in seq_along(unique(str_mod))) {
    str <- str_mod[n]
    grp <- grep(as.character(str), statewide_main_minus_two$cw_concat, value=FALSE) # gives ALLLL occcurrences of the first part
    grp2 <- grep(as.character(str), statewide_main_minus_four$cw_concat, value=FALSE)
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
      }
    }
    for (k in seq_along(list_grp2)) {
      if (k == 1) {
        index <- list_grp2[[k]]
        setting_row <- statewide_main_minus_four[index, ]
        df2 <- as.data.frame(setting_row)
      }
      else {
        index <- list_grp2[[k]]
        new_row <- statewide_main_minus_four[index, ]
        df2[nrow(df2) + 1,] <- new_row
      }
    }
  }
  df3 <- rbind(df1, df2)
  df4 <- vote_prep_uncon(df3)
  df5 <- rbind(df_org, df4)
  obj <- dis_baseline_ve2(dis_name, df5)
  print("precincts not found!")
  return(obj)
}

precincts_found <- function(main_year, mainyearminus2, mainyearminus4, dis_name, districts) {
  districts[[dis_name]][["data"]] <- rbind(main_year, mainyearminus2, mainyearminus4)
  obj <- dis_baseline_ve(dis_name, districts[[dis_name]][["data"]])
  print("precincts found!")
  return(obj)
}

# it looks like all precincts are found soooooo... I did something right

check_concat <- function(year_data) {
  concat <- unique(year_data$cw_concat)
  return(concat)
}

baseline_func_mismatched <- function(x) {
  base_x <- x %>%
    dplyr::group_by(office, year, party, district) %>%
    dplyr::summarize(total_votes_2p = mean(total_votes_2p),
                     cand_total_votes = mean(cand_total_votes),
                     prop=mean(prop)) %>%
    tidyr::pivot_wider(names_from="party", values_from=prop)
  base_x2 <- subsetting(base_x)
  #print(base_x2)
}

dis_baseline_ve2 <- function(dis_name, data) {
  base_sa <- baseline_func_mismatched(data)
  trim_sa <- slicing_func(base_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_name)
  ve <- vote_estimate(trimmed_sa)
  return(ve)
}

vote_prep_uncon <- function(df3) {
  df_temp <- df3 %>%
    dplyr::select(-c(total_votes, total_votes_2p))
  #print(head(df_temp))
  tv_p <- df_temp %>%
    group_by(office, year) %>%
    summarize(total_votes = sum(votes))
  #print(head(tv_p))
  tv_2p <- df_temp %>%
    filter(party == "DEM" | party == "REP") %>%
    group_by(office, year) %>%
    summarize(total_votes_2p = sum(votes))
  #print(head(tv_2p))
  cv <- df_temp %>%
    filter(party == "DEM" | party == "REP") %>%
    group_by(office, year, party) %>%
    summarize(cand_total_votes = sum(votes))
  #print(head(cv))
  df_new <- df_temp %>%
    left_join(tv_p, by=c("office", "year")) %>%
    left_join(tv_2p, by=c("office", "year")) %>%
    left_join(cv, by=c("office", "year", "party"))
  df_new <- df_new %>%
    filter(party == "DEM" | party == "REP") %>%
    mutate(prop = cand_total_votes / total_votes_2p)
  return(df_new)
}
