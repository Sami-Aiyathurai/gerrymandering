precincts_not_found <- function(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                statewide_main_minus_four, dis_name,...) {
  df_org <- main_year %>%
    filter(office != "State House")
  print(head(df_org))
  cc <- check_concat(temp)
  str_mod <- unique(str_extract_part(cc, before=TRUE, pattern=" precinct"))
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
  return(obj)
}

precincts_found <- function(main_year, mainyearminus2, mainyearminus4, dis_name, districts) {
  print("precincts found!")
  districts[[dis_name]][["data"]] <- rbind(main_year, mainyearminus2, mainyearminus4)
  obj <- dis_baseline_ve(dis_name, districts[[dis_name]][["data"]])
  print(obj)
  return(obj)
}

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
