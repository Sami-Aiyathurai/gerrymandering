## CO variations of precincts_not_found

COprecincts_not_found <- function(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                  statewide_main_minus_four, dis_name,...) {
  df_org <- main_year %>%
    filter(office != "State House")

  str_county <- unique(main_year$county)
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
        print(j)
      }
    }
    for (k in seq_along(list_grp2)) {
      if (k == 1) {
        index <- list_grp2[[k]]
        setting_row <- statewide_main_minus_four[index, ]
        df2 <- as.data.frame(setting_row)
        print(df2)
      }
      else {
        index <- list_grp2[[k]]
        new_row <- statewide_main_minus_four[index, ]
        df2[nrow(df2) + 1,] <- new_row
        print(k)
      }
    }
  }
  df3 <- rbind(df1, df2)
  df4 <- vote_prep_uncon(df3)
  df5 <- rbind(df_org, df4)
  obj <- COdis_baseline_ve(dis_name, df5, dis_est)
  print("CO precincts not found")
  return(obj)
}



COdis_baseline_ve <- function(dis_name, data, dis_est) {
  base_sa <- baseline_func_mismatched(data)
  trim_sa <- slicing_func(base_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_name)
  l1 <- COvote_estimate(trimmed_sa, dis_est)
  return(l1)
}


COvote_estimate <- function(trimmed_sa, dis_est) {
  est_dem <- as.integer(dis_est[[3]] * trimmed_sa[[3]])
  est_rep <- as.integer(dis_est[[3]] * trimmed_sa[[4]])
  l1 <- list(District = trimmed_sa[[1]], Dem_votes = est_dem, Rep_votes = est_rep, Contested = "uncontested")
  return(l1)
}
