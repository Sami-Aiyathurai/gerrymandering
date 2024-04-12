## CO variations of precincts_not_found

# ## district_func for counties
#
# #district_func_precincts <- function(temp, statewide_main_year) {
#   tv_sax_year <- total_vote_func(temp8)
#   tv2p_sax_year <- total_2p_vote_func(temp8)
#   sax_year <- vote_join(temp8, tv_sax_year, tv2p_sax_year) %>%
#     dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
#   wards_sax_year <- data.frame(precinct = check_precincts(temp8)) # did not change these variable names
#   statewide_x_year <- statewide_main_year %>%
#     dplyr::right_join(wards_sax_year, by = "precinct") # changed to precinct
#   statewide_x_year <- statewide_x_year %>%
#     dplyr::select(-c(total_votes, total_votes_2p))
#   tv_statewide_x_year <- total_vote_func(statewide_x_year)
#   tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
#   statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
#     dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
#   district_x_year <- rbind(statewide_x_year, sax_year)
#   district_x_year <- candidate_function(district_x_year)
# #} # this should be spitting out the correct information... one county, 429 precincts

COprecincts_not_found <- function(temp, main_year, statewide_main_year, statewide_main_minus_two,
                                  statewide_main_minus_four, dis_name, dis_est) {
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

COdis_baseline_ve <- function(dis_name, data, dis_est) {
  base_sa <- baseline_func_mismatched(data)
  trim_sa <- slicing_func(base_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_name)
  l1 <- COvote_estimate(trimmed_sa, dis_est)
  return(l1)
}

COvote_estimate <- function(trimmed_sa, dis_est) {
  print(dis_est)
  print(trimmed_sa)
  est_dem <- as.integer(dis_est[[3]] * trimmed_sa[[3]])
  est_rep <- as.integer(dis_est[[3]] * trimmed_sa[[4]])
  l1 <- list(District = trimmed_sa[[1]], Dem_votes = est_dem, Rep_votes = est_rep, Contested = "uncontested")
  return(l1)
}

# COprecincts_found <- function(main_year, mainyearminus2, mainyearminus4, dis_name, districts) {
#   districts[[dis_name]][["data"]] <- rbind(main_year, mainyearminus2, mainyearminus4)
#   obj <- dis_baseline_ve(dis_name, districts[[dis_name]][["data"]])
#   return(obj)
# }

## ALL OF THESE ARE ACTUALLY CONTESTED

## everything below here is pulled out of the functions!

districts_full <- data.frame(District = 1:65, # changed from 1:99 to 1:65 for all of these
                             Dem_votes = integer(length(1:65)),
                             Rep_votes = integer(length(1:65)),
                             Contested = character(length(1:65)))
myear <- as.character(2008)
myearm2 <- as.character((2006))
myearm4 <- as.character((2004))

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

#for (i in un_districts_main_year) {
temp32 <- uncon_main_year %>%
  dplyr::filter(.data[["district"]] == 32) %>%
  dplyr::select(-c("contest_r", "contest_d", "contested"))
dis_name <- as.character(32)
print(dis_est)
main_year <- district_func_precincts(temp32, statewide_main_year) # district func mi
mainyearminus2 <- district_func_precincts(temp32, statewide_main_minus_two) # district_func_mi
mainyearminus4 <- district_func_precincts(temp32, statewide_main_minus_four) # district_func_mi
ifelse(mainyearminus2$year[1] == as.numeric(myear) | mainyearminus4$year[1] == as.numeric(myear),
       l1 <- COprecincts_not_found(temp=temp32, main_year, statewide_main_year, statewide_main_minus_two,
                                   statewide_main_minus_four, dis_name, dis_est),
       l1 <- precincts_found(main_year, mainyearminus2, mainyearminus4, dis_name, districts))
districts_full[i, ] <- as.data.frame(l1)
#}

COprecincts_not_found(temp=temp32, main_year=main_year, statewide_main_year, statewide_main_minus_two,
                      statewide_main_minus_four, dis_name, dis_est)



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
