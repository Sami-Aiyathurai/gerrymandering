d58 <- uncon_main_year %>%
  filter(district == 58) %>%
  dplyr::select(-c("contest_r", "contest_d", "contested"))
dis_name <- as.character(58)
d58my <- district_func(d58, statewide_main_year)
d58mym2 <- district_func(d58, statewide_main_minus_two)
d58mym4 <- district_func(d58, statewide_main_minus_four)

fixing_time <- function(main_year, mainyearminus2, mainyearminus4,...) {
  ifelse(any(mainyearminus2$year != m2010) | any(mainyearminus4$year != m2008), print("no match"), print("match"))
}

fixing_time(main_year, mainyearminus2, mainyearminus4)


yes_we_have_issues <- function(temp, ...) {
  cc <- check_concat(temp)
  str_mod <- unique(str_extract_part(cc, before=TRUE, pattern=" precinct"))
  list_grp <- list()
  list_grp2 <- list()
  for (i in seq_along(unique(str_mod))) {
    str <- str_mod[i]
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
        print(k)
        index <- list_grp2[[k]]
        print(index)
        setting_row <- statewide_main_minus_four[index, ]
        df2 <- as.data.frame(setting_row)
      }
      else {
        index <- list_grp2[[k]]
        new_row <- statewide_main_minus_four[index, ]
        df2[nrow(df2) + 1,] <- new_row
      }
    }
    df3 <- rbind(df1, df2)
  }
}
unique(d58$cw_concat)

check_concat <- function(year_data) {
  concat <- unique(year_data$cw_concat)
  return(concat)
}

cc <- check_concat(d58)
str_mod <- unique(str_extract_part(cc, before=TRUE, pattern=" precinct"))
list_grp <- list()
list_grp2 <- list()

for (i in seq_along(unique(str_mod))) {
  str <- str_mod[i]
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
      print(k)
      index <- list_grp2[[k]]
      print(index)
      setting_row <- statewide_main_minus_four[index, ]
      df2 <- as.data.frame(setting_row)
    }
    else {
      index <- list_grp2[[k]]
      new_row <- statewide_main_minus_four[index, ]
      df2[nrow(df2) + 1,] <- new_row
    }
  }
  #df3 <- rbind(df1, df2)
  #df3 <- office_func_uncon(df3)

  }# this worked!!! YAYYYYYY

data_frame_prep <- function(df) {
  df <- df %>%
    dplyr::select(-c(total_votes, total_votes_2p))
  tv_sax_yr <- total_vote_func(df)
  print(tv_sax_yr)
  #return(df)
}

data_frame_prep(df1)

## OKAY I'M STOPPING HERE FOR THE NIGHT
# EVERYTHING ABOVE IS WORKING! IT IS PULLING WHAT IT NEEDS TO!
# BUT FOR SOME REASON, THE TOTAL_VOTES FUNCS ARE NOT WORKING PROPERLY

df1_2 <- df1 %>%
  dplyr::select(-c(total_votes, total_votes_2p))

df1_2 %>%
  group_by(office, party) %>%
  summarize(total_votes = sum(votes)) # this number is just TOO HIGH


tv_sax_year <- total_vote_func(df1)
tv2p_sax_year <- total_2p_vote_func(df1)
sax_year <- vote_join(df1, tv_sax_year, tv2p_sax_year) %>%
  filter(party == "DEM" | party == "REP")
district_x_year <- office_func_uncon(sax_year) ## OMG IT DOES

## so district_x_year has all of the results from statewide_2010 that match the county/precinct
# name.

## So now I need to mod the baseline funcs to work properly for this

baseline_func_mismatched <- function(x) {
  base_x <- x %>%
    dplyr::group_by(office, year, party, district) %>%
    dplyr::summarize(total_votes_2p = mean(total_votes_2p),
                     office_total_votes = mean(office_total_votes),
                     prop=mean(prop)) %>%
    tidyr::pivot_wider(names_from="party", values_from=prop)
  base_x2 <- subsetting(base_x)
  print(base_x2)
}

baseline_func_mismatched(district_x_year)
baseline_function(disf1) #these have the same output so I think it's right??? YES!!

dis_baseline_ve(1, disf1)



office_func_uncon <- function(df) { # to replace cand_func at the end of district_func
  office_x_year <- df3 %>%
    dplyr::group_by(office, party, year) %>%
    summarize(office_total_votes = sum(votes))
  print(office_x_year)
  df4 <- df3 %>%
    left_join(office_x_year, by=c("office", "party", "year")) %>%
    mutate(prop= office_total_votes / total_votes_2p)
  return(df4)
}

head(office_func_uncon(df1))
