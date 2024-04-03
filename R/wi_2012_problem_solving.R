## baseline func mod

MODbaseline_function <- function(x) {
  base_x <- x %>%
    dplyr::group_by(.data[["office"]], .data[["year"]], .data[["party"]], .data[["district"]]) %>%
    dplyr::summarize(total_votes_2p = mean(.data[["total_votes_2p"]]),
                     cand_total_votes = mean(.data[["cand_total_votes"]]),
                     prop=mean(.data[["prop"]])) %>%
    tidyr::pivot_wider(names_from = "party", values_from = .data[["prop"]])
  if (nrow(base_x) < 2) {
    return(base_x)
  }
  else {
    base_x2 <- subsetting(base_x)
    return(base_x2)
  }
  print(base_x)
  print(base_x)
}

MODdis_baseline_ve <- function(dis_num, data){
  base_sa <- MODbaseline_function(data)
  trim_sa <- slicing_func(base_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_num)
  ve <- vote_estimate(trimmed_sa)
  return(ve)
}

wi <-  baseline_function(wi_data[[6]])

## dis_baseline_ve takes dis_num, data

# wi dis 58

wi <- dis_baseline_ve(58, wi_data)

wi_2008 <- wi_data[[5]]
wi_2010 <- wi_data[[6]]
wi_2012 <- wi_data[[7]]

ybd_2010 <- year_baseline_data(2010, wi_data)
ybd_2012 <- MODyear_baseline_data(2012, wi_data) # TESTING WITH MOD

# when run with and without mod the same error of x[[jj]][iseq] <- vjj: replacement has length zero

MODdis_baseline_ve <- function(dis_num, data){
  base_sa <- MOD_baseline_function(data) ## CHANGED 4/2
  trim_sa <- slicing_func(base_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_num)
  ve <- vote_estimate(trimmed_sa)
  return(ve)
}

## passing into dis_baseline_ve rbind of main_year 1 2 3
# which is dis_func going on uncon_main year, filtered and shit

MODyear_baseline_data <- function(year, data) {
  districts_full <- data.frame(District = 1:99,
                               Dem_votes = integer(length(1:99)),
                               Rep_votes = integer(length(1:99)),
                               Contested = character(length(1:99))) ## add a column that counts the places the districts can be found

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
    main_year <- district_func(temp, statewide_main_year)
    mainyearminus2 <- district_func(temp, statewide_main_minus_two)
    mainyearminus4 <- district_func(temp, statewide_main_minus_four)
    districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
    districts[[dis_name]][["estimates"]] <- MODdis_baseline_ve(i, districts[[dis_name]][["data"]])
    districts_full[i, ] <- districts[[dis_name]][["estimates"]]
  }
  return(districts_full)
}

## BREAKING APART MOD BASELINE FUNCTION

districts_full <- data.frame(District = 1:99,
                             Dem_votes = integer(length(1:99)),
                             Rep_votes = integer(length(1:99)),
                             Contested = character(length(1:99))) ## add a column that counts the places the districts can be found

myear <- as.character(2012)
myearm2 <- as.character((2012-2))
myearm4 <- as.character((2012-4))

full_sa_di <- sa_contest_all(wi_data)
main_year <- full_sa_di[[myear]]
main_year_list <- split(main_year, main_year$contested)
uncon_main_year <- main_year_list[["uncontested"]] # everything through here is working properly

main_year_state <- access_state_year(myear, wi_data)
main_minus_two <- access_state_year(myearm2, wi_data)
main_minus_four <- access_state_year(myearm4, wi_data)

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
} ## working

un_districts_main_year <- check_districts(uncon_main_year)
districts <- list()
ve_list <- list()

for (i in un_districts_main_year) {
  temp <- uncon_main_year %>%
    dplyr::filter(.data[["district"]] == i) %>%
    dplyr::select(-c("contest_r", "contest_d", "contested"))
  dis_name <- as.character(i)
  main_year <- district_func(temp, statewide_main_year)
  mainyearminus2 <- district_func(temp, statewide_main_minus_two)
  mainyearminus4 <- district_func(temp, statewide_main_minus_four)
  districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
  districts[[dis_name]][["estimates"]] <- MODdis_baseline_ve(i, districts[[dis_name]][["data"]])
  districts_full[i, ] <- districts[[dis_name]][["estimates"]]
}
## dis 7 is working and is pulling on BACK YEARS

## now test with 58
temp <- uncon_main_year %>%
  filter(district == 58) %>%
  dplyr::select(-c("contest_r", "contest_d", "contested"))

main_year2 <- district_func(temp, statewide_main_year) # pulls pres, sen, state assembly
main_yearm2 <- district_func(temp, statewide_main_minus_two) # just state assem
main_yearm4 <- district_func(temp, statewide_main_minus_four) # just stae assem


## compare Town of Polk 2010 wards 1-4, 6&7 behavior to that of ward 5

tp <- wi_2010 %>%
  filter(ward == "Town Of Polk Wards 1 - 4, 6 & 7")
tp2 <- wi_2010 %>%
  filter(ward == "Town Of Polk Ward 5")

tp_df <- district_func(tp, statewide_main_minus_two)
tp2_df <- district_func(tp2, statewide_main_minus_four)

basetp <- baseline_function(tp_df)
basetp2 <- baseline_function(tp2_df)
basetpc <- rbind(basetp, basetp2)

# if you can't find a precinct in the year before -> go to whatever is
# before Ward and cut it -> search for it in the earlier filtered years
# if you find it great then mash those together as one obesrvation, baseline it
## estimate votes based on the entire ward joined together

# hope that the partisanship of the area is the same

# this is going to work for WI and maybe MI but NOT CO because that uses NUMBERS
## check for name though cause that might do the trick
