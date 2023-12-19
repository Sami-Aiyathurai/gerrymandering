wi_full_sa_di <- sa_contest_all_wi()

year_baseline_data <- function(year) {
  myear <- as.character(year)
  myearm2 <- as.character((year-2))
  myearm4 <- as.character((year-4))

  main_year <- wi_full_sa_di[[myear]]
  main_year_list <- split(main_year, main_year$contested)
  uncon_main_year <- main_year_list[["uncontested"]]

  main_year_state <- access_state_year("wi", myear, data)
  main_minus_two <- access_state_year("wi", myearm2, data)
  main_minus_four <- access_state_year("wi", myearm4, data)
  #gets statewide data for year and two previous years

  #actually running on statewide master
  statewide_main_year <- statewide_master(main_year_state)
  statewide_main_minus_two <- statewide_master(main_minus_two)
  statewide_main_minus_four <- statewide_master(main_minus_four)

  un_districts_main_year <- check_districts(uncon_main_year)
  # empty list to store data for year
  districts <- list()
  ve_list <- list()
  # gets data for uncontested districts for year
  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      filter(district == i) %>%
      subset(select=-c(contest_r, contest_d, contested))
    dis_name <- as.character(i)
    main_year <- district_func(temp, statewide_main_year)
    mainyearminus2 <- district_func(temp, statewide_main_minus_two)
    mainyearminus4 <- district_func(temp, statewide_main_minus_four)
    districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
    districts[[dis_name]][["mollysaveme"]] <- dis_baseline_ve(i, districts[[dis_name]][["data"]])
  }
  return(districts)
}

#this leaves me with data from three years for all uncontested districts in 2010
testing1 <- year_baseline_data(2010)

dis_baseline_ve <- function(dis_num, data){
  base_sa <- baseline_function(data)
  trim_sa <- slicing_func(base_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_num)
  ve <- vote_estimate(trimmed_sa)
  return(ve)
}

#this works on a district basis
#get baselines for district 3 and district 8
base_sa3 <- baseline_function(testing1[["3"]])
base_sa8 <- baseline_function(testing1[["8"]])
#trim baselines
trim_sa3 <- slicing_func(base_sa3)
trim_sa8 <- slicing_func(base_sa8)
#who knows
trimmed_sa3 <- trimmed_func(trim_sa3, 3)
trimmed_sa8 <- trimmed_func(trim_sa8, 8)


