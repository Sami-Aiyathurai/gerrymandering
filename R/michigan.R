# MICHIGAN #

## Nothing before 2008 for state legislative

## Functions to load the data

open_elections_factory_mi <- function(state) {
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "mi"){ # changed the state and __precinct
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  }

  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- utils::read.csv(url)
    for (district in data) {
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)

      if (year == 2022) {
        data$votes <- as.numeric(data$votes)
        data$votes <- as.integer(data$votes)
      }
    }
    data
  }
}

generate_data <- function(oe_data){
  dfs <- list()
  for(i in seq(2000, 2022, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

access_state_year <- function(year, data){
access_state_year_mi <- function(year, data){
  state_year <- data[[year]]

  return(state_year)
}

mi_data <- open_elections_factory_mi("mi")
mi_data <- generate_data(mi_data)

## uncontested step 1 functions modified

contest_di_mi <- function(year_data){
  sa_data <- year_data %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    dplyr::filter(.data[["office"]] == "State House") # mod state assembly -> state house
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

sa_contest_all_mi <- function(data){
  sa_contest_dfs<- list()
  for(i in seq(2000, 2022, 2)){
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_mi(year_data)
  }
  return(sa_contest_dfs)
}

## Data cleaning uncontested

statewide_master_mi <- function(x) { # create new func: statewide_master_mi
  x <- filter_statewide_mi(x) # call filter_statewide_mi
  y <- total_vote_func(x)
  z <- total_2p_vote_func(x)
  x <- vote_join(x, y, z)
  return(x)
}

filter_statewide_mi <- function(x) {
  x <- x %>%
    filter(.data[["office"]] == "U.S. Senate" | .data[["office"]] == "President" | .data[["office"]] == "Attorney General" | # changed to include U.S. Senate
             .data[["office"]] == "Secretary of State" | .data[["office"]] == "Governor")
  #x <- x[-c(1, 5:6)]
  return(x)
}

total_vote_func <- function(x) {
  x <- x %>%
    group_by(.data[["office"]]) %>%
    summarize(total_votes = sum(.data[["votes"]]))
  return(x)
}

total_2p_vote_func <- function(x) {
  x <- x %>%
    filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    group_by(.data[["office"]]) %>%
    summarize(total_votes_2p = sum(.data[["votes"]]))
  return(x)
}

vote_join <- function(x, y, z) {
  x <- x %>%
    left_join(y, by = "office") %>%
    left_join(z, by = "office")
  return(x)
}

check_districts <- function(x) {
  x <- as.integer(unique(x$district))
  return(x)
}

# x is the state assembly house data filtered for that year, y is the statewide data but where does x get created? Y gets created by statewide_master

# x = sa_contest (product of sa_contest_all function on my_data)
# y = statewide_master_mi (year requested statewide race info)

district_func_mi <- function(x, y) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  wards_sax_year <- data.frame(precinct = check_precincts(x)) # did not change these variable names
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "precinct") # changed to precinct
  statewide_x_year <- statewide_x_year[-(11:12)] # changed the indices
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  print(names(statewide_x_year))
  print(names(sax_year))
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year)
}

district_func_mi(contested_mi, statewide_mi_2012)

check_precincts <- function(x) { # remade this function to be precincts not wards, put into district_func
  uprecinct <- (unique(x$precinct))
  return(uprecinct)
}

candidate_function <- function(x) {
  cand_x_year <- x %>%
    dplyr::group_by(.data[["candidate"]]) %>%
    dplyr::summarize(cand_total_votes = sum(.data[["votes"]]))
  x <- x %>%
    dplyr::left_join(cand_x_year, by = "candidate") %>%
    dplyr::mutate(prop = .data[["cand_total_votes"]]/.data[["total_votes_2p"]])
  return(x)
} # stays the same


## District baseline func

### - none of these need to change!!

## baseline_data_generate

sa_contest <- sa_contest_all_mi(mi_data)


year_baseline_data_mi <- function(year, data) {
  districts_full <- data.frame(District = 1:110, # changed from 1:99 to 1:110 for all of these
                               Dem_votes = integer(length(1:110)),
                               Rep_votes = integer(length(1:110)),
                               Contested = character(length(1:110)))
  myear <- as.character(year)
  myearm2 <- as.character((year-2))
  myearm4 <- as.character((year-4))

  full_sa_di <- sa_contest_all_mi(data)

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

  un_districts_main_year <- check_districts(uncon_main_year)
  districts <- list()
  ve_list <- list()

  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      dplyr::filter(.data[["district"]] == i) %>%
      dplyr::select(-c("contest_r", "contest_d", "contested"))
    dis_name <- as.character(i)
    main_year <- district_func_mi(temp, statewide_main_year) # district func mi
    mainyearminus2 <- district_func_mi(temp, statewide_main_minus_two) # district_func_mi
    mainyearminus4 <- district_func_mi(temp, statewide_main_minus_four) # district_func_mi
    print(names(main_year))
    print(names(mainyearminus2))
    print(names(mainyearminus4))
    #districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
    #districts[[dis_name]][["estimates"]] <- dis_baseline_ve(i, districts[[dis_name]][["data"]])
    #districts_full[i, ] <- districts[[dis_name]][["estimates"]]
  }
  return(districts_full)
}

efficiency_gap_mi <- function(full_votes, year) { #changed the default table
  mi_sa <- data.frame(Year = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(110, 110, 110, 110, 110, 110, 110, 110),
                      Dem_seats = c(67, 46, 51, 47, 47, 52, 52, 56),
                      Rep_seats = c(43, 64, 59, 63, 63, 58, 58, 54)
  )
  myear <- as.character(year)
  wi_sa_year <- mi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  amended <- vote_prep(full_votes) %>%
    dplyr::mutate(MI = "MI") %>% #changed from wi to mi
    dplyr::group_by(.data[["MI"]]) %>%
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

efficiency_gap_contested_mi <- function(full_votes, year) {
  mi_sa <- data.frame(Year = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(110, 110, 110, 110, 110, 110, 110, 110),
                      Dem_seats = c(67, 46, 51, 47, 47, 52, 52, 56),
                      Rep_seats = c(43, 64, 59, 63, 63, 58, 58, 54)
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
    dplyr::mutate(MI = "MI") %>%
    dplyr::group_by(.data[["MI"]]) %>%
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
