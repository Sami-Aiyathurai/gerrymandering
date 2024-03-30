## NEVADA

## NEVADA

library(rio)

nv_cand <- import("nv_cand.csv")
nv_cand <- nv_cand %>%
  dplyr::select(year, office, district, candidate, party)
nv_cand$year <- as.numeric(nv_cand$year)

## LOADING DATA

open_elections_factory_nv <- function(state) {
  dates = c("2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "nv"){ # changed the state and __precinct
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  }
  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- utils::read.csv(url)
    #for (district in data) {
      if (as.numeric(year) < 2016) {
        data <- nv_prep(data, year)
      }

    #}
    data
  }
}



generate_data_nv <- function(oe_data){
  dfs <- list()
  for(i in seq(2004, 2022, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

nv_data <- open_elections_factory_nv("nv")
nv_data <- generate_data_nv(nv_data)

nv_prep <- function(nv_year, year) {
  yearc <- as.character(year)
  nv_year$year <- as.numeric(year)
  cands_year <- nv_cand %>%
    filter(year == yearc)
  nv_year <- nv_year %>%
    inner_join(cands_year, by=c("year", "office", "district", "candidate"))
  nv_year$contest_dem <- ifelse(nv_year$party == "DEM", 1, 0)
  nv_year$contest_rep <- ifelse(nv_year$party == "REP", 1, 0)
  nv_year$candidate <- str_to_upper(nv_year$candidate)
  return(nv_year)
}

sa_contest_all_nv <- function(data){ #original sa_contest_all, can also just use the CO one as they both start 2004
  sa_contest_dfs<- list()
  for(i in seq(2004, 2014, 2)){ #mod range to 2004-2014 for now
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_co(year_data)
  }
  return(sa_contest_dfs)
}

nv_contested <- sa_contest_all_co(nv_data)

## copy paste new form of sa_contest_all_nv

contested_nv <- sa_contest_all_nv(nv_data)


## How am I going to deal with 2018, 2020, 2022
nv_2014 <- nv_data[[6]] # names formatted as proper nouns, first name last name
nv_2016 <- nv_data[[7]] # can set to all caps, remove commas between
nv_2018 <- nv_data[[8]] # can set to all caps, remove commas between
nv_2020 <- nv_data[[9]] # can set to all caps, remove commas between
nv_2022 <- nv_data[[10]] # can set to all caps, remove commas between

cand_2018 <- as.data.frame(unique(nv_2018$candidate))
cand_2020 <- as.data.frame(unique(nv_2020$candidate))
cand_2022 <- as.data.frame(unique(nv_2022$candidate))

for (i in seq_along(cand_2018)) {
  cand_2018[i] <- str_to_lower(cand_2018[i])
  return(cand_2018)
}

## this worked for 2018 but not 2020 nor 2022 :/

## I think I'm going to want to grepl this; anything before the space gets moved to front
## anything after the space stays in the back, saving strings

## GOAL MAKE ALL OF IT CAPPED

for (i in seq_along(cand_2020)) {
  cand_2020[i] <- str_to_upper(cand_2020[i])
  return(cand_2020)
}

for (i in seq_along(cand_2022)) {
  cand_2022[i] <- str_to_lower(cand_2022[i])
  return(cand_2022)
}

## OTHER MODS
nv_2008 <- access_state_year("2008", nv_data)
statewide_nv08 <- filter_statewide_mi(nv_2008)

nv08d4 <- nv_2008 %>%
  filter(office == "State House") %>%
  filter(precinct == 130)

## when this is 04 there are some NA values for votes
## there might be something fishy going around with NV precinct values, Clark 130
## is in both HD 21 and 22

district_func_nv <- function(x, y) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    dplyr::filter(.data[["office"]] == "State House")
  wards_sax_year <- data.frame(precinct = check_precincts(x))
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "precinct") # changed to precinct
  print(statewide_x_year)
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
