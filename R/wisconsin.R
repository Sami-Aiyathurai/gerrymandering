## WISCONSIN

library(tidyverse)
library(stringr)
library(stringi)
library(forstringr)
library(redist)
library(geomander)
library(alarmdata)

wi_2012 <- wi_data[[7]]
write.csv(wi_2012, "C:\\Users\\mzelloe\\Documents\\wi_2012.csv", row.names=FALSE)


## WISCONSIN

wi_prep <- function(data) {
  data$office[data$office == "Senate"] <- "U.S. Senate"
  data$office[data$office == "State Assembly"] <- "State House"
  data <- data %>%
    filter(office == "U.S. Senate" | office == "President" | office == "State House" |
             office == "Attorney General" | office == "Secretary of State" |
             office == "Governor")
  data$ward <- str_to_lower(data$ward)
  data$county <- str_to_lower(data$county)
  data$cw_concat <- paste(data$county, data$ward, sep=" ")
  data$cw_concat <- stri_replace_all_regex(data$cw_concat,
                                           pattern=c("ward", "wds", "wd", "wards"),
                                           replacement="precinct",
                                           vectorize=FALSE)
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)
  return(data)
}

open_elections_factory_wi <- function(state) {
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "wi"){
    temp2 <- paste("__",state,"__general__ward.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  }

  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- utils::read.csv(url)
    for (district in data) {
      data <- wi_prep(data)
      data$year <- as.numeric(year)

    }
    data
  }
}

generate_data <- function(oe_data){
  dfs <- list()
  for(i in seq(2000, 2022, 2)){ # changed to include 2022
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}

# takes dataframe from access_state_year
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
# param is full data from generate data
sa_contest_all<- function(data){
  sa_contest_dfs<- list()
  for(i in seq(2000, 2022, 2)){ # changed to include 2022
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di(year_data)
  }
  return(sa_contest_dfs)
}

wi_data <- open_elections_factory_wi("wi")
wi_data <- generate_data(wi_data)
contested_wi <- sa_contest_all(wi_data)

## run districts_baseline_func (none of these get mod)

## data cleaning uncontested

#' @param x data frame created by open_elections_factory and generate_data
statewide_master <- function(x) {
  x <- filter_statewide(x)
  y <- total_vote_func(x)
  z <- total_2p_vote_func(x)
  x <- vote_join(x, y, z)
  return(x)
}

#' @param x data frame created by open_elections_factory and generate_data
filter_statewide <- function(x) {
  x <- x %>%
    filter(.data[["office"]] == "U.S. Senate" | .data[["office"]] == "President" | .data[["office"]] == "Attorney General" |
             .data[["office"]] == "Secretary of State" | .data[["office"]] == "Governor")
  return(x)
}

#' @param x data frame created by open_elections_factory and generate_data
total_vote_func <- function(x) {
  x <- x %>%
    group_by(.data[["office"]]) %>%
    summarize(total_votes = sum(.data[["votes"]]))
  return(x)
}

#' @param x data frame created by open_elections_factory and generate_data
total_2p_vote_func <- function(x) {
  x <- x %>%
    filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    group_by(.data[["office"]]) %>%
    summarize(total_votes_2p = sum(.data[["votes"]]))
  return(x)
}

#' all parties and two parties.
#' @param x data frame created by open_elections_factory and generate_data
#' @param y data frame created by total_vote_func
#' @param z data frame created by total_vote_2p_func
vote_join <- function(x, y, z) {
  x <- x %>%
    left_join(y, by = "office") %>%
    left_join(z, by = "office")
  return(x)
}

#' @param x data frame returned by the contested or uncontested state assembly data
check_districts <- function(x) {
  x <- as.integer(unique(x$district))
  return(x)
}

#' @param x data frame of state assembly by district, as established by for loops
#' @param y data frame of statewide data for the given year
#' MODIFied to negative select
district_func <- function(x, y) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  wards_sax_year <- data.frame(ward = check_wards(x))
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "ward")
  statewide_x_year <- statewide_x_year %>%
    dplyr::select(-c(total_votes, total_votes_2p))# this index chops off the total_votes and total_votes_2p columns!!
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year)
  return(district_x_year)

}

#' @param x data frame of wards in a given state assembly district
check_wards <- function(x) {
  uwards <- (unique(x$ward))
  return(uwards)
}

#' @param x data frame of state assembly district data
candidate_function <- function(x) {
  cand_x_year <- x %>%
    dplyr::group_by(.data[["candidate"]]) %>%
    dplyr::summarize(cand_total_votes = sum(.data[["votes"]]))
  x <- x %>%
    dplyr::left_join(cand_x_year, by = "candidate") %>%
    dplyr::mutate(prop = .data[["cand_total_votes"]]/.data[["total_votes_2p"]])
  return(x)
}

year_baseline_data <- function(year, data) {
  districts_full <- data.frame(District = 1:99,
                               Dem_votes = integer(length(1:99)),
                               Rep_votes = integer(length(1:99)),
                               Contested = character(length(1:99)))

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
    districts[[dis_name]][["estimates"]] <- dis_baseline_ve(i, districts[[dis_name]][["data"]])
    districts_full[i, ] <- districts[[dis_name]][["estimates"]]
  }
  return(districts_full)
}

ybd_2010 <- year_baseline_data(2010, wi_data)
ybd_2012 <- year_baseline_data(2012, wi_data)
## make Sami fix this

#' @param full_votes data frame produced by the year_baseline_data function
vote_prep <- function(full_votes) {
  total_votes <- full_votes %>%
    dplyr::group_by(.data[["District"]]) %>%
    dplyr::summarize(total_votes = sum(.data[["Dem_votes"]], .data[["Rep_votes"]]))
  full_votes <- full_votes %>%
    dplyr::left_join(total_votes, by = "District")
  return(full_votes)
}

#' @param full_votes data frame as made by year_baseline_data function
#' @param year a numeric vector as inputted by the user within the year range 2006-2020
#' @export
efficiency_gap <- function(full_votes, year) {
  wi_sa <- data.frame(Year = c("2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(99, 99, 99, 99, 99, 99, 99, 99, 99),
                      Dem_seats = c(47, 52, 39, 39, 39, 36, 35, 38, 35),
                      Rep_seats = c(52, 47, 60, 60, 60, 63, 64, 61, 64)
  )
  myear <- as.character(year)
  wi_sa_year <- wi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  amended <- vote_prep(full_votes) %>%
    dplyr::mutate(WI = "WI") %>%
    dplyr::group_by(.data[["WI"]]) %>%
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

#' @param full_votes A data frame of length 99, for each legislative district and
#' and how they voted Democrat or Republican for a given year.
#' @param year A character vector identifying the requested year. The user can
#' input a numeric or integer vector, but the function will transform it into a character.
#' @export
efficiency_gap_contested <- function(full_votes, year) {
  wi_sa <- data.frame(Year = c("2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(99, 99, 99, 99, 99, 99, 99, 99, 99),
                      Dem_seats = c(47, 52, 39, 39, 39, 36, 35, 38, 35),
                      Rep_seats = c(52, 47, 60, 60, 60, 63, 64, 61, 64)
  )
  myear <- as.character(year)
  wi_sa_year <- wi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  full_votes_cont <- full_votes %>%
    dplyr::filter(.data[["Contested"]] == "contested")
  amended <- vote_prep(full_votes_cont) %>%
    dplyr::mutate(WI = "WI") %>%
    dplyr::group_by(.data[["WI"]]) %>%
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

#' @return A numeric vector, between -0.25 and 0.25 that estimates how gerrymandered the
#' state assembly is.
#'
