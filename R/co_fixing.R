## CO pt 2

## Colorado!!

variable_prep <- function(data) {
  data$party[data$party == "Democratic"] <- "DEM"
  data$party[data$party == "Democrat"] <- "DEM"
  data$party[data$party == "Democratic Party"] <- "DEM"
  data$party[data$party == "Republican"] <- "REP"
  data$party[data$party == "Republican Party"] <- "REP"
  data$party[data$party == "Dem"] <- "DEM"
  data$party[data$party == "Rep"] <- "REP"
  data$office[data$office == "State Representative"] <- "State House"
  data$office[data$office == "State Assembly"] <- "State House"
  data$office[data$office == "Senate"] <- "U.S. Senate"
  data$office[data$office == "US Senate"] <- "U.S. Senate"
  data$precinct <- str_to_lower(data$precinct)
  data$county <- str_to_lower(data$county)
  data$county <- str_squish(data$county)
  data$precinct <- str_squish(data$precinct)
  data$precinct <- as.character(data$precinct)
  data$votes <- as.integer(data$votes)
  data$cw_concat <- paste(data$county, data$precinct, sep=" ")
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district) # do this to catch 2008 where it's a character
  data <- data %>%
    select(county, precinct, office, district, party, candidate, votes, cw_concat) %>%
    filter(office == "State House" | office == "President" | office == "U.S. Senate" |
             office == "Governor" | office == "Secretary of State" | office == "Attorney General")
  return(data)
}

open_elections_factory_co <- function(state) { # 2004-2022
  dates = c("2004"="20041106", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "co"){ # changed the state and __precinct
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  } # changed 2004 date from 1102 to 1106

  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- utils::read.csv(url)
    for (district in data) {
      data <- variable_prep(data)
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)
    }
    data
  }
}

generate_data_co <- function(oe_data){
  dfs <- list()
  for(i in seq(2004, 2022, 2)){
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}

## CHECK CO STRUCTURES

co_data <- open_elections_factory_co("co")
co_data <- generate_data_co(co_data)

## CHECK OFFICES

## CHECK 2010 CO there's 14 variables where there should be 10, 2014 has 12, 2020 has 12
# need to recode the contest_dem and contest_rep

# uncontested mods

sa_contest_all_co <- function(data){ #mod function name
  sa_contest_dfs<- list()
  for(i in seq(2004, 2022, 2)){ #mod range to 2004
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di(year_data)
  }
  return(sa_contest_dfs)
}

contested_co <- sa_contest_all_co(co_data)

# can use the same filtering as MI (for statewide_master and filter_statewide)

# need to figure out what to cut off

co_2010 <- access_state_year("2010", co_data)
co_2008 <- access_state_year("2008", co_data)
statewide_co_2008 <- statewide_master_mi(co_2008)

## use the same district_func_precincts


year_baseline_data_co <- function(year, data) {
  districts_full <- data.frame(District = 1:65, # changed from 1:99 to 1:65 for all of these
                               Dem_votes = integer(length(1:65)),
                               Rep_votes = integer(length(1:65)),
                               Contested = character(length(1:65)))
  myear <- as.character(year)
  myearm2 <- as.character((year-2))
  myearm4 <- as.character((year-4))

  full_sa_di <- sa_contest_all_co(data) #mod

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

  un_districts_main_year <- check_districts(uncon_main_year) # this is producing a list like it should (yay)
  districts <- list()
  ve_list <- list()

  for (i in un_districts_main_year) {
    temp <- uncon_main_year %>%
      dplyr::filter(.data[["district"]] == i) %>%
      dplyr::select(-c("contest_r", "contest_d", "contested"))
    dis_name <- as.character(i)
    main_year <- district_func_precincts(temp, statewide_main_year) # district func mi
    mainyearminus2 <- district_func_precincts(temp, statewide_main_minus_two) # district_func_mi
    mainyearminus4 <- district_func_precincts(temp, statewide_main_minus_four) # district_func_mi
    districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
    districts[[dis_name]][["estimates"]] <- dis_baseline_ve(i, districts[[dis_name]][["data"]])
    districts_full[i, ] <- districts[[dis_name]][["estimates"]]
  }
  return(districts_full)
}

ybdco12 <- year_baseline_data_co(2012, co_data)


co_2008 <- co_data[[3]]
co_2010 <- co_data[[4]]
co_2012 <- co_data[[5]]
co_2014 <- co_data[[6]]
co_2016 <- co_data[[7]]
co_2018 <- co_data[[8]]
co_2020 <- co_data[[9]]
co_2022 <- co_data[[10]]

denver2022 <- co_2022 %>%
  filter(county == "denver")

denver2020 <- co_2020 %>%
  filter(county=="denver")

dprec <- rbind(denver2020, denver2022)

dprec %>%
  distinct(precinct)
