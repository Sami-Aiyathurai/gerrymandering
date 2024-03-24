## MERGE ISSUES SCRIPT

## Colorado!!

## Check data functions

## Taking a break from CO: there's data prep work that needs to be done to make every year the same
## removing certain columns, renaming variables --> what's the best place do do that??

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

# 2004: 10 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num)
# 2006: 10 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num)
# 2008: 10 vars -- county (chr), precinct (chr), office (chr), district (chr), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num)
# 2010: 14 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num), poll_votes, mail_votes, early_votes, provisional_votes
# where votes = the others
# 2012: 10 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num)
# 2014: 12 vars -- county (chr), precinct (chr), office (chr), district (num), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num), yes_votes, no_votes
# 2016: 10 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num)
# 2018: 10 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num)
# 2020: 12 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num), yes_votes(int), no_votes(int)
# 2022: 10 vars -- county (chr), precinct (chr), office (chr), district (int), party (chr), candidate (chr), votes (int), contest_dem (num), contest_rep (num), year (num)

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
  data$precinct <- as.character(data$precinct)
  data$votes <- as.integer(data$votes)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district) # do this to catch 2008 where it's a character
  data <- data %>%
    select(county, precinct, office, district, party, candidate, votes)
  return(data)
}



## CHECK OFFICES


## CHECK 2010 CO there's 14 variables where there should be 10, 2014 has 12, 2020 has 12
# need to recode the contest_dem and contest_rep

# uncontested mods

contest_di_co <- function(year_data){
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

precincts1 <- check_precincts(co_data[[1]])
precincts2 <- check_precincts(co_data[[2]])
precincts3 <- check_precincts(co_data[[3]])
precincts4 <- check_precincts(co_data[[4]])
precincts5 <- check_precincts(co_data[[5]])
precincts6 <- check_precincts(co_data[[6]])
precincts7 <- check_precincts(co_data[[7]])
precincts8 <- check_precincts(co_data[[8]])
precincts9 <- check_precincts(co_data[[9]])
precincts10 <- check_precincts(co_data[[10]])

sa_contest_all_co <- function(data){ #mod function name
  sa_contest_dfs<- list()
  for(i in seq(2004, 2022, 2)){ #mod range to 2004
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_co(year_data)
  }
  return(sa_contest_dfs)
}

co_data <- open_elections_factory_co("co")
co_data <- generate_data_co(co_data)
co_2004 <- access_state_year("2004", co_data)
contested_co <- sa_contest_all_co(co_data)

# can use the same filtering as MI (for statewide_master and filter_statewide)

# need to figure out what to cut off

co_2010 <- access_state_year("2010", co_data)
co_2008 <- access_state_year("2008", co_data)
statewide_co_2008 <- statewide_master_mi(co_2008)

co_8_2008 <- co_2008 %>%
  filter(office == "State House") %>%
  filter(district == 8)

df_co_2008 <- district_func_co(co_8_2008, statewide_co_2008)
df_wi_2010 <- district_func(wi_3_2010, statewide_wi_2010)

district_func_co <- function(x, y) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    dplyr::filter(.data[["office"]] == "State House")
  wards_sax_year <- data.frame(precinct = check_precincts(x))
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "precinct") # changed to precinct
  statewide_x_year <- statewide_x_year[-(11:12)] # changed the indices
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") #%>%
  #dplyr::filter(.data[["office"]] == "State House" | .data[["office"]] == "State Representative")
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year)
  return(district_x_year)

}

co_2008 <- year_baseline_data_co(2008, co_data)
co_2010 <- year_baseline_data_co(2010, co_data)

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
    main_year <- district_func_co(temp, statewide_main_year) # district func mi
    mainyearminus2 <- district_func_co(temp, statewide_main_minus_two) # district_func_mi
    mainyearminus4 <- district_func_co(temp, statewide_main_minus_four) # district_func_mi
    districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
    districts[[dis_name]][["estimates"]] <- dis_baseline_ve(i, districts[[dis_name]][["data"]])
    districts_full[i, ] <- districts[[dis_name]][["estimates"]]
  }
  return(districts_full)
}

prelim_co_2008 <- year_baseline_data_co(2008, co_data)
prelim_wi_2010 <- year_baseline_data(2010, wi_data)

efficiency_gap_co <- function(full_votes, year) { #changed the default table
  mi_sa <- data.frame(Year = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(65, 65, 65, 65, 65, 65, 65, 65),
                      Dem_seats = c(37, 32, 37, 34, 37, 41, 41, 46),
                      Rep_seats = c(28, 33, 28, 31, 28, 24, 24, 19)
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

efficiency_gap_contested_co <- function(full_votes, year) {
  mi_sa <- data.frame(Year = c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(65, 65, 65, 65, 65, 65, 65, 65),
                      Dem_seats = c(37, 32, 37, 34, 37, 41, 41, 46),
                      Rep_seats = c(28, 33, 28, 31, 28, 24, 24, 19)
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

## NEVADA

## NEVADA

nv_cand <- import("nv_candidates.csv")
nv_cand <- nv_cand %>%
  select(year, office, district, candidate, party)
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
    for (district in data) {
      #data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      #data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)
    }
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

access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}

sa_contest_all_nv <- function(data){ #mod function name
  sa_contest_dfs<- list()
  for(i in seq(2004, 2022, 2)){ #mod range to 2004
    year <- toString(i)
    year_data <- access_state_year(year, data)
    yearnum <- as.character(unique(year_data$year))
    print(yearnum)
    year_data$contest_dem <- as.integer(length(year_data))
    year_data$contest_rep <- as.integer(length(year_data))
    year_data <- nv_cand %>% # overwriting year data with joined candidate data for that year
      filter(year == yearnum) %>%
      full_join(year_data) %>%
      mutate(votes = ifelse(is.na(votes), 0, votes))
    year_data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
    year_data$contest_rep <- ifelse(data$party == "REP", 1, 0)
    # print(str(year_data))
    # sa_contest_dfs[[year]] <- contest_di_co(year_data) ## using same contest_di as CO
  }
  return(year_data)
  #return(sa_contest_dfs)
}

contested_nv <- sa_contest_all_nv(nv_data)

# nv_2022 <- access_state_year("2022", nv_data)
# cand_nv_2022 <- as.list(unique(nv_2022$candidate))

## Matching Candidates! this is only going through 2016 for now bc of formatting issues

nv_2004 <- access_state_year("2004", nv_data)
nv_cand_2004 <- nv_cand %>%
  filter(year == 2004) %>%
  full_join(nv_2004)

nv_2006 <- access_state_year("2006", nv_data)
nv_cand_2006 <- nv_cand %>%
  filter(year == 2006) %>%
  full_join(nv_2006)

nv_2008 <- access_state_year("2008", nv_data)
nv_cand_2008 <- nv_cand %>%
  filter(year == 2008) %>%
  full_join(nv_2008)

nv_2010 <- access_state_year("2010", nv_data)
nv_cand_2010 <- nv_cand %>%
  filter(year == 2010) %>%
  full_join(nv_2010)

nv_2012 <- access_state_year("2012", nv_data)
nv_cand_2012 <- nv_cand %>%
  filter(year == 2012) %>%
  full_join(nv_2012)

nv_2014 <- access_state_year("2014", nv_data)
nv_cand_2014 <- nv_cand %>%
  filter(year == 2014) %>%
  full_join(nv_2014)

nv_2016 <- access_state_year("2016", nv_data)
nv_cand_2016 <- nv_cand %>%
  filter(year == 2016) %>%
  full_join(nv_2016)

nv2_data <- rbind(nv_cand_2004, nv_cand_2006, nv_cand_2008, nv_cand_2010, nv_cand_2012,
                  nv_cand_2014, nv_cand_2016)
nv2_data <- nv2_data %>%
  mutate(votes = ifelse(is.na(votes), 0, votes))

## Uncontested mods


## WISCONSIN

open_elections_factory <- function(state) {
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
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
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
    dplyr::filter(.data[["office"]] == "State Assembly")
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

wi_data <- open_elections_factory("wi")
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
    filter(.data[["office"]] == "Senate" | .data[["office"]] == "President" | .data[["office"]] == "Attorney General" |
             .data[["office"]] == "Secretary of State" | .data[["office"]] == "Governor")
  #x <- x[-c(1, 5:6)]
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
district_func <- function(x, y) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  wards_sax_year <- data.frame(ward = check_wards(x))
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "ward")
  statewide_x_year <- statewide_x_year[-(12:13)] # this index chops off the total_votes and total_votes_2p columns!!
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

year_baseline_data(2010, wi_data)

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


## Preliminary modeling

states <- import("StatesAndCyclesData_production-20240301a.csv")
egs$Year <- as.numeric(egs$Year)
egs$Year <- as.integer(egs$Year)


mod_state <- states %>%
  select(c(State, `Cycle Year`, Level, Seats, Institution, `Party Control`, Governor)) %>%
  filter(Level == "State Lower") %>%
  rename("Year" = "Cycle Year")

egs_mod <- egs %>%
  left_join(mod_state, by = c("Year" = "Year", "State" = "State"))

egs_mod <- egs_mod %>%
  distinct()

egs_mod$Level <- "State Lower"
egs_mod$Seats <- ifelse(egs_mod$State == "WI", 99, egs_mod$Seats)
egs_mod$Institution <- ifelse(egs_mod$State == "WI", "Legislature", egs_mod$Institution)
egs_mod$Seats <- ifelse(egs_mod$State == "CO", 65, egs_mod$Seats)
egs_mod$Institution <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2020, "Independent commission", egs_mod$Institution)
egs_mod$Institution <- ifelse(egs_mod$State == "CO" & egs_mod$Year < 2020, "Politician commission", egs_mod$Institution)
egs_mod$Seats <- ifelse(egs_mod$State == "MI", 110, egs_mod$Seats)
egs_mod$Institution <- ifelse(egs_mod$State == "MI" & egs_mod$Year < 2020, "Legislature", egs_mod$Institution)


ggplot(egs_mod, aes(x=Year, y=Efficiency_gap)) +
  geom_point(aes(color=Institution)) +
  scale_x_continuous(breaks=seq(2008, 2022, 2),
                     labels=c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022")) +
  ylab("Efficiency gap") +
  scale_y_continuous(breaks=seq(-0.24, 0.16, 0.08),
                     labels=c("-0.24", "-0.16", "-0.08", "0", "0.08", "0.16")) +
  facet_wrap(~State)

## need to note when each group passed maps -- eg, if independent commission was passed in 2018 but their maps
# weren't into effect until 2022 then that's necessary to note

ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_point(aes(color = State)) +
  ylab("Efficiency Gap") +
  ggtitle("Scatterplot of state house efficiency gap over time")+
  scale_y_continuous(breaks=seq(-0.16, 0.16, 0.08),
                     labels = c("-0.16", "-0.08", "0", "0.08", "0.16"))

ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_point() +
  facet_wrap(~State) +
  ggtitle("Efficiency gaps for Colorado, Michigan, and Wisconsin over time") +
  ylab("Efficiency Gap") +
  scale_y_continuous(breaks=seq(-0.16, 0.16, 0.08),
                     labels = c("-0.16", "-0.08", "0", "0.08", "0.16"))


ggplot(egs, aes(x=Efficiency_gap, y=Efficiency_gap_contested)) +
  geom_point(aes(color=State))

ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_line(aes(color=State))


