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
