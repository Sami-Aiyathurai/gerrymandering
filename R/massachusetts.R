## MASSACHUSETTS

open_elections_factory_ma <- function(state) {
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104",
            "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106",
            "2020"="20201103")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "ma"){ # changed the state and __precinct
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
      data <- variable_prep(data)
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)
    }
    data
  }
}

generate_data_ma <- function(oe_data){
  dfs <- list()
  for(i in seq(2000, 2020, 2)){ # mod
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

ma_data <- open_elections_factory_ma("ma")
ma_data <- generate_data_ma(ma_data)

# use contest_di_mi

sa_contest_all_ma <- function(data){
  sa_contest_dfs<- list()
  for(i in seq(2000, 2020, 2)){ # mod for years
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_mi(year_data)
  }
  return(sa_contest_dfs)
}

ma_2004 <- access_state_year("2004", ma_data)
statewide_ma_2004 <- statewide_master_mi(ma_2004)
contested_ma <- sa_contest_all_ma(ma_data)

district_func_ma <- function(x, y) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  wards_sax_year <- data.frame(precinct = check_precincts(x)) # did not change these variable names
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "precinct") # changed to precinct
  print(statewide_x_year)
#  statewide_x_year <- statewide_x_year[-(11:12)] # changed the indices
 # tv_statewide_x_year <- total_vote_func(statewide_x_year)
#  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
#  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
 #   dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
#  district_x_year <- rbind(statewide_x_year, sax_year)
#  district_x_year <- candidate_function(district_x_year)
}

district_func_ma(statewide_ma_2004, contested_ma)
