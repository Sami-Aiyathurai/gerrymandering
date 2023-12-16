#generates data
open_elections_factory <- function(state) {
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  if (state == "mi"){
    temp2 <- paste("__",state,"__general__precinct.csv", sep = "")
  }else {
    temp2 <- paste("__",state,"__general__ward.csv", sep = "")
  }

  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , year, temp3)
    data <- read.csv(url)
    for (district in data) {
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year) # need to create this because it doesn't save year as variable bc each set is separate
    }
    data
  }
}

oe_data_WI <- open_elections_factory("wi")
oe_data_MI <- open_elections_factory("mi")

generate_data <- function(){
  dfs_wi <- list()
  dfs_mi <- list()
  for(i in seq(2000, 2020, 2)){
    year <- toString(i)
    dfs_wi[[year]] <- oe_data_WI(year)
    dfs_mi[[year]] <- oe_data_MI(year)
  }
  dfs <- list()
  dfs[["wi"]] <- dfs_wi
  dfs[["mi"]] <- dfs_mi
  return(dfs)
}
state_data <- generate_data()

#generating full wisonsin 2000 sa data
wi_data_2000_sa <- state_data[["wi"]][["2000"]]%>%
  filter(party == "DEM" | party == "REP") %>%
  filter(office == "State Assembly")

test_wi_2000_sa <- wi_data_2000_sa %>%
  group_by(district)  %>%
  summarize(contest_d = sum(contest_dem), contest_r = sum(contest_rep))

for (district in test_wi_2000_sa) {
  test_wi_2000_sa$contested <- ifelse((test_wi_2000_sa$contest_d == 0) | (test_wi_2000_sa$contest_r == 0), "uncontested", "contested")
}

full_wi_2000_sa <- wi_data_2000_sa %>% left_join( test_wi_2000_sa, by=c('district'))

#generating full wisonsin 2010 sa data
wi_data_2010_sa <- state_data[["wi"]][["2010"]]%>%
  filter(party == "DEM" | party == "REP") %>%
  filter(office == "State Assembly")

test_wi_2010_sa <- wi_data_2010_sa %>%
  group_by(district)  %>%
  summarize(contest_d = sum(contest_dem), contest_r = sum(contest_rep))

for (district in test_wi_2010_sa) {
  test_wi_2010_sa$contested <- ifelse((test_wi_2010_sa$contest_d == 0) | (test_wi_2010_sa$contest_r == 0), "uncontested", "contested")
}

full_wi_2010_sa <- wi_data_2010_sa %>% left_join( test_wi_2010_sa, by=c('district'))








test_wi_2000_house <- state_data[["wi"]][["2000"]] %>%
  filter(party == "DEM" | party == "REP") %>%
  filter(office == "House") %>%
  group_by(district)  %>%
  summarize(contest_d = sum(contest_dem), contest_r = sum(contest_rep))



test_wi_2002 <- state_data[["wi"]][["2002"]] %>%
  filter(party == "DEM" | party == "REP") %>%
  filter(office == "House") %>%
  group_by(district)  %>%
  summarize(contest_d = sum(contest_dem), contest_r = sum(contest_rep))
#sum is 0 if uncontested otherwise its equal to number of wards in district

for (district in test_wi_2000_house) {
  test_wi_2000_house$contested <- ifelse((test_wi_2000_house$contest_d == 0) | (test_wi_2000_house$contest_r == 0), "uncontested", "contested")
}

for (district in test_wi_2002) {
  test_wi_2002$contested <- ifelse((test_wi_2002$contest_d == 0) | (test_wi_2002$contest_r == 0), "uncontested", "contested")
}



wi_contested_2000 <- split(test_wi_2000, test_wi_2000$contested)
wi_contested_2002 <- split(test_wi_2002, test_wi_2002$contested)


#takes string of state and year and the full dataframe and returns a year of a states data
access_state_year <- function(state, year, data){
  state_year <- data[[state]][[year]]
  return(state_year)
}

# takes a year and states data from full dataframe and returns a list of two with contested districts and uncontested districts
contest_di <- function(year_data){
  sa_data <- year_data %>%
    filter(party == "DEM" | party == "REP") %>%
    filter(office == "State Assembly")
  di_data_sa <- sa_data %>%
    group_by(district)  %>%
    summarize(contest_d = sum(contest_dem), contest_r = sum(contest_rep))
  for (district in di_data_sa) {
    di_data_sa$contested <- ifelse((di_data_sa$contest_d == 0) | (di_data_sa$contest_r == 0), "uncontested", "contested")
  }
  sa_di <- sa_data %>% left_join(di_data_sa, by=c('district'))
  full_sa_di <- split(sa_di, sa_di$contested)

  return(full_sa_di)
}

#generates list of each year with a list of contested districts and uncontested districts for all of WI
sa_contest_all_wi <- function(){
  sa_contest_dfs_wi <- list()
  for(i in seq(2000, 2020, 2)){
    year <- toString(i)
    year_data <- access_state_year("wi", year, data)
    sa_contest_dfs_wi[[year]] <- contest_di(year_data)
  }
  return(sa_contest_dfs_wi)
}

#runs and saves to variable
wi_full_sa_di <- sa_contest_all_wi()

