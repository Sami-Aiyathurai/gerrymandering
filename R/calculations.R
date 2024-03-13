# Mass EG calculations

## Wisconsin 2010-2020

wi_data <- open_elections_factory("wi")
wi_data <- generate_data(wi_data)
wi_contested <- sa_contest_all(wi_data)

variable_prep <- function(data) { # run this within each generate FUNC
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
  return(data)
}

## IN WI: each df has 11 variables: county, ward, office, district, total.votes, party, candidate, votes, contest_dem, contest_rep, year

wisconsin <- function(year, ...) {
  year <- as.character(year)
  year_num <- as.numeric(year)
  #wi_year <- access_state_year(year, wi_data)
  votes_year <- year_baseline_data(year_num, wi_data)
  eg_year <- efficiency_gap(votes_year, year_num)
  eg_con_year <- efficiency_gap_contested(votes_year, year_num)

  wi_year <- data.frame(Year = year,
                        Efficiency_gap = eg_year,
                        Efficiency_gap_contested = eg_con_year,
                        State = "WI")
  return(wi_year)
}

# for some reason wi_2012 doesn't work???

wi_egs <- rbind(wisconsin(2008), wisconsin(2010), wisconsin(2014),
                wisconsin(2016), wisconsin(2018), wisconsin(2020), wisconsin(2022))

## Michigan

mi_data <- open_elections_factory_mi("mi")
mi_data <- generate_data(mi_data)
mi_contested <- sa_contest_all_mi(mi_data)

## MI: each df has 10 variables: county, precinct, office, district, party, candidate, votes, contest_dem, contest_rep, year

michigan <- function(year,...) {
  year <- as.character(year)
  year_num <- as.numeric(year)
  #mi_year <- access_state_year_mi(year, mi_data)
  votes_year <- year_baseline_data_mi(year_num, mi_data)
  eg_year <- efficiency_gap_mi(votes_year, year_num)
  eg_con_year <- efficiency_gap_contested_mi(votes_year, year_num)

  mi_year <- data.frame(Year = year,
                        Efficiency_gap = eg_year,
                        Efficiency_gap_contested = eg_con_year,
                        State = "MI")
  return(mi_year)
}

mi_2022 <- mi_data[[12]]
mi_2022$votes <- as.numeric(mi_2022$votes)
mi_2022$votes <- as.integer(mi_2022$votes)

mi_2022 <- michigan(2022)

## CO

colorado <- function(year, ...) {
  year <- as.character(year)
  year_num <- as.numeric(year)
  votes_year <- year_baseline_data_co(year_num, co_data)
  eg_year <- efficiency_gap_co(votes_year, year_num)
  eg_con_year <- efficiency_gap_contested_co(votes_year, year_num)

  co_year <- data.frame(Year = year,
                        Efficiency_gap = eg_year,
                        Efficiency_gap_contested = eg_con_year,
                        State = "CO")
  return(co_year)
}

colorado(2008) # worked
colorado(2010) # worked
colorado(2012) # did not!!
colorado(2014) # worked
colorado(2016) # worked
colorado(2018) # worked!
colorado(2020) # worked!
colorado(2022) # worked!!

co_egs <- rbind(colorado(2008), colorado(2010), colorado(2014), colorado(2016),
                colorado(2018), colorado(2020), colorado(2022))

## THE PROBLEM WITH 2022 IS THAT THE DATA STRUCTURE ITSELF IS NOT THE SAME AS
# FOR THE OTHER YEARS. IT HAS 2 EXTRA VARIABLES. THIS IS WHY BEFORE ANYTHING
# ELSE CAN HAPPEN I NEED TO PROOF THE DFS TO MAKE SURE THEY ALL HAVE THE SAME STRUCTURE
# AND VARIABLES AND FORMATTING

# I don't think MI 2022 works oh well it's because votes are coded as characters not integers

mi_egs <- rbind(michigan(2008), michigan(2010), michigan(2012), michigan(2014),
                michigan(2016), michigan(2018), michigan(2020))

egs <- rbind(wi_egs, mi_egs, co_egs)
