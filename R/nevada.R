## NEVADA

## NEVADA

library(rio)

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

sa_contest_all_co <- function(data){ #original sa_contest_all
  sa_contest_dfs<- list()
  for(i in seq(2004, 2022, 2)){ #mod range to 2004
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_co(year_data)
  }
  return(sa_contest_dfs)
}

nv_2008 <- access_state_year("2008", nv_data)
nv_cand_08 <- nv_cand %>%
  filter(year == 2008)

nv_08 <- nv_2008 %>%
  right_join(nv_cand_08, by=c("candidate", "office", "year", "district"))

sa_contest_all_nv <- function(data){ #mod function name
  sa_contest_dfs<- list()
  for(i in seq(2004, 2022, 2)){ #mod range to 2004
    year <- toString(i)
    year_data <- access_state_year(year, data)
    yearnum <- as.character(unique(year_data$year))

    cand_year <- nv_cand %>%
      filter(year == yearnum)
    joined <- year_data %>%
      right_join(cand_year, by=c("candidate", "office", "year", "district"))

    joined$contest_dem <- as.integer(length(joined))
    joined$contest_rep <- as.integer(length(joined))

    joined$contest_dem <- ifelse(joined$party == "DEM", 1, 0)
    joined$contest_rep <- ifelse(joined$party == "REP", 1, 0)
    sa_contest_dfs[[year]] <- contest_di_co(year_data) ## using same contest_di as CO
  }
  return(sa_contest_dfs)
}

contested_nv <- sa_contest_all_nv(nv_data)

# nv_2022 <- access_state_year("2022", nv_data)
# cand_nv_2022 <- as.list(unique(nv_2022$candidate))

## How am I going to deal with 2018, 2020, 2022


nv_2018 <- nv_data[[8]]
nv_2020 <- nv_data[[9]]
nv_2022 <- nv_data[[10]]

cand_2018 <- as.data.frame(unique(nv_2018$candidate))
cand_2020 <- as.data.frame(unique(nv_2020$candidate))
cand_2022 <- as.data.frame(unique(nv_2022$candidate))

for (i in seq_along(cand_2018)) {
  cand_2018[i] <- str_to_lower(cand_2018[i])
  return(cand_2018)
}

## this worked for 2018 but not 2020 nor 2022 :/

for (i in seq_along(cand_2020)) {
  cand_2020[i] <- str_to_upper(cand_2020[i])
  return(cand_2020)
}

for (i in seq_along(cand_2022)) {
  cand_2022[i] <- str_to_lower(cand_2022[i])
  return(cand_2022)
}



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
