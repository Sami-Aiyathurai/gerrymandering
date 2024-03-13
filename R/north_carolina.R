open_elections_factory_nc <- function(state) {
  dates = c("2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-results-",state,"/master/raw", sep = "")
  if (state == "nc"){
    temp2 <- paste("__",state,"__general__precinct__raw.csv", sep = "")
  }else {
    stop("This package does not have the functionality for state: ", state)
  }

  read <- function(year){
    date <- dates[year]
    temp3 <- paste(date,temp2, sep = "")
    url <- file.path(temp1 , temp3) # removed the second year from factory (it works for others but not this??)
    data <- utils::read.csv(url)
    for (district in data) {
      data <- variable_prep_nc(data)
      data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
      data$contest_rep <- ifelse(data$party == "REP", 1, 0)
      data$year <- as.numeric(year)
    }
    data
  }
}

generate_data_nc <- function(oe_data){
  dfs <- list()
  for(i in seq(2004, 2020, 2)){ # changed to 2020
    year <- toString(i)
    dfs[[year]] <- oe_data(year)
  }
  return(dfs)
}

access_state_year <- function(year, data){
  state_year <- data[[year]]
  return(state_year)
}

nc_data <- open_elections_factory_nc("nc")
nc_data <- generate_data_nc(nc_data)

## ORGANIZING DATA

variable_prep_nc <- function(data) {
  data$office[data$office == "NC STATE HOUSE"] <- "State House"
  data$office[data$office == "NC HOUSE OF REPRESENTATIVES"] <- "State House"
  data$office[data$office == "US SENATE"] <- "U.S. Senate"
  data$office[data$office == "US Senate"] <- "U.S. Senate"
  data$office[data$office == "PRESIDENT"] <- "President"
  data$office[data$office == "PRESIDENT AND VICE PRESIDENT OF THE UNITED STATES"] <- "President"
  data$office[data$office == "GOVERNOR"] <- "Governor"
  data$office[data$office == "NC GOVERNOR"]
  data$office[data$office == "SECRETARY OF STATE"] <- "Secretary of State"
  data$office[data$office == "ATTORNEY GENERAL"] <- "Attorney General"
  data$office[data$office == "NC ATTORNEY GENERAL"] <- "Attorney General"
  data <- data %>%
    select(office, district, name_raw, party, parent_jurisdiction, jurisdiction, division, votes)
  return(data)
}
# use contest_di_mi

sa_contest_all_nc <- function(data){
  sa_contest_dfs<- list()
  for(i in seq(2004, 2020, 2)){ # mod range
    year <- toString(i)
    year_data <- access_state_year(year, data)
    sa_contest_dfs[[year]] <- contest_di_mi(year_data)
  }
  return(sa_contest_dfs)
}

contested_nc <- sa_contest_all_nc(nc_data)


### ALERT: NC DOES NOT HAVE STATE HOUSE DATA FOR 2008-2012; they do for 2014-2020 (calculate values for 2018, 2020)
