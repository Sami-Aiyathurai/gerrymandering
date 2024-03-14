## NEVADA

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

nv_2022 <- access_state_year("2022", nv_data)
cand_nv_2022 <- as.list(unique(nv_2022$candidate))






nv_2008_sh_parties <- data.frame(
  candidate = c("Marilyn Kirkpatrick", "Linda West Myers",
                "Carlos Blumberg", "John Hambrick",
                "Peggy Pierce", "Eric Morelli",
                "Craig Ballew", "Richard McArthur",
                "Marilyn Dondero Loop", "Donna Toussaint",
                "Harvey Munford", "LisaMarie Johnson",
                "Morse Arberry, Jr", "Geraldine Lewis",
                "Barbara Buckley", "Kevin Child",
                "Richard 'Tick' Segerblom", "Jefferson Lee",
                "Joe Hogan", "Mitch Hostmeyer",
                "Ruben Kihuen", "Ken Upp",
                "James Ohrenschall", "Dallas Augustine",
                ),
  party = c("DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP",
            "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP",
            "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP",
            "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP",
            "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP",
            "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP", "DEM", "REP",
            "DEM", "REP", "DEM", "REP", "DEM", "REP"

            ),
  district = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
               9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14,
               15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20,
               21, 21, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26,
               27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32,
               33, 33, 34, 34, 35, 35, 36, 36, 37, 37, 38, 38,
               39, 39, 40, 40, 41, 41, 42, 42), --- 84 values
  office = c("State House", "State House")
)

nv_2008_sh <- data.frame(candidate = length(1:42),
                         party = length(1:42),
                         district = length(1:42),
                         office = "State House"
)
