url <- file.path("https://xkcd.com" , number , "info.0.json")

#get data using changing urls like with xkcd
#read data/make dataframe using techniques from dengue fever
#Wisconsin urls
#We can set the url but the election date changes WI
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2000/20001107__wi__general__ward.csv
#https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2000/NA__wi__general__ward.csv


# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2002/20021105__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2004/20041102__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2006/20061107__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2008/20081104__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2010/20101102__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2012/20121106__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2014/20141104__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2016/20161108__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2018/20181106__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2020/20201103__wi__general__ward.csv
# https://raw.githubusercontent.com/openelections/openelections-data-wi/master/2022/20221108__wi__general__ward.csv
#URLs MI
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2000/20001107__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2002/20021105__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2004/20041102__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2006/20061107__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2008/20081104__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2010/20101102__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2012/20121106__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2014/20141104__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2016/20161108__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2018/20181106__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2020/20201103__mi__general__precinct.csv
# https://raw.githubusercontent.com/openelections/openelections-data-mi/master/2022/20221108__mi__general__precinct.csv


#readhttps://raw.githubusercontent.com/openelections/openelections-data-wi/master/2016/20161108__wi__general__ward.csv


data_WI <- function(year, date){
  temp <- paste(date,"__wi__general__ward.csv", sep = "")
  url <- file.path("https://raw.githubusercontent.com/openelections/openelections-data-wi/master" , year, temp)
  base_data <- read.csv(url)
  data <- base_data %>%
    group_by(district, office, party) %>%
    summarize(cand_votes = sum(votes))

  for (district in data) {
    data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
    data$contest_rep <- ifelse(data$party == "REP", 1, 0)
    data$year <- as.numeric(year) # need to create this because it doesn't save year as variable bc each set is separate
  }
  data
}

test_wi <- data_WI(2000, 20001107)


test_url <- function(state, year){
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
  temp1 <-paste("https://raw.githubusercontent.com/openelections/openelections-data-",state,"/master", sep = "")
  temp2 <- paste("__",state,"__general__ward.csv", sep = "")
  date <- dates[year]
  temp3 <- paste(date,temp2, sep = "")
  url <- file.path(temp1 , year, temp3)
  base_data <- read.csv(url)
  return(base_data)
}

testing <- test_url("mi", "2000")



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
      base_data <- read.csv(url)
      data <- base_data %>%
        group_by(district, office, party) %>%
        summarize(cand_votes = sum(votes))

      for (district in data) {
        data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
        data$contest_rep <- ifelse(data$party == "REP", 1, 0)
        data$year <- as.numeric(year) # need to create this because it doesn't save year as variable bc each set is separate
      }
      data
    }
}



# offices we care about: President, State Attorney General, State Assembly, State Senate, Senate, House
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


#ask whopper or grace for way to automate
wi_2000 <- oe_data_WI("2000")
wi_2002 <- oe_data_WI("2002")
wi_2004 <- oe_data_WI("2004")
wi_2006 <- oe_data_WI("2006")
wi_2008 <- oe_data_WI("2008")
wi_2010 <- oe_data_WI("2010")
wi_2012 <- oe_data_WI("2012")
wi_2014 <- oe_data_WI("2014")
wi_2016 <- oe_data_WI("2016")
wi_2018 <- oe_data_WI("2018")
wi_2020 <- oe_data_WI("2020")
wi_2022 <- oe_data_WI("2022")

mi_2000 <- oe_data_MI("2000")
mi_2002 <- oe_data_MI("2002")
mi_2004 <- oe_data_MI("2004")
mi_2006 <- oe_data_MI("2006")
mi_2008 <- oe_data_MI("2008")
mi_2010 <- oe_data_MI("2010")
mi_2012 <- oe_data_MI("2012")
mi_2014 <- oe_data_MI("2014")
mi_2016 <- oe_data_MI("2016")
mi_2018 <- oe_data_MI("2018")
mi_2020 <- oe_data_MI("2020")
#mi_2022 <- oe_data_MI("2022")

# for (i in 2000:2022:2) {
# wi_i # there should be a way to do the index as the df name
#code
# }

  for(i in seq(2000, 2022, 2)){
    print(i)
  }


#Idea for dividing into contested and uncontested districts
#make temp df that summarizes based on election and district
#if contest_dem and contest_rep is both => 1 then the original rows go into contested df
#if only one is >= 1 then the original rows go into uncontested df


