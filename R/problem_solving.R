## okay

## step 1: add column in original Df that concatenates county and precinct, separated by space

open_elections_factory_wi <- function(state) {
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
      data$cw_concat <- paste(data$county, data$ward, sep=" ") # yay! this worked!
    }
    data
  }
}

wi_data <- open_elections_factory_wi("wi")
wi_data <- generate_data(wi_data) # this worked for creating the things

## te

districts_full <- data.frame(District = 1:99,
                             Dem_votes = integer(length(1:99)),
                             Rep_votes = integer(length(1:99)),
                             Contested = character(length(1:99)))

myear <- as.character(2012)
myearm2 <- as.character((2012-2))
myearm4 <- as.character((2012-4))

full_sa_di <- sa_contest_all(wi_data)
main_year <- full_sa_di[[myear]]
main_year_list <- split(main_year, main_year$contested)
uncon_main_year <- main_year_list[["uncontested"]] # everything through here is working properly

main_year_state <- access_state_year(myear, wi_data)
main_minus_two <- access_state_year(myearm2, wi_data)
main_minus_four <- access_state_year(myearm4, wi_data)

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
} ## working

un_districts_main_year <- check_districts(uncon_main_year)
districts <- list()
ve_list <- list()

## testing with dis 58

temp <- uncon_main_year %>%
  filter(district == 58) %>%
  dplyr::select(-c("contest_r", "contest_d", "contested"))
dis_name <- as.character(58)
MODmain_year <- MODdistrict_func(temp, statewide_main_year)
MODmainyearminus2 <- MODdistrict_func(temp, statewide_main_minus_two)
MODmainyearminus4 <- MODdistrict_func(temp, statewide_main_minus_four)

#for (i in un_districts_main_year) {
  temp <- uncon_main_year %>%
    dplyr::filter(.data[["district"]] == i) %>%
    dplyr::select(-c("contest_r", "contest_d", "contested"))
  dis_name <- as.character(i)
  main_year <- district_func(temp, statewide_main_year) ## use mod dis func (change dis func!!)
  mainyearminus2 <- district_func(temp, statewide_main_minus_two)
  mainyearminus4 <- district_func(temp, statewide_main_minus_four)

  # ifelse(mainyearminus2$year != as.numeric(myearm2) | mainyearminus4$year != as.numeric(myearm4), ## check to make sure it doesn't appear within the year column
         ## write function that will perform the shit, else do nothing)

  districts[[dis_name]][["data"]] <- rbind(main_year,  mainyearminus2, mainyearminus4)
  districts[[dis_name]][["estimates"]] <- MODdis_baseline_ve(i, districts[[dis_name]][["data"]])
  districts_full[i, ] <- districts[[dis_name]][["estimates"]]
}

## make this work outside the functions

## use trim trailing whitespace on counties, ward names
# force to lower case, will decrease likelihood of errors
# put in open elections

cc <- check_concat(temp)
str <- cc[1]
str_mod <- str_extract_part(str, before=TRUE, pattern=" W") # modifies right
grp <- grep(as.character(str_mod), statewide_main_minus_two$cw_concat, value=FALSE) # gives ALLLL occcurrences of the first part
for (i in grp) {
  index <- grp[i]
  newrow <- statewide_main_minus_two[index, ]

}

#use this when grep value =TRUE
# uwards <- unique(grp)
#
# df1 <- statewide_main_minus_two %>%
#   filter(ward == uwards[1] & ward == uwards[2])

## change the str extract to an and/or statement that is
# " Ward" or " Wd"



sacw_concat <- check_concat(MODmain_year)
for (i in seq_along(cw_concat)) {
  str <- cw_concat[i]
  str_mod <- str_extract_part(str, before=TRUE, pattern=" W")
  grep(as.character(str_mod), MODmainyearminus2$cw_concat)
  ## create a new DF by filtering year before by STR_MOD
  ## make a new DF with all the columns that we have as normal, empty vectors
  ## inside a loop maybe? data_frame[i, ] <- vec where vec is pullsed from existing??
}

## this is the function that will get called in the ifelse
redis_wards <- function(MODmain_year, MODmainyearminus2, MODmainyearminus4, myear) { # remember this is for a district!!
  ## shit above goes in here
  }
}

redis_wards(temp)

check_concat <- function(year_data) {
  concat <- unique(year_data$cw_concat)
  return(concat)
}

## now test with 58
temp <- uncon_main_year %>%
  filter(district == 58) %>%
  dplyr::select(-c("contest_r", "contest_d", "contested"))

main_year2 <- MODdistrict_func(temp, statewide_main_year) # pulls pres, sen, state assembly
mainyearminus2 <- MODdistrict_func(temp, statewide_main_minus_two) # just state assem and it's the 2012 DISTRICT DATA
mainyearminus4 <- MODdistrict_func(temp, statewide_main_minus_four) # just stae assem and it's the 2012 DISTRICT DATA

## checking district_func with the additional column (do the indexes need to change??)
d58 <- wi_data[[7]] %>%
  filter(office == "State Assembly") %>%
  filter(district == 58) # this is x

# y is statewide main year (aka statewide 2012)

MODdistrict_func <- function(state_assem_dis_year_data, statewide_year_data) {
  tv_sax_year <- total_vote_func(state_assem_dis_year_data)
  tv2p_sax_year <- total_2p_vote_func(state_assem_dis_year_data)
  sax_year <- vote_join(state_assem_dis_year_data, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  wards_sax_year <- data.frame(ward = check_wards(state_assem_dis_year_data)) # gives the list of wards that we want to
  statewide_x_year <- statewide_year_data %>% # mod to statewide main year
    dplyr::right_join(wards_sax_year, by = "ward")
  statewide_x_year <- statewide_x_year %>%
    dplyr::select(-c(total_votes, total_votes_2p)) # changed to a column negative select to fix the problem
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year)
  return(district_x_year)
}
1
