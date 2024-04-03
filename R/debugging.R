## experimenting with 2022

mi_2022 <- mi_data[[10]]
mi_2020 <- mi_data[[9]] # str of detroit city 441
mi_2016 <- mi_data[[7]]
mi_2018 <- mi_data[[8]]
mi_2014 <- mi_data[[6]]
mi_2012 <- mi_data[[5]] # detroit city 441 ward 5
mi_2010 <- mi_data[[4]] # not in
mi_2008 <- mi_data[[3]] # not in

mihd1 <- mi_2022 %>%
  filter(district == 1) %>%
  filter(office == "State House")
## mihd 1, 2, 3, 4 totally empty -- manually input from the secretary of state WEBSITE (all county vals)

mihd35 <- mi_2022 %>%
  filter(district ==35) %>%
  filter(office == "State House")

mihd36 <- mi_2022 %>%
  filter(district ==36) %>%
  filter(office == "State House")

## write a function that wraps around loading of 2022 data and will make sure that state house
## parties are dem and rep if empty string then discard

mi_2022_prep <- function(year_data) {
  yd <- year_data[!year_data$party=="", ]
  return(yd)
}

mi_2022 <- mi_data[[10]]
mm <- mi_2022_prep(mi_2022)

## WI district 58 2012 is the issue

# dis_baseline_ve and estimates is empty
# everything is empty except for uncontested

# district 58 isn't pulling 2010, 2008

wi_2012 <- wi_data[[7]]
statewide_2012 <- statewide_master(wi_2012)

wi_2010 <- wi_data[[6]]
statewide_2010 <- statewide_master(wi_2010)

wi_2008 <- wi_data[[5]]
statewide_2008 <- statewide_master(wi_2008)

## district FUNC not working properly for 2008, 2010 district 58

wihd40 <- wi_2012 %>%
  filter(office == "State Assembly") %>%
  filter(district == 40)

wihd58 <- wi_2012 %>%
  filter(office == "State Assembly") %>%
  filter(district == 58)

# x= district, y=statewide data

## for dis 40

tv_40_2012 <- total_vote_func(wihd40)
tv2p_40_2012 <- total_2p_vote_func(wihd40)
s40_2012 <- vote_join(wihd40, tv_40_2012, tv2p_40_2012) %>%
  dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
wards_40_2012 <- data.frame(ward = check_wards(wihd40))
statewide_40_2012 <- statewide_2012 %>%
  dplyr::right_join(wards_40_2012, by = "ward")

statewide_40_2010 <- statewide_2010 %>%
  right_join(wards_40_2012, by="ward")

## for dis 58

tv_58_2012 <- total_vote_func(wihd58)
tv2p_58_2012 <- total_2p_vote_func(wihd58)
s58_2012 <- vote_join(wihd58, tv_58_2012, tv2p_58_2012) %>%
  dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
wards_58_2012 <- data.frame(ward = check_wards(wihd58))
statewide_58_2012 <- statewide_2012 %>%
  dplyr::right_join(wards_58_2012, by = "ward") ## this works for 2012

statewide_58_2010 <- statewide_2010 %>%
  right_join(wards_58_2012, by="ward")

statewide_58_2008 <- statewide_2008 %>%
  right_join(wards_58_2012, by="ward")


# tv_sax_year <- total_vote_func(x)
# tv2p_sax_year <- total_2p_vote_func(x)
# sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
#   dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
wards_sax_year <- data.frame(ward = check_wards(x))
statewide_x_year <- y %>%
  dplyr::right_join(wards_sax_year, by = "ward")
statewide_x_year <- statewide_x_year[-(12:13)]
tv_statewide_x_year <- total_vote_func(statewide_x_year)
tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
  dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
district_x_year <- rbind(statewide_x_year, sax_year)
district_x_year <- candidate_function(district_x_year)


wards <- data.frame(ward=check_wards(wihd58))

w582010 <- wi_2010 %>%
  right_join(wards, by="ward")

## Co district 64 2012
# everything is the same error
