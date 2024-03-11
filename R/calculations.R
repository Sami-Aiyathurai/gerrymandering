# Mass EG calculations

## Wisconsin 2010-2020

wi_data <- open_elections_factory("wi")
wi_data <- generate_data(wi_data)
wi_contested <- sa_contest_all(wi_data)

wisconsin <- function(year, ...) {
  year <- as.character(year)
  year_num <- as.numeric(year)
  wi_year <- access_state_year(year, wi_data)
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


years <- as.character(c("2008", "2010", "2012", "2014", "2016", "2018", "2020"))

## Michigan

mi_data <- open_elections_factory_mi("mi")
mi_data <- generate_data(mi_data)
mi_contested <- sa_contest_all_mi(mi_data)

michigan <- function(year,...) {
  year <- as.character(year)
  year_num <- as.numeric(year)
  mi_year <- access_state_year(year, mi_data)
  votes_year <- year_baseline_data_mi(year_num, mi_data)
  eg_year <- efficiency_gap_mi(votes_year, year_num)
  eg_con_year <- efficiency_gap_contested_mi(votes_year, year_num)

  mi_year <- data.frame(Year = year,
                        Efficiency_gap = eg_year,
                        Efficiency_gap_contested = eg_con_year,
                        State = "MI")
  return(mi_year)
}

mi_egs <- rbind(michigan(2008), michigan(2010), michigan(2012), michigan(2014),
                michigan(2016), michigan(2018), michigan(2020), michigan(2022))

egs <- rbind(wi_egs, mi_egs)