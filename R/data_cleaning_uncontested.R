
#' The functions in this file are called within districts_baselin_func.R to create the baselines for uncontested
#' districts.

#' statewide_master is a function that executes smaller functions to find the total votes cast per office,
#' and the total two-party votes cast per office, for all statewide races in a given year. A two-party vote is
#' defined as the total votes cast for either the Democrat or Republican parties.
#' @param x data frame created by open_elections_factory and generate_data
statewide_master <- function(x) {
  x <- filter_statewide(x)
  y <- total_vote_func(x)
  z <- total_2p_vote_func(x)
  x <- vote_join(x, y, z)
  return(x)
}

#' filter_statewide takes the entire state's voting data for a given year and filters it so that only the most
#' prominent statewide offices are included: Senate, President, Attorney General, Secretary of State, and
#' Governor. In any given year, there will be some of, but never all, of these races, as they are temporally
#' staggered.
#' @param x data frame created by open_elections_factory and generate_data
filter_statewide <- function(x) {
  x <- x %>%
    filter(.data[["office"]] == "U.S. Senate" | .data[["office"]] == "President" | .data[["office"]] == "Attorney General" |
             .data[["office"]] == "Secretary of State" | .data[["office"]] == "Governor")
  return(x)
}


#' total_vote_func takes the filtered statewide information and finds
#' @param x data frame created by open_elections_factory and generate_data
total_vote_func <- function(x) {
  x <- x %>%
    group_by(.data[["office"]]) %>%
    summarize(total_votes = sum(.data[["votes"]]))
  return(x)
}

#' @param x data frame created by open_elections_factory and generate_data
total_2p_vote_func <- function(x) {
  x <- x %>%
    filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP") %>%
    group_by(.data[["office"]]) %>%
    summarize(total_votes_2p = sum(.data[["votes"]]))
  return(x)
}

#' vote_join combines the original data frame and appends the total statewide votes for
#' all parties and two parties.
#' @param x data frame created by open_elections_factory and generate_data
#' @param y data frame created by total_vote_func
#' @param z data frame created by total_vote_2p_func
vote_join <- function(x, y, z) {
  x <- x %>%
    left_join(y, by = "office") %>%
    left_join(z, by = "office")
  return(x)
}

#' check_districts creates an integer vector with the unique number of districts in the year. This is important
#' for iteration.
#' @param x data frame returned by the contested or uncontested state assembly data
check_districts <- function(x) {
  x <- as.integer(unique(x$district))
  return(x)
}

#' district_func binds together the district-based race with the statewide race to extract how that district
#' behaved in a statewide race. This is used to create the baseline and estimate district behavior.
#' @param x data frame of state assembly by district, as established by for loops
#' @param y data frame of statewide data for the given year
district_func <- function(x, y) { # changed the [] in statewide_x_year
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  wards_sax_year <- data.frame(ward = check_wards(x))
  statewide_x_year <- y %>%
    dplyr::right_join(wards_sax_year, by = "ward")
  statewide_x_year <- statewide_x_year %>%
    dplyr::select(-c(total_votes, total_votes_2p)) # this index chops off the total_votes and total_votes_2p columns!!
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    dplyr::filter(.data[["party"]] == "DEM" | .data[["party"]] == "REP")
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year)
  return(district_x_year)
}

#' Takes in the filtered statewide information and creates a character vector with all of the unique wards within
#' the specified district. Each district is composed of specified wards, and by taking the unique wards of a
#' district we can identify across redistricting cycles how voters behaved.
#' This is called in district_func
#' @param x data frame of wards in a given state assembly district
check_wards <- function(x) {
  uwards <- (unique(x$ward))
  return(uwards)
}

#' candidate_function should only be run on two party races, and summarizes the data to provide vote shares and
#' proportions for candidates. This allows us to then find average proportions to estimate vote shares.
#' @param x data frame of state assembly district data
candidate_function <- function(x) {
  cand_x_year <- x %>%
    dplyr::group_by(.data[["candidate"]]) %>%
    dplyr::summarize(cand_total_votes = sum(.data[["votes"]]))
  x <- x %>%
    dplyr::left_join(cand_x_year, by = "candidate") %>%
    dplyr::mutate(prop = .data[["cand_total_votes"]]/.data[["total_votes_2p"]])
  return(x)
}
