
#' The functions in this file are called within districts_baselin_func.R to create the baselines for uncontested
#' districts.

#' @export
#' statewide_master is a function that executes smaller functions to find the total votes cast per office,
#' and the total two-party votes cast per office, for all statewide races in a given year. A two-party vote is
#' defined as the total votes cast for either the Democrat or Republican parties.
statewide_master <- function(x) { # produces the vote info for each state race: total votes, 2p total votes, etc.
  x <- filter_statewide(x)
  y <- total_vote_func(x)
  z <- total_2p_vote_func(x)
  x <- vote_join(x, y, z)
  return(x)
}

#' @export
#' filter_statewide takes the entire state's voting data for a given year and filters it so that only the most
#' prominent statewide offices are included: Senate, President, Attorney General, Secretary of State, and
#' Governor. In any given year, there will be some of, but never all, of these races, as they are temporally
#' staggered.
filter_statewide <- function(x) {
  x <- x %>%
    filter(office == "Senate" | office == "President" | office == "Attorney General" |
             office == "Secretary of State" | office == "Governor")
  #x <- x[-c(1, 5:6)]
  return(x)
}

#' @export
#' total_vote_func takes the filtered statewide information and finds
total_vote_func <- function(x) {
  x <- x %>%
    group_by(office) %>%
    summarize(total_votes = sum(votes))
  return(x)
}

#' @export
total_2p_vote_func <- function(x) {
  x <- x %>%
    filter(party == "DEM" | party == "REP") %>%
    group_by(office) %>%
    summarize(total_votes_2p = sum(votes))
  return(x)
}

#' @export
#' vote_join combines the
vote_join <- function(x, y, z) {
  x <- x %>%
    left_join(y, by = "office") %>%
    left_join(z, by = "office")
  return(x)
}


#' @export
#' check_districts creats an integer vector with the unique number of districts in the year. This is important
#' for iteration.
check_districts <- function(x) {
  x <- as.integer(unique(x$district))
  return(x)
}


#' @export
#' district_func binds together the district-based race with the statewide race to extract how that district
#' behaved in a statewide race. This is used to create the baseline and estimate district behavior.

district_func <- function(x, y,...) {
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    filter(party == "DEM" | party == "REP")
  wards_sax_year <- data.frame(ward = check_wards(x))
  statewide_x_year <- y %>%
    right_join(wards_sax_year, by = "ward")
  statewide_x_year <- statewide_x_year[-(12:13)]
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    filter(party == "DEM" | party == "REP")
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year)
}

#' @export
#' Takes in the filtered statewide information and creates a character vector with all of the unique wards within
#' the specified district. Each district is composed of specified wards, and by taking the unique wards of a
#' district we can identify across redistricting cycles how voters behaved.
#' This is called in district_func
check_wards <- function(x) {
  uwards <- (unique(x$ward))
  return(uwards)
}

#' @export
#' candidate_function should only be run on two party races, and summarizes the data to provide vote shares and
#' proportions for candidates. This allows us to then find average proportions to estimate vote shares.
candidate_function <- function(x, ...) { # should only be run on 2p races
  cand_x_year <- x %>%
    group_by(candidate) %>%
    summarize(cand_total_votes = sum(votes)) # groups votes received by candidate
  x <- x %>%
    left_join(cand_x_year, by = "candidate") %>%
    mutate(prop = cand_total_votes/total_votes_2p)
  return(x)
}

