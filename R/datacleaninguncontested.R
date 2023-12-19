#once all of these functions have been run we are left with a list of all uncontested districts in year
#each uncontested district has data from all three years

#they are accessed on a year basis

# cleaning data for statewide master
statewide_master <- function(x) { # produces the vote info for each state race: total votes, 2p total votes, etc.
  x <- filter_statewide(x)
  y <- total_vote_func(x)
  z <- total_2p_vote_func(x)
  x <- vote_join(x, y, z)
  return(x)
}
filter_statewide <- function(x) {
  x <- x %>%
    filter(office == "Senate" | office == "President" | office == "Attorney General" |
             office == "Secretary of State" | office == "Governor")
  #x <- x[-c(1, 5:6)]
  return(x)
}
total_vote_func <- function(x) {
  x <- x %>%
    group_by(office) %>%
    summarize(total_votes = sum(votes))
  return(x)
}
total_2p_vote_func <- function(x) {
  x <- x %>%
    filter(party == "DEM" | party == "REP") %>%
    group_by(office) %>%
    summarize(total_votes_2p = sum(votes))
  return(x)
}
vote_join <- function(x, y, z) {
  x <- x %>%
    left_join(y, by = "office") %>%
    left_join(z, by = "office")
  return(x)
}


#gets district numbers for for loo
check_districts <- function(x) {
  x <- as.integer(unique(x$district))
  return(x)
}




#functions that get run on districts for each year
district_func <- function(x, y,...) { # binds together the district based race with the statewide races
  tv_sax_year <- total_vote_func(x)
  tv2p_sax_year <- total_2p_vote_func(x)
  sax_year <- vote_join(x, tv_sax_year, tv2p_sax_year) %>%
    filter(party == "DEM" | party == "REP")
  wards_sax_year <- data.frame(ward = check_wards(x)) # pulls the wards from the district we're looking at
  statewide_x_year <- y %>%
    right_join(wards_sax_year, by = "ward") # joins the wards to the statewide data to isolate wards we want
  statewide_x_year <- statewide_x_year[-(12:13)] # removes the total votes columns bc they're for statewide
  tv_statewide_x_year <- total_vote_func(statewide_x_year) # now we're pulling info for that district
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    filter(party == "DEM" | party == "REP")
  district_x_year <- rbind(statewide_x_year, sax_year)
  district_x_year <- candidate_function(district_x_year) # now we're getting candidate vote totals, props for each district
}

check_wards <- function(x) {
  uwards <- (unique(x$ward)) # creates uwards as a character vector
  return(uwards)
}

candidate_function <- function(x, ...) { # should only be run on 2p races
  cand_x_year <- x %>%
    group_by(candidate) %>%
    summarize(cand_total_votes = sum(votes)) # groups votes received by candidate
  x <- x %>%
    left_join(cand_x_year, by = "candidate") %>%
    mutate(prop = cand_total_votes/total_votes_2p)
  return(x)
}

