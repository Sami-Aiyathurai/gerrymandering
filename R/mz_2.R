# this creates a massive list of all the data, to access any one year you [[i]]
years <- as.character(c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"))

everything <- map(.x=years, .f=oe_data_WI)

wi_2010 <- data[["wi"]][["2010"]]
wi_2008 <- data[["wi"]][["2008"]]
wi_2006 <- data[["wi"]][["2006"]]


# Getting the statewide vote information
# these are the primary functions used to organize the statewide data

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

statewide_master <- function(x) { # produces the vote info for each state race: total votes, 2p total votes, etc.
  x <- filter_statewide(x)
  y <- total_vote_func(x)
  z <- total_2p_vote_func(x)
  x <- vote_join(x, y, z)
  return(x)
}

statewide_all <- map(.x=everything, .f=statewide_master)

statewide_mapped <- map(.x=state_data[["wi"]], .f=statewide_master)
# these produce different objects I've compared but I don't know how different the objects are

# to select specific years (for trials) need to subset with [[]]
statewide_2006 <- statewide_all[[4]]
statewide_2008 <- statewide_all[[5]]
statewide_2010 <- statewide_all[[6]]

statewide_2006 <- statewide_master(wi_2006)
statewide_2008 <- statewide_master(wi_2008)
statewide_2010 <- statewide_master(wi_2010)

### BASELINE

# need the cycle in question for example, (2010), and how they voted in the two elections prior (2008 and 2006)

## Getting district data

state_assembly_filter <- function(x) {
  x <- x %>%
    filter(office == "State Assembly")
  return(x)
}

check_districts <- function(x) {
  x <- as.integer(unique(x$district))
  return(x)
}

sa_2010 <- state_assembly_filter(wi_2010) # pass original object in here, NOT statewide version
sa_2008 <- state_assembly_filter(wi_2008)
sa_2006 <- state_assembly_filter(wi_2006)

assembly_districts <- check_districts(sa_2010) # integer vector -- this will be the same across years

candidate_function <- function(x, ...) { # should only be run on 2p races
  cand_x_year <- x %>%
    group_by(candidate) %>%
    summarize(cand_total_votes = sum(votes)) # groups votes received by candidate
  x <- x %>%
    left_join(cand_x_year, by = "candidate") %>%
    mutate(prop = cand_total_votes/total_votes_2p)
  return(x)
}

t2 <- map(.x=check_districts(house_2000), .f=house_district_func()) # Audrey had said this could work

# this process needs to be done on each year INDIVIDUALLY

filter_district <- function(x) { # this needs to be put in a loop but right now I'm having it set for uncontested district 3 in 2010
  single_dis <- x %>%
    filter(district == 3)
  return(single_dis)
}

check_wards <- function(x) {
  uwards <- (unique(x$ward)) # creates uwards as a character vector
  return(uwards)
}

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

filter_district8 <- function(x) { # this needs to be put in a loop but right now I'm having it set for uncontested district 3 in 2010
  single_dis <- x %>%
    filter(district == 8)
  return(single_dis)
}


joining <- function(x, y,...) { # x is sa_2010, y=statewide_2010
  dis <- filter_district(x) %>% # we're filtering the state assembly data for district in question
    district_func(y)
  return(dis)
}

joining8 <- function(x, y,...) { # x is sa_2010, y=statewide_2010
  dis <- filter_district8(x) %>% # we're filtering the state assembly data for district in question
    district_func(y)
  return(dis)
}


j1 <- joining(x=sa_2010, y=statewide_2010)
j2 <- joining(x=sa_2008, y=statewide_2008)
j3 <- joining(x=sa_2006, y=statewide_2006)

j18 <- joining8(x=sa_2010, y=statewide_2010)
j28 <- joining8(x=sa_2008, y=statewide_2008)
j38 <- joining8(x=sa_2006, y=statewide_2006)


## Creating the baseline

# Creating a baseline via trimmed means (dropping highest DEM/REP percents) then averaging

baseline_function <- function(x, ...) {
  base_x <- x %>%
    group_by(office, year, party, district) %>%
    summarize(total_votes_2p = mean(total_votes_2p),
              cand_total_votes = mean(cand_total_votes),
              prop=mean(prop)) %>%
    pivot_wider(names_from = "party", values_from = prop)
  base_x2 <- subsetting(base_x)
  return(base_x2)
}

subsetting <- function(x, ...) {
  tail <- tail(x, n=1)
  ifelse((is.na(tail$DEM)), x <- dem_prep(x), x <- rep_prep(x)) # standard for control flow MZ
  return(x)
}

dem_prep <- function(x) {
  prepped <- x %>%
    filter(!is.na(DEM)) %>%
    mutate(REP=1-DEM)
  return(prepped)
}

rep_prep <- function(x) {
  temp <- x %>%
    filter(!is.na(REP)) %>%
    mutate(DEM=1-REP)
  return(temp)
}

base_sa3 <- baseline_function(baseline_sa3)
base_sa8 <- baseline_function(baseline_sa8)

slicing_func <- function(x) {
  hdem <- x %>%
    arrange(REP)
  hdem <- hdem[-1,]
  hrep <- hdem %>%
    arrange(DEM)
  trimmed <- hrep[-1,]
  return(trimmed)
}

trimmed_sa3 <- slicing_func(base_sa3)
trimmed_sa8 <- slicing_func(base_sa8)

trimmed_func <- function(x) {
  x <- x %>%
    mutate(district = i) %>% # where i is whatever district it is on
    mutate(diff = DEM - REP) %>%
    group_by(district) %>%
    summarize(avg_total_2p = mean(total_votes_2p),
              avg_dem_prop = mean(DEM),
              avg_rep_prop = mean(REP),
              avg_diff = mean(diff))
  return(x)
}

trimmed_sa3 <- trimmed_func(trimmed_sa3)

vote_estimate <- function(x) {
  est_dem <- x[[2]] * x[[3]]
  est_rep <- x[[2]] * x[[4]]
  return(est_dem, est_rep)
}

