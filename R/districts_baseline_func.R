## These functions generate the estimated vote shares for uncontested districts

# creates the baseline by gathering the joined dataframes, pivots to flatten dataframe
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

# this function is called within baseline to take district for the year we're looking at
# and run dem_prep and rep_prep on it to determine how NA's should be removed
subsetting <- function(x, ...) {
  tail <- tail(x, n=1)
  ifelse((is.na(tail$DEM)), x <- dem_prep(x), x <- rep_prep(x)) # standard for control flow MZ
  return(x)
}

# if the uncontested district is won by a democrat, dem_prep redefines REP in terms of DEM
# to remove NAs
dem_prep <- function(x) {
  prepped <- x %>%
    filter(!is.na(DEM)) %>%
    mutate(REP=1-DEM)
  return(prepped)
}

# if uncontested district is won by Republican, rep_prep redefines DEM in terms of REP
rep_prep <- function(x) {
  temp <- x %>%
    filter(!is.na(REP)) %>%
    mutate(DEM=1-REP)
  return(temp)
}

# slicing_func trims the data to remove the highest DEM prop and the highest REP prop
slicing_func <- function(x) {
  hdem <- x %>%
    arrange(REP)
  hdem <- hdem[-1,]
  hrep <- hdem %>%
    arrange(DEM)
  trimmed <- hrep[-1,]
  return(trimmed)
}

# trimmed_func produces averages the baseline to estimated votes cast (avg_total_2p), etc.
trimmed_func <- function(x, i) {
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

# vote_estimate returns
vote_estimate <- function(x,...) {
  est_dem <- as.integer(x[[2]] * x[[3]])
  est_rep <- as.integer(x[[2]] * x[[4]])
  l1 <- list(District = x[[1]], Dem_votes = est_dem, Rep_votes = est_rep, Contested = "uncontested")
  return(l1)
}

