#these are the functions called to generate the new vote shares
#these access on a district bases
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

#trim data
slicing_func <- function(x) {
  hdem <- x %>%
    arrange(REP)
  hdem <- hdem[-1,]
  hrep <- hdem %>%
    arrange(DEM)
  trimmed <- hrep[-1,]
  return(trimmed)
}

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
#i'm assuming x in this is what trimmed_func returns?
vote_estimate <- function(x) {
  est_dem <- x[[2]] * x[[3]]
  est_rep <- x[[2]] * x[[4]]
  return(c(est_dem, est_rep))
}
