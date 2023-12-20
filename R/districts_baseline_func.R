#' @export
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
#' @export
subsetting <- function(x, ...) {
  tail <- tail(x, n=1)
  ifelse((is.na(tail$DEM)), x <- dem_prep(x), x <- rep_prep(x)) # standard for control flow MZ
  return(x)
}

#' @export
dem_prep <- function(x) {
  prepped <- x %>%
    filter(!is.na(DEM)) %>%
    mutate(REP=1-DEM)
  return(prepped)
}

#' @export
rep_prep <- function(x) {
  temp <- x %>%
    filter(!is.na(REP)) %>%
    mutate(DEM=1-REP)
  return(temp)
}

#' @export
slicing_func <- function(x) {
  hdem <- x %>%
    arrange(REP)
  hdem <- hdem[-1,]
  hrep <- hdem %>%
    arrange(DEM)
  trimmed <- hrep[-1,]
  return(trimmed)
}

#' @export
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

#' @export
vote_estimate <- function(x,...) {
  est_dem <- as.integer(x[[2]] * x[[3]])
  est_rep <- as.integer(x[[2]] * x[[4]])
  l1 <- list(District = x[[1]], Dem_votes = est_dem, Rep_votes = est_rep, Contested = "uncontested")
  return(l1)
}

