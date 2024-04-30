#' @title baseline function
#' add something in here to make sure that the district in uncontested using is.na
#' @param x the primary data frame with all of the information as returned by generate data
baseline_function <- function(x) {
  base_x <- x %>%
    dplyr::group_by(.data[["office"]], .data[["year"]], .data[["party"]], .data[["district"]]) %>%
    dplyr::summarize(total_votes_2p = mean(.data[["total_votes_2p"]]),
                     cand_total_votes = mean(.data[["cand_total_votes"]]),
                     prop=mean(.data[["prop"]])) %>%
    tidyr::pivot_wider(names_from = "party", values_from = .data[["prop"]])
  base_x2 <- subsetting(base_x)
  return(base_x2)
}

#' @param x takes the primary data frame as modified in baseline_function
subsetting <- function(x, ...) {
  tail <- tail(x, n=1)
  ifelse((is.na(tail$DEM)), x <- dem_prep(x), x <- rep_prep(x)) # standard for control flow MZ
  return(x)
}

#' @param x takes the data as modified by baseline_function and subsetting
dem_prep <- function(x) {
  prepped <- x %>%
    dplyr::filter(!is.na(.data[["DEM"]])) %>%
    dplyr::mutate(REP=1-.data[["DEM"]])
  return(prepped)
}

#' @param x data frame as modified by baseline_function and subsetting function
rep_prep <- function(x) {
  temp <- x %>%
    dplyr::filter(!is.na(.data[["REP"]])) %>%
    dplyr::mutate(DEM=1-.data[["REP"]])
  return(temp)
}

#' @param x modifies the data frame as produced by the baseline_function to remove outliers
slicing_func <- function(x) {
  hdem <- x %>%
    dplyr::arrange(.data[["REP"]])
  hdem <- hdem[-1,]
  hrep <- hdem %>%
    dplyr::arrange(.data[["DEM"]])
  trimmed <- hrep[-1,]
  return(trimmed)
}

#' @param x the data frame as produced by the slicing_func
#' @param i the district that is being observed aka the iteration of the loop

trimmed_func <- function(x, i) {
  x <- x %>%
    dplyr::mutate(district = i) %>%
    dplyr::mutate(diff = .data[["DEM"]] - .data[["REP"]]) %>%
    dplyr::group_by(.data[["district"]]) %>%
    dplyr::summarize(avg_total_2p = mean(.data[["total_votes_2p"]]),
                     avg_dem_prop = mean(.data[["DEM"]]),
                     avg_rep_prop = mean(.data[["REP"]]),
                     avg_diff = mean(diff))
  return(x)
}

#' @param x takes the trimmed means data frame from trimmed_func

vote_estimate <- function(x,...) {
  est_dem <- as.integer(x[[2]] * x[[3]])
  est_rep <- as.integer(x[[2]] * x[[4]])
  l1 <- list(District = x[[1]], Dem_votes = est_dem, Rep_votes = est_rep, Contested = "uncontested")
  return(l1)
}

#' @param dis_num the district that is being observed, as noted in the loops
#' @param data the original data frame goes here
#'
dis_baseline_ve <- function(dis_num, data){
  base_sa <- baseline_function(data)
  print(base_sa)
  trim_sa <- slicing_func(base_sa)
  print(trim_sa)
  trimmed_sa <- trimmed_func(trim_sa, dis_num)
  print(trimmed_sa)
  ve <- vote_estimate(trimmed_sa)
  print(ve)
  return(ve)
}
