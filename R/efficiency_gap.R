#' vote_prep takes in full_votes, of the object from year_baseline_data, and finds the total votes cast
#' for either of the major parties in each district. This number is joined to the primary data frame
#' so we can find the total votes, estimated or actually cast, for the state assembly cast by the whole state
#' which is necessary for the efficiency gap.
#' @param full_votes data frame produced by the year_baseline_data function

vote_prep <- function(full_votes) {
  total_votes <- full_votes %>%
    dplyr::group_by(.data[["District"]]) %>%
    dplyr::summarize(total_votes = sum(.data[["Dem_votes"]], .data[["Rep_votes"]]))
  full_votes <- full_votes %>%
    dplyr::left_join(total_votes, by = "District")
  return(full_votes)
}

#' The efficiency_gap function receives the object of year_baseline_data and the user can
#' input the year they are investigating. This function enters the preset WI General Assembly
#' data frame and pulls the information for whichever year was requested by the user, before
#' then performing all of the calculations on the data, mostly involving summing the total Democratic
#' and Republican votes.
#' @param full_votes data frame as made by year_baseline_data function
#' @param year a numeric vector as inputted by the user within the year range 2006-2020
#' @export

efficiency_gap <- function(full_votes, year) {
  wi_sa <- data.frame(Year = c("2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(99, 99, 99, 99, 99, 99, 99, 99, 99),
                      Dem_seats = c(47, 52, 39, 39, 39, 36, 35, 38, 35),
                      Rep_seats = c(52, 47, 60, 60, 60, 63, 64, 61, 64)
  )
  myear <- as.character(year)
  wi_sa_year <- wi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  amended <- vote_prep(full_votes) %>%
    dplyr::mutate(WI = "WI") %>%
    dplyr::group_by(.data[["WI"]]) %>%
    dplyr::summarize(total_dem = sum(.data[["Dem_votes"]]),
                     total_rep = sum(.data[["Rep_votes"]]),
                     total_total = sum(.data[["total_votes"]]))
  Vrep <- amended$total_rep
  Vdem <- amended$total_dem
  Vtotal <- amended$total_total
  Vmargin <- ((Vrep - Vdem) / Vtotal)
  Smargin <- 0.5 * ((Srep - Sdem) / Stotal)
  EG <- as.numeric(Vmargin - Smargin)
  return(EG)
}

#' The only difference between the efficiency gap and the efficiency gap contested
#' is the additional line of code before we use the object of year_baseline_data, where
#' we filter it to exclude uncontested estimates, which impacts the information pulled
#' by the function.
#'
#' @return A numeric vector, between -0.25 and 0.25 that estimates how gerrymandered the
#' state assembly is.
#'
#' @param full_votes A data frame of length 99, for each legislative district and
#' and how they voted Democrat or Republican for a given year.
#' @param year A character vector identifying the requested year. The user can
#' input a numeric or integer vector, but the function will transform it into a character.
#' @export

efficiency_gap_contested <- function(full_votes, year) {
  wi_sa <- data.frame(Year = c("2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"),
                      Total_seats = c(99, 99, 99, 99, 99, 99, 99, 99, 99),
                      Dem_seats = c(47, 52, 39, 39, 39, 36, 35, 38, 35),
                      Rep_seats = c(52, 47, 60, 60, 60, 63, 64, 61, 64)
  )
  myear <- as.character(year)
  wi_sa_year <- wi_sa %>%
    dplyr::filter(.data[["Year"]] == myear)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  full_votes_cont <- full_votes %>%
    dplyr::filter(.data[["Contested"]] == "contested")
  amended <- vote_prep(full_votes_cont) %>%
    dplyr::mutate(WI = "WI") %>%
    dplyr::group_by(.data[["WI"]]) %>%
    dplyr::summarize(total_dem = sum(.data[["Dem_votes"]]),
                     total_rep = sum(.data[["Rep_votes"]]),
                     total_total = sum(.data[["total_votes"]]))
  Vrep <- amended$total_rep
  Vdem <- amended$total_dem
  Vtotal <- amended$total_total
  Vmargin <- ((Vrep - Vdem) / Vtotal)
  Smargin <- 0.5 * ((Srep - Sdem) / Stotal)
  EG <- as.numeric(Vmargin - Smargin)
  return(EG)
}

#' @return A numeric vector, between -0.25 and 0.25 that estimates how gerrymandered the
#' state assembly is.
#'
