# open_elections <- function(x) {
#
# }
#
# # states in question: Wisconsin, Michigan, Arizona, Georgia, Pennsylvania, Nevada, North Carolina, Ohio, Maryland
#
# # Molly: make sure that each file for each state that is needed has the SAME url format across years
#
# # Mia (& Molly): once MZ has verified the formating of the URLS then we can go through the files and see how much cleaning
# # is necessary for each one and how it compares to WI. If it's super different we're going to write a different function
#
# # Sami: working on data downloaded function for WI
#
# wi_2016 <- wi_2016 %>%
#   group_by(district, office, party) %>%
#   summarize(cand_votes = sum(votes))
#
# for (district in wi_2016) {
#   wi_2016$contest_dem <- ifelse(wi_2016$party == "DEM", 1, 0)
#   wi_2016$contest_rep <- ifelse(wi_2016$party == "REP", 1, 0)
#   wi_2016$year <- as.numeric("2016") # need to create this because it doesn't save year as variable bc each set is separate
# }
