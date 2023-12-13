
test_wi_2000 <- state_data[["wi"]][["2000"]] %>%
  filter(party == "DEM" | party == "REP") %>%
  filter(office == "House") %>%
  group_by(district)  %>%
  summarize(contest_d = sum(contest_dem), contest_r = sum(contest_rep))

contest_wi_2000 <- state_data[["wi"]][["2000"]] %>%
  filter(party == "DEM" | party == "REP") %>%
  filter(office == "House")

funsies_wi_2000 <- state_data[["wi"]][["2000"]] %>%
  filter(party == "DEM" | party == "REP") %>%
  filter(office == "House") %>%
  filter(contest_dem == 1 | contest_rep == 1)

#stuff from contested fun
df_split <- function(wi_2000) {
  x <- x %>%
    filter(office == "House")
  y <- x %>%
    filter(office == "General Assembly")
  pos_x_types <- unique(x$district)
  pos_y_types <- unique(x$district)
  positions <- c()
}

# working to make two dfs this will go inside a function
wi_2000_house <- wi_2000 %>%
  filter(office == "House")
wi_2000_sa <- wi_2000 %>%
  filter(office == "State Assembly")
pos_x_types <- unique(wi_2000_house$district)
pos_ward <- as.list(unique(wi_2000_house$ward))
pos_y_types <- unique(wi_2000_sa$district)
wi_2000$new <- ifelse(pos_ward == wi_2000$ward, wi_2000$new, wi_2000)
wi_2000 %>%
  filter(ward == pos_ward)

#Wisconsin specific code for separating out state Assembly from hous

wi_sa_2000 <- wi_2000 %>%
  filter(office == "State Assembly")
wi_h_2000 <- wi_2000 %>%
  filter(office == "House")

wi_sa_2000$contested = vector(mode = "logical", length = 9838)

for (district in wi_sa_2000$district) {
  ifelse(wi_sa_2000$contest_dem[district] == 1 & wi_sa_2000$contest_rep[district] == 1, wi_sa_2000$contested[district] == TRUE, wi_sa_2000$contested[district] == FALSE)
}

for (ward in wi_sa_2000$ward){
  for (party in wi_sa_2000$party){
    ifelse(wi_sa_2000$contest_dem[district] == 1 & wi_sa_2000$contest_rep[district] == 1, wi_sa_2000$contested[district] == TRUE, wi_sa_2000$contested[district] == FALSE)
  }
}
