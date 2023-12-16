## TESTING UNCONTESTED DISTRICTS

# State Assembly district 3 in 2010 was UNCONTESTED
# - need statewide_2010, 2008, 2006

# Statewides

statewide_2006 <- filter_statewide(wi_2006) # getting all of the statewide races for all the years
statewide_2008 <- filter_statewide(wi_2008)
statewide_2010 <- filter_statewide(wi_2010)

tv_2006 <- total_vote_func(statewide_2006) # getting total votes cast per candidate per race
tv_2008 <- total_vote_func(statewide_2008)
tv_2010 <- total_vote_func(statewide_2010)

tv_2p_2006 <- total_2p_vote_func(statewide_2006) # getting total 2 party votes cast per candidate per race
tv_2p_2008 <- total_2p_vote_func(statewide_2008)
tv_2p_2010 <- total_2p_vote_func(statewide_2010)

statewide_2006 <- vote_join(statewide_2006, tv_2006, tv_2p_2006) # joining it all together
statewide_2008 <- vote_join(statewide_2008, tv_2008, tv_2p_2008)
statewide_2010 <- vote_join(statewide_2010, tv_2010, tv_2p_2010)

# this gives us, for 2006, 2008, 2010: total votes cast for ALL candidates/parties, votes cast for each
# candidate, year, total_votes for the office, and total two party votes for the office

## Calculating the baselines

# now we need to isolate the wards

state_assembly_filter <- function(x) {
  x <- x %>%
    filter(office == "State Assembly")
  return(x)
}

filter_district3 <- function(x) { # this is inefficient and needs to be changeable for each district, I think it should a loop
  single_dis <- x %>%
    filter(district == 3) # this needs to be able to change for each district in the state
  return(single_dis)
}

sa_3_2010 <- filter_district3(state_assembly_filter(wi_2010))
sa_3_2008 <- filter_district3(state_assembly_filter(wi_2008))
sa_3_2006 <- filter_district3(state_assembly_filter(wi_2006))

sa_3_wards <- check_wards(sa_3_2010)

candidate_function(sa_3_2010)

# the issue was that I didn't create and call both a new filter district and a new mother... these
# need to be fixed so they don't have to change


mother3 <- function(x, y,...) {
  dis <- filter_district3(x) %>%
    house_district_func(y)
  return(dis)
}

m2010_3 <- mother3(x=sa_2010, y=statewide_2010)
m2008_3 <- mother3(x=sa_2008, y=statewide_2008)
m2006_3 <- mother3(x=sa_2006, y=statewide_2006)
# nb. SA3 was contested in 2006, 2008, but not in 2010

baseline_sa3 <- rbind(m2010_3, m2008_3, m2006_3) # gets all the info in one place

base_sa3 <- baseline_sa3 %>%
  group_by(office, year, party) %>%
  summarize(total_votes_2p = mean(total_votes_2p),
            cand_total_votes = mean(cand_total_votes),
            prop = mean(prop)) %>%
  pivot_wider(names_from = "party", values_from = prop) # this got rid of the 2010 district and that's okay

base_sa3dem <- base_sa3 %>%
  filter(!is.na(DEM)) %>%
  mutate(REP = 1-DEM)

base_sa3_mod <- base_sa3 %>%
  right_join(base_sa3dem, by = c("DEM", "REP", "office",
                                 "total_votes_2p", "cand_total_votes", "year")) %>%
  arrange(REP)

base_sa3_mod <- base_sa3_mod[-1,] # removes the highest dem percent (senate 2006)

base_sa3_mod <- base_sa3_mod %>%
  arrange(DEM)

trimmed_sa3 <- base_sa3_mod[-1,]

trimmed_sa3 <- trimmed_sa3 %>%
  mutate(district = 3) %>%
  mutate(diff = DEM - REP)

final_trimmed_sa3 <- trimmed_sa3 %>%
  group_by(district) %>%
  summarize(avg_total_2p = mean(total_votes_2p),
            avg_dem_prop = mean(DEM),
            avg_rep_prop = mean(REP),
            avg_diff = mean(diff))

est_vt_dem <- final_trimmed_sa3[[2]] * final_trimmed_sa3[[3]]
est_vt_rep <- final_trimmed_sa3[[2]] * final_trimmed_sa3[[4]]

est_votes <- data.frame(est_votes = c(est_vt_dem, est_vt_rep))

# This tells us that, based on the other results of the 2010 electoral cycle and the
