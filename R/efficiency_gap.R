## Efficiency Gap

#EG = ((VB - VA) / V)- 0.5((SB - SA)/S))
#VB: votes of party B (Republican)
#VA: votes of party A (Democrat)
#V: total state-wide votes
#SB: seat share of party B (Republican)
#SA: seat share of party A (Democrat)
#S: total congressional seats

# Calculating EG for WI_2000

# I don't think the numbers here are actually accurate

st <- statewide_2000 %>%
  filter(party == "DEM" | party == "REP") %>%
  group_by(party, office) %>%
  summarize(total_votes_2p = sum(votes))

states_tot <- statewide_2000 %>%
  filter(party == "DEM" | party == "REP") %>%
  group_by(office, party) %>%
  summarize(total_votes = mean(total_votes), total_votes_2p = mean(total_votes_2p))

states <- statewide_2000 %>% # getting cand vote totals for WI 2000 statewide races
  filter(party == "DEM" | party == "REP") %>%
  group_by(party, office) %>%
  summarize(cand_total_votes = sum(votes))

states_all <- states_tot %>%
  left_join(states, by = c("office", "party"))

# Manipulating the House

total_vote_house_func <- function(x) {
  x %>%
    group_by(district) %>%
    summarize(total_votes = sum(votes))
}

total_2p_vote_house_func <- function(x) {
  x %>%
    filter(party == "DEM" | party == "REP") %>%
    group_by(district) %>%
    summarize(total_votes_2p = sum(votes))
}

tv_h2000 <- total_vote_house_func(house_2000)
tv_2p_h2000 <- total_2p_vote_house_func(house_2000)

# for some reason my join function wasn't working so I'm just hard coding it instead I guess


h2000 <- house_2000 %>%
  left_join(tv_h2000, by = "district") %>%
  left_join(tv_2p_h2000, by = "district")

h2000_cand <- h2000 %>%
  filter(party == "DEM" | party == "REP") %>%
  group_by(party, district) %>%
  summarize(cand_total_votes = sum(votes))

h2000 <- h2000 %>%
  right_join(h2000_cand, by = c("party", "district")) %>%
  group_by(district, party) %>%
  summarize(total_votes = mean(total_votes), total_votes_2p = mean(total_votes_2p),
            cand_total_votes = mean(cand_total_votes)) %>%
  mutate(prop = cand_total_votes/total_votes_2p)

# so now h2000 contains for each district, DEM and rep

# delegation of 4 R, 5 D

h2000_party_votes <- h2000 %>%
  group_by(party) %>%
  summarize(total_votes_2p = sum(total_votes_2p), party_total_votes = sum(cand_total_votes))

Vrep <- h2000_party_votes$party_total_votes[2]
Vdem <- h2000_party_votes$party_total_votes[1]
Votes <- h2000_party_votes$total_votes_2p[1]
Srep <- as.numeric(4)
Sdem <- as.numeric(5)
Stot <- as.numeric(9)

Vmargin <- ((Vrep - Vdem) / Votes)
Smargin <- 0.5 * ((Srep - Sdem) / Stot)

EG_h2000 <- as.numeric(Vmargin - Smargin)

pres_2000_party_votes <- states_all %>%
  filter(office == "President") %>%
  group_by(party) %>%
  summarize(total_votes_2p = sum(total_votes_2p), party_total_votes = sum(cand_total_votes))

Vrep_pres <- pres_2000_party_votes$party_total_votes[2]
Vdem_pres <- pres_2000_party_votes$party_total_votes[1]
Vtot_pres <- pres_2000_party_votes$total_votes_2p[1]

Vmargin_pres <- ((Vrep_pres - Vdem_pres) / Vtot_pres)

EG_p2000 <- as.numeric(Vmargin_pres - Smargin)

sen_2000_party_votes <- states_all %>%
  filter(office == "Senate") %>%
  group_by(party) %>%
  summarize(total_votes_2p = sum(total_votes_2p), party_total_votes = sum(cand_total_votes))

Vrep_sen <- sen_2000_party_votes$party_total_votes[2]
Vdem_sen <- sen_2000_party_votes$party_total_votes[1]
Vtot_sen <- sen_2000_party_votes$total_votes_2p[1]

Vmargin_sen <- ((Vrep_sen - Vdem_sen) / Vtot_sen)

EG_s2000 <- as.numeric(Vmargin_sen - Smargin)

avg_EG_2000 <- mean(c(EG_h2000, EG_p2000, EG_s2000))



## SCRATCH CODE DO NOT RUN ##

h2000_all <- house_2000 %>%
  filter(party == "DEM" | party == "REP") %>%
  group_by(district, party) %>%
  summarize(total_votes = sum(total.votes), total_votes_2p = mean(total_votes_2p))

m2000 <- m1 %>%
  group_by(office, party) %>%
  summarize(total_votes = mean(total_votes), total_votes_2p = mean(total_votes_2p), cand_total_votes = mean(cand_total_votes))

house <- m1 %>%
  filter(office == "House") %>%
  group_by(district) %>%
  summarize(total_votes = mean(total_votes), total_votes_2p = mean(total_votes_2p),
            cand_total_votes = mean(cand_total_votes))
