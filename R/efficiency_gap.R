## Efficiency Gap

#EG = ((VB - VA) / V)- 0.5((SB - SA)/S))
#VB: votes of party B (Republican)
#VA: votes of party A (Democrat)
#V: total state-wide votes
#SB: seat share of party B (Republican)
#SA: seat share of party A (Democrat)
#S: total congressional seats


## Calculating Efficiency Gap for 2010 and 2016 for State Assembly Districts

# Goal: show the difference between calculating with just presidency, statewide info, and

# Wisconsin State Assembly Composition in 2010: 38 D / 61 R (technically 60 but there's
# one Independent who caucused with the Republicans so I'm counting it as a REP)

## Now we can take our data and find Efficiency Gap for the requested year

vote_prep <- function(full_votes) {
  total_votes <- full_votes %>%
    group_by(District) %>%
    summarize(total_votes = sum(Dem_votes, Rep_votes))

  full_votes <- full_votes %>%
    left_join(total_votes, by = "District")
  return(full_votes)
}


amended <- vote_prep(full_votes)

a2 <- amended %>%
  mutate(WI = "WI") %>%
  group_by(WI) %>%
  summarize(total_dem = sum(Dem_votes),
            total_rep = sum(Rep_votes),
            total_total = sum(total_votes))

Vrep <- a2$total_rep
Vdem <- a2$total_dem
Vtotal <- a2$total_total
Srep <- as.numeric(61)
Sdem <- as.numeric(38)
Stot <- as.numeric(99)

Vmargin <- ((Vrep - Vdem) / Vtotal)
Smargin <- 0.5 * ((Srep - Sdem) / Stot)

EG_sa2010 <- as.numeric(Vmargin - Smargin)

full_2016 <- year_baseline_data(2016)

a3 <- vote_prep(full_2016)

a3 <- a3 %>%
  mutate(WI = "WI") %>%
  group_by(WI) %>%
  summarize(total_dem = sum(Dem_votes),
            total_rep = sum(Rep_votes),
            total_total = sum(total_votes))

Vrep <- a3$total_rep
Vdem <- a3$total_dem
Vtotal <- a3$total_total
Srep <- as.numeric(64)
Sdem <- as.numeric(35)
Stot <- as.numeric(99)

Vmargin <- ((Vrep - Vdem) / Vtotal)
Smargin <- 0.5 * ((Srep - Sdem) / Stot)

EG_sa2016 <- as.numeric(Vmargin - Smargin)


## Using Presidential Data




hRseats2010 <- as.numeric(5)
hDseats2010 <- as.numeric(3)
hTseats2010 <- as.numeric(8)

## Using aggregated statewide race data

## Using only (estimated) state assembly data

## Using ALL


# Calculating EG for WI_2000


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
