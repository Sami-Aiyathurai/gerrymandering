## Cover's turnout gap

# Based on Ballotpedia, in 2010, each representative in WI represents 57,558 people

# to account for inter-district turnour differentials

# T*: constant turnout level
# Ti: district level turnout
# i: every district in plan
# D: the plan

# I think this is the competitiveness equation
# deltaW(xy) = S* - 2V* = deltaC(xy)

# Turnout gaps

# we're going to take the votes_2010

votes_2010t <- votes_2010t %>%
  mutate(total_pop = 57558) %>%
  mutate(Ti = total_votes / total_pop)

# T* is the average turnout across all districts (starT)

votes_2010t <- votes_2010t %>%
  mutate(starT = sum(Ti)/99) %>%
  mutate(deltaTi = (Ti-starT)/starT)

# deltaTi: the proportional difference between actual turnout in district i and the average turnout over all districts
# tagging who wins each district, separating into DEM

votes_2010t <- votes_2010t %>%
  mutate(Total_votes = Dem_votes + Rep_votes) %>%
  mutate(dem_prop = Dem_votes / Total_votes) %>%
  arrange(dem_prop)

votes_2010t$winner <- ifelse(votes_2010m$dem_prop >= 0.5, "DEM", "REP")

dem_2010 <- votes_2010t %>%
  filter(winner == "DEM")

rep_2010 <- votes_2010t %>%
  filter(winner == "REP")

# delta T p: average value of delta Ti for districts won by party X (here, DEM)
# using dem_2010

dem_2010$deltaTp <- (sum(dem_2010$deltaTi)) / nrow(dem_2010)

rep_2010$deltaTp <- (sum(rep_2010$deltaTi)) / nrow(rep_2010)

# he never defines what deltaTiC is so I will skip that stage

new_2010 <- rbind(dem_2010, rep_2010)
new_2010$deltaTxy <- (((nrow(dem_2010)) * (dem_2010$deltaTp)) - (nrow(rep_2010) * rep_2010$deltaTp)) / 2


# turnout gap = S* - 2v* + deltaTxy
