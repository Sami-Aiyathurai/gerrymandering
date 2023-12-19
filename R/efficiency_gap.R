## Efficiency Gap

#EG = ((VB - VA) / V)- 0.5((SB - SA)/S))
#VB: votes of party B (Republican)
#VA: votes of party A (Democrat)
#V: total state-wide votes
#SB: seat share of party B (Republican)
#SA: seat share of party A (Democrat)
#S: total congressional seats

## Calculating Efficiency Gap for 2010 and 2016 for State Assembly Districts

# There's technically one independent in 2010 who was formerly a REP so for the sakes of 2party VS, I counted
# them as a Republican

## Now we can take our data and find Efficiency Gap for the requested year


wi_sa <- data.frame(Year = c("2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020"),
                    Total_seats = c(99, 99, 99, 99, 99, 99, 99, 99),
                    Dem_seats = c(47, 52, 39, 39, 39, 36, 35, 36),
                    Rep_seats = c(52, 47, 60, 60, 60, 63, 64, 63)
                    )

vote_prep <- function(full_votes) {
  total_votes <- full_votes %>%
    group_by(District) %>%
    summarize(total_votes = sum(Dem_votes, Rep_votes))

  full_votes <- full_votes %>%
    left_join(total_votes, by = "District")
  return(full_votes)
}

efficiency_gap <- function(full_votes, year) {
  myear <- as.character(year)
  wi_sa_year <- wi_sa %>%
    filter(Year == myear)
  print(wi_sa_year)
  Stotal <- wi_sa_year$Total_seats[1]
  Sdem <- wi_sa_year$Dem_seats[1]
  Srep <- wi_sa_year$Rep_seats[1]
  amended <- vote_prep(full_votes) %>%
    mutate(WI = "WI") %>%
    group_by(WI) %>%
    summarize(total_dem = sum(Dem_votes),
              total_rep = sum(Rep_votes),
              total_total = sum(total_votes))
  Vrep <- amended$total_rep
  Vdem <- amended$total_dem
  Vtotal <- amended$total_total
  Vmargin <- ((Vrep - Vdem) / Vtotal)
  Smargin <- 0.5 * ((Srep - Sdem) / Stotal)
  EG <- as.numeric(Vmargin - Smargin)
  return(EG)
  }

efficiency_gap(votes_2010, 2010)
efficiency_gap(votes_2014, 2014)
efficiency_gap(votes_2016, 2016)
