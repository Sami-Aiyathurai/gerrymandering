library(rio)
library(tidyverse)

## Variable organizing

states <- import("StatesAndCyclesData_production-20240301a.csv")
egs <- import("EGs.csv")

egs <- rbind(oh_egs, egs)

egs$Year <- as.numeric(egs$Year)
egs$Year <- as.integer(egs$Year)
egs$eg_pos <- abs(egs$Efficiency_gap)


mod_state <- states %>%
  dplyr::select(State, `Cycle Year`, Level, Seats, Institution, `Party Control`, Governor) %>% #`Plan Status`) %>%
  filter(Level == "State Lower") %>%
  rename("Year" = "Cycle Year") %>%
  mutate(trifecta = "1")

egs_mod <- egs %>%
  left_join(mod_state, by = c("Year" = "Year", "State" = "State")) %>%
  distinct() %>%
  rename("year" = "Year") %>%
  rename("eg" = "Efficiency_gap") %>%
  rename("eg_con" = "Efficiency_gap_contested") %>%
  rename("state" = "State") %>%
  rename("level" = "Level") %>%
  rename("total_seats" = "Seats") %>%
  rename("institution" = "Institution") %>%
  rename("gov_control" = "Governor") %>%
  rename("leg_control" = "Party Control")

egs_mod$level <- "State Lower"
egs_mod$state_supreme <- as.character(nrow(egs_mod))
egs_mod$primary <- as.character(nrow(egs_mod))
egs_mod$simp_inst <- as.character(nrow(egs_mod))


# prepping seats
egs_mod$total_seats <- ifelse(egs_mod$state == "CO", 65, egs_mod$total_seats)
egs_mod$total_seats <- ifelse(egs_mod$state == "MI", 110, egs_mod$total_seats)
egs_mod$total_seats <- ifelse(egs_mod$state == "PA", 203, egs_mod$total_seats)
egs_mod$total_seats <- ifelse(egs_mod$state == "WI", 99, egs_mod$total_seats)
egs_mod$total_seats <- ifelse(egs_mod$state == "OH", 99, egs_mod$total_seats)

# prepping institution -- note to self: should this be who actually drew the lines each year??
egs_mod$institution <- ifelse(egs_mod$state == "CO" & egs_mod$year > 2020, "Independent commission",
                              egs_mod$institution)
egs_mod$institution <- ifelse(egs_mod$state == "CO" & egs_mod$year < 2022, "Politician commission",
                              egs_mod$institution)
egs_mod$institution <- ifelse(egs_mod$state == "MI" & egs_mod$year < 2022, "Legislature", egs_mod$institution)
egs_mod$institution <- ifelse(egs_mod$state == "MI" & egs_mod$year > 2020, "Independent commission", egs_mod$institution)
egs_mod$institution <- ifelse(egs_mod$state == "PA", "Politician commission", egs_mod$institution)
egs_mod$institution <- ifelse(egs_mod$state == "OH", "Politician commission", egs_mod$institution)
egs_mod$institution <- ifelse(egs_mod$state == "WI", "Legislature", egs_mod$institution)

## simplified institution

egs_mod$simp_inst[egs_mod$institution == "Independent commission" |
                    egs_mod$institution == "Politician commission"] <- "Commission"
egs_mod$simp_inst[egs_mod$institution == "Legislature"] <- "Legislature"

# prepping gov_control
egs_mod$gov_control <- ifelse(egs_mod$state == "CO" & egs_mod$year <= 2022, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "MI" & egs_mod$year < 2010, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "MI" & egs_mod$year > 2010, "R", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "MI" & egs_mod$year > 2018, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "PA" & egs_mod$year < 2010, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "PA" & egs_mod$year > 2008, "R", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "PA" & egs_mod$year > 2014, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "WI" & egs_mod$year < 2010, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "WI" & egs_mod$year > 2010, "R", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "WI" & egs_mod$year > 2016, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "OH" & egs_mod$year < 2010, "D", egs_mod$gov_control)
egs_mod$gov_control <- ifelse(egs_mod$state == "OH" & egs_mod$year > 2008, "R", egs_mod$gov_control)

# prepping leg_control
egs_mod$leg_control <- ifelse(egs_mod$state == "CO" & egs_mod$year < 2010, "D", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "CO" & egs_mod$year > 2008, "Split", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "CO" & egs_mod$year > 2010, "D", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "CO" & egs_mod$year > 2012, "Split", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "CO" & egs_mod$year > 2016, "D", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "MI" & egs_mod$year < 2010, "Split", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "MI" & egs_mod$year > 2008, "R", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "MI" & egs_mod$year > 2020, "D", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "PA" & egs_mod$year < 2010, "Split", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "PA" & egs_mod$year > 2008, "R", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "PA" & egs_mod$year > 2020, "Split", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "WI" & egs_mod$year < 2010, "D", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "WI" & egs_mod$year > 2008, "R", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "OH" & egs_mod$year < 2010, "Split", egs_mod$leg_control)
egs_mod$leg_control <- ifelse(egs_mod$state == "OH" & egs_mod$year > 2008, "R", egs_mod$leg_control)


# prepping trifecta
egs_mod$trifecta <- ifelse(egs_mod$state == "CO" & egs_mod$year < 2010, "D", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "CO" & egs_mod$year > 2008, "Split", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "CO" & egs_mod$year > 2016, "D", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "MI" & egs_mod$year < 2010, "Split", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "MI" & egs_mod$year > 2008, "R", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "MI" & egs_mod$year > 2018, "Split", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "PA" & egs_mod$year < 2010, "Split", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "PA" & egs_mod$year > 2008, "R", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "PA" & egs_mod$year > 2012, "Split", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "WI" & egs_mod$year < 2010, "D", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "WI" & egs_mod$year > 2008, "R", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "WI" & egs_mod$year > 2016, "Split", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "OH" & egs_mod$year > 2008, "R", egs_mod$trifecta)
egs_mod$trifecta <- ifelse(egs_mod$state == "OH" & egs_mod$year <2010, "Split", egs_mod$trifecta)

# prepping state_supreme
egs_mod$state_supreme <- ifelse(egs_mod$state == "CO", "Governor Assisted Appointment", egs_mod$state_supreme)
egs_mod$state_supreme <- ifelse(egs_mod$state == "MI", "Nonpartisan Election", egs_mod$state_supreme)
egs_mod$state_supreme <- ifelse(egs_mod$state == "PA", "Partisan Election", egs_mod$state_supreme)
egs_mod$state_supreme <- ifelse(egs_mod$state == "WI", "Nonpartisan Election", egs_mod$state_supreme)
egs_mod$state_supreme <- ifelse(egs_mod$state == "OH" & egs_mod$year <= 2020, "Nonpartisan Election", egs_mod$state_supreme)
egs_mod$state_supreme <- ifelse(egs_mod$state == "OH" & egs_mod$year > 2020, "Partisan Election", egs_mod$state_supreme)

## state supreme categorized

egs_mod$state_supcat <- as.character(nrow(egs_mod))
egs_mod$state_supcat[egs_mod$state_supreme == "Nonpartisan Election" |
                       egs_mod$state_supreme == "Partisan Election" |
                       egs_mod$state_supreme == "Michigan Method"] <- "Election"
egs_mod$state_supcat[egs_mod$state_supreme == "Governor Assisted Appointment"] <- "Governor Appointment"
# prepping primaries

egs_mod$primary[egs_mod$state == "CO"] <- "Semi"
egs_mod$primary[egs_mod$state == "PA"] <- "Closed"
egs_mod$primary[egs_mod$state == "MI"] <- "Open"
egs_mod$primary[egs_mod$state == "WI"] <- "Open"
egs_mod$primary[egs_mod$state == "OH"] <- "Semi"

# factoring variables

egs_mod$year <- as.factor(egs_mod$year)
egs_mod$state <- as.factor(egs_mod$state)
egs_mod$state <- as.character(egs_mod$state)
egs_mod$state_supreme <- as.factor(egs_mod$state_supreme)
egs_mod$institution <- as.factor(egs_mod$institution)
egs_mod$leg_control <- as.factor(egs_mod$leg_control)
egs_mod$gov_control <- as.factor(egs_mod$gov_control)
egs_mod$trifecta <- as.factor(egs_mod$trifecta)
egs_mod$primary <- as.factor(egs_mod$primary)
egs_mod$simp_inst <- as.factor(egs_mod$simp_inst)
egs_mod$state_supcat <- as.factor(egs_mod$state_supcat)
# multiplying eg by 100

egs_mod$eg_pct <- as.character(nrow(egs_mod))
egs_mod$eg_pct <- egs_mod$eg * 100

write.csv(egs_mod, "C:\\Users\\mzelloe\\Documents\\egs_mod_OH.csv", row.names=FALSE)
