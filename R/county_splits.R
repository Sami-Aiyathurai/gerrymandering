## County splits attempt for WI

library(readxl)

d2010 <- data[[6]]

# state's total pop / no. districts

d2010c <- d2010 %>%
  group_by(county, office, year) %>%
  summarize(total.votes = sum(total.votes),
            votes = sum(votes))

# 5,686,986 from gov

wi_hdr <- 5686986/99 # this is the ideal amount of people in each state legislative district in
wi_cdr <- 5686986/9 # ideal amount of people in each HD

# now do the county's population / the ideal of the state's population for EACH county

county_pop <- read_xls("Census-2010-Wisconsin-County-Database.xls")

county <- county_pop %>%
  rename(county_name = "COUNTY NAME",
         state = "STATE NAME",
         pop_2010 = "POPULATION, 2010") %>%
  filter(county_name != "NA") %>%
  select(county_name, state, pop_2010)

county$pop_2010 <- as.numeric(county$pop_2010)

county_add <- county %>%
  mutate(leg_dr = as.numeric(5686986/99)) %>%
  mutate(cong_dr = as.numeric(5686986/9))

## just starting for the first county



adams <- county_add %>%
  filter(county_name == "Adams County") %>%
  mutate(leg_pr = pop_2010/leg_dr) %>%
  mutate(cong_pr = pop_2010/cong_dr)

dane <- county_add %>%
  filter(county_name == "Dane County") %>%
  mutate(leg_pr = pop_2010/leg_dr) %>%
  mutate(cong_pr = pop_2010/cong_dr)
