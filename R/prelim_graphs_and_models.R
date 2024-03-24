## Preliminary modeling

states <- import("StatesAndCyclesData_production-20240301a.csv")
egs$Year <- as.numeric(egs$Year)
egs$Year <- as.integer(egs$Year)


mod_state <- states %>%
  select(c(State, `Cycle Year`, Level, Seats, Institution, `Party Control`, Governor)) %>%
  filter(Level == "State Lower") %>%
  rename("Year" = "Cycle Year")

egs_mod <- egs %>%
  left_join(mod_state, by = c("Year" = "Year", "State" = "State"))

egs_mod <- egs_mod %>%
  distinct()

egs_mod$Level <- "State Lower"
egs_mod$Seats <- ifelse(egs_mod$State == "WI", 99, egs_mod$Seats)
egs_mod$Institution <- ifelse(egs_mod$State == "WI", "Legislature", egs_mod$Institution)
egs_mod$Seats <- ifelse(egs_mod$State == "CO", 65, egs_mod$Seats)
egs_mod$Institution <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2020, "Independent commission", egs_mod$Institution)
egs_mod$Institution <- ifelse(egs_mod$State == "CO" & egs_mod$Year < 2020, "Politician commission", egs_mod$Institution)
egs_mod$Seats <- ifelse(egs_mod$State == "MI", 110, egs_mod$Seats)
egs_mod$Institution <- ifelse(egs_mod$State == "MI" & egs_mod$Year < 2020, "Legislature", egs_mod$Institution)


ggplot(egs_mod, aes(x=Year, y=Efficiency_gap)) +
  geom_point(aes(color=Institution)) +
  scale_x_continuous(breaks=seq(2008, 2022, 2),
                     labels=c("2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022")) +
  ylab("Efficiency gap") +
  scale_y_continuous(breaks=seq(-0.24, 0.16, 0.08),
                     labels=c("-0.24", "-0.16", "-0.08", "0", "0.08", "0.16")) +
  facet_wrap(~State)

## need to note when each group passed maps -- eg, if independent commission was passed in 2018 but their maps
# weren't into effect until 2022 then that's necessary to note

ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_point(aes(color = State)) +
  ylab("Efficiency Gap") +
  ggtitle("Scatterplot of state house efficiency gap over time")+
  scale_y_continuous(breaks=seq(-0.16, 0.16, 0.08),
                     labels = c("-0.16", "-0.08", "0", "0.08", "0.16"))

ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_point() +
  facet_wrap(~State) +
  ggtitle("Efficiency gaps for Colorado, Michigan, and Wisconsin over time") +
  ylab("Efficiency Gap") +
  scale_y_continuous(breaks=seq(-0.16, 0.16, 0.08),
                     labels = c("-0.16", "-0.08", "0", "0.08", "0.16"))


ggplot(egs, aes(x=Efficiency_gap, y=Efficiency_gap_contested)) +
  geom_point(aes(color=State))

ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_line(aes(color=State))

