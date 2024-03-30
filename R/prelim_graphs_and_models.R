## Preliminary modeling

library(rio)

states <- import("StatesAndCyclesData_production-20240301a.csv")
egs$Year <- as.numeric(egs$Year)
egs$Year <- as.integer(egs$Year)


mod_state <- states %>%
  dplyr::select(State, `Cycle Year`, Level, Seats, Institution, `Party Control`, Governor) %>% #`Plan Status`) %>%
  filter(Level == "State Lower") %>%
  rename("Year" = "Cycle Year") %>%
  rename("Legislature_Control" = "Party Control") %>%
  mutate(Trifecta = "1") %>%
  mutate(State_Supreme_method = "1") #%>%
  # rename("Status" = "Plan Status")

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
egs_mod$Seats <- ifelse(egs_mod$State == "PA", 203, egs_mod$Seats)
egs_mod$Institution <- ifelse(egs_mod$State == "PA", "Politician commission", egs_mod$Institution)

## GOV party each year

egs_mod$Governor <- ifelse(egs_mod$State == "WI" & egs_mod$Year < 2010, "D", egs_mod$Governor)
egs_mod$Governor <- ifelse(egs_mod$State == "WI" & egs_mod$Year > 2010, "R", egs_mod$Governor)
egs_mod$Governor <- ifelse(egs_mod$State == "WI" & egs_mod$Year > 2016, "D", egs_mod$Governor)

egs_mod$Governor <- ifelse(egs_mod$State == "MI" & egs_mod$Year < 2010, "D", egs_mod$Governor)
egs_mod$Governor <- ifelse(egs_mod$State == "MI" & egs_mod$Year > 2010, "R", egs_mod$Governor)
egs_mod$Governor <- ifelse(egs_mod$State == "MI" & egs_mod$Year > 2018, "D", egs_mod$Governor)

egs_mod$Governor <- ifelse(egs_mod$State == "CO" & egs_mod$Year <= 2022, "D", egs_mod$Governor)

egs_mod$Governor <- ifelse(egs_mod$State == "PA" & egs_mod$Year < 2010, "D", egs_mod$Governor)
egs_mod$Governor <- ifelse(egs_mod$State == "PA" & egs_mod$Year > 2008, "R", egs_mod$Governor)
egs_mod$Governor <- ifelse(egs_mod$State == "PA" & egs_mod$Year > 2014, "D", egs_mod$Governor)


## Legislature control

egs_mod$Legislature_Control <- ifelse(egs_mod$State == "WI" & egs_mod$Year < 2010, "D", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "WI" & egs_mod$Year > 2008, "R", egs_mod$Legislature_Control)

egs_mod$Legislature_Control <- ifelse(egs_mod$State == "MI" & egs_mod$Year < 2010, "Split", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "MI" & egs_mod$Year > 2008, "R", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "MI" & egs_mod$Year > 2020, "D", egs_mod$Legislature_Control)

egs_mod$Legislature_Control <- ifelse(egs_mod$State == "CO" & egs_mod$Year < 2010, "D", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2008, "Split", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2010, "D", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2012, "Split", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2016, "D", egs_mod$Legislature_Control)

egs_mod$Legislature_Control <- ifelse(egs_mod$State == "PA" & egs_mod$Year < 2010, "Split", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "PA" & egs_mod$Year > 2008, "R", egs_mod$Legislature_Control)
egs_mod$Legislature_Control <- ifelse(egs_mod$State == "PA" & egs_mod$Year > 2020, "Split", egs_mod$Legislature_Control)


## make column for trifecta status

egs_mod$Trifecta <- ifelse(egs_mod$State == "WI" & egs_mod$Year < 2010, "D", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "WI" & egs_mod$Year > 2008, "R", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "WI" & egs_mod$Year > 2016, "Split", egs_mod$Trifecta)

egs_mod$Trifecta <- ifelse(egs_mod$State == "MI" & egs_mod$Year < 2010, "Split", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "MI" & egs_mod$Year > 2008, "R", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "MI" & egs_mod$Year > 2018, "Split", egs_mod$Trifecta)

egs_mod$Trifecta <- ifelse(egs_mod$State == "CO" & egs_mod$Year < 2010, "D", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2008, "Split", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "CO" & egs_mod$Year > 2016, "D", egs_mod$Trifecta)

egs_mod$Trifecta <- ifelse(egs_mod$State == "PA" & egs_mod$Year < 2010, "Split", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "PA" & egs_mod$Year > 2008, "R", egs_mod$Trifecta)
egs_mod$Trifecta <- ifelse(egs_mod$State == "PA" & egs_mod$Year > 2012, "Split", egs_mod$Trifecta)

## State supreme court method

egs_mod$State_Supreme_method <- ifelse(egs_mod$State == "WI", "Nonpartisan Election", egs_mod$State_Supreme_method)
egs_mod$State_Supreme_method <- ifelse(egs_mod$State == "MI", "Michigan Method", egs_mod$State_Supreme_method)
egs_mod$State_Supreme_method <- ifelse(egs_mod$State == "CO", "Governor Assisted Appointment", egs_mod$State_Supreme_method)
egs_mod$State_Supreme_method <- ifelse(egs_mod$State == "PA", "Partisan Election", egs_mod$State_Supreme_method)


write.csv(egs_mod, "C:\\Users\\molly\\Desktop\\egs_mod2.csv", row.names=FALSE)
## need to mod party control, governor, and method of judicial elections, partisan court composition if applicable

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

