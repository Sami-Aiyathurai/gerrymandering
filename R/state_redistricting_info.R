library(rio)

states <- import("StatesAndCyclesData_production-20240301a.csv")

together <- egs %>%
  left_join(states, by=c("State", "Year"))
