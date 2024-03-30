## pulling from NV sec of state

## exploring NV 2014 data

nv_2014sd21 <- import("2014_nv_sh21.csv")
nv_2014_gov <- import("2014_nv_gov.csv")
nv_2014s <- import("2014_nv_csv.csv")

# FOR STARTERS nv_2014s has about 80,000 more observations than the 2014 from GIT

## compare the amount of precincts in gov and s

gov_p <- as.data.frame(unique(nv_2014_gov$Precinct))

names(gov_p)[names(gov_p) == "unique(nv_2014_gov$Precinct)"] <- "precinct"

ps <- as.data.frame(unique(nv_2014s$V2))
 # there's a difference of 14

names(ps)[names(ps) == "unique(nv_2014s$V2)"] <- "precinct"

gov_p <- gov_p %>%
  filter(precinct != "") # now has 1956
gov_p[nrow(gov_p)+13,] <- NA

ps <- ps %>%
  filter(precinct != "") # now has 1969

cbind(ps, gov_p)

new <- ps %>%
  full_join(gov_p, by = "precinct")

pt2 <- ps %>%
  inner_join(gov_p, by="precinct")



tog <- cbind(pt2, new)
