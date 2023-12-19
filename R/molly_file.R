wi_full_sa_di <- sa_contest_all_wi()
SAsa_2010 <- wi_full_sa_di[["2010"]]
SAsa_2010_list <- split(SAsa_2010, SAsa_2010$contested)
uncon_wi_2010 <- SAsa_2010_list[["uncontested"]]
#gets uncontested data for year


wi_2010 <- access_state_year("wi", "2010", data)
wi_2008 <- access_state_year("wi", "2008", data)
wi_2006 <- access_state_year("wi", "2006", data)
#gets statewide data for year and two previous years

#actually running on statewide master
statewide_2010 <- statewide_master(wi_2010)
statewide_2008 <- statewide_master(wi_2008)
statewide_2006 <- statewide_master(wi_2006)

un_districts_2010 <- check_districts(uncon_wi_2010)
# empty list to store data for year
districts <- list()
test <- list()
# gets data for uncontested districts for year
for (i in un_districts_2010) {
  temp <- uncon_wi_2010 %>%
    filter(district == i) %>%
    subset(select=-c(contest_r, contest_d, contested))
  dis_name <- as.character(i)
  year1 <- district_func(temp, statewide_2010)
  year2 <- district_func(temp, statewide_2008)
  year3 <- district_func(temp, statewide_2006)
  districts[[dis_name]] <- rbind(year1, year2, year3)
  test[[dis_name]][["2010"]] <- year1
  test[[dis_name]][["2008"]] <- year2
  test[[dis_name]][["2006"]] <- year3
  }
