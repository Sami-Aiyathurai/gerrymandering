un_2010 <- wi_full_sa_di[["2010"]][["uncontested"]]

SAsa_2010 <- wi_full_sa_di[["2010"]]



# Use functionality from mz_2 to estimate baselines for all uncontested districts in 2010

wi_2010 <- access_state_year("wi", "2010", data)
wi_2008 <- access_state_year("wi", "2008", data)
wi_2006 <- access_state_year("wi", "2006", data)

statewide_2010 <- statewide_master(wi_2010)
statewide_2008 <- statewide_master(wi_2008)
statewide_2006 <- statewide_master(wi_2006)

SAsa_2010_list <- split(SAsa_2010, SAsa_2010$contested)
uncon_wi_2010 <- SAsa_2010_list[["uncontested"]]

#uncon_wi_2010 <- uncon_wi_2010 %>%
 # select(ward, district, office, total.votes, party, candidate, votes, year, contested)



un_districts_2010 <- check_districts(uncon_wi_2010)
# refresh
districts <- list()
# this is the loop that works and for every uncontested
# district in 2010 that pulls from uncon_wi_2010 the dis num
# saves as character, then appends that way
for (i in un_districts_2010) {
  print(i)
  print("i in undistricts")
  temp <- uncon_wi_2010 %>%
    filter(district == i) %>%
    subset(select=-c(contest_r, contest_d, contested))
  dis_name <- as.character(i)
  districts[[dis_name]] <- district_func(temp, statewide_2010)
  }

temp <- uncon_wi_2010 %>%
  filter(district == 3) %>%
  subset(select=-c(contest_r, contest_d, contested))
testing_nw <- district_func(temp, statewide_2010)
testing_w <- district_func(sa_3_2010, statewide_2010)










filter_district <- function(x,i) { # this needs to be put in a loop but right now I'm having it set for uncontested district 3 in 2010
  single_dis <- x %>%
    filter(district == i)
}

joining_ <- function(){
  dist_func_year <- list()
  for(i in seq(2000, 2020, 2)){
    year <- toString(i)
    dfs_wi[[year]] <- oe_data_WI(year)
    dfs_mi[[year]] <- oe_data_MI(year)
  }
  dfs <- list()
  dfs[["wi"]] <- dfs_wi
  dfs[["mi"]] <- dfs_mi
  return(dfs)
}



x <- district_func(x=sa_2010, y=statewide_2010)

x1 <- joining(sa_2010, statewide_2010)
