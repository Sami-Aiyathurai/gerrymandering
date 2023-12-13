##This section is Molly's code, my (Mia's) job is to automate it/make it a function factory
#
#
#
#
# filter_statewide <- function(x) {
#   x <- x %>%
#     filter(office == "Senate" | office == "President" | office == "Attorney General" |
#              office == "Secretary of State" | office == "Governor")
#   #x <- x[-c(1, 5:6)]
#   return(x)
# }
#
# statewide_2000 <- filter_statewide(wi_2000) # only statewide offices for 2000
# statewide_2002 <- filter_statewide(wi_2002) # only statewide offices for 2000
#
# total_vote_func <- function(x) {
#   x %>%
#     group_by(office) %>%
#     summarize(total_votes = sum(votes))
# }
#
# tv_2000 <- total_vote_func(statewide_2000) # total votes cast for each candidate STATEWIDE
# tv_2002 <- total_vote_func(statewide_2002)
#
# total_2p_vote_func <- function(x) {
#   x %>%
#     filter(party == "DEM" | party == "REP") %>%
#     group_by(office) %>%
#     summarize(total_votes_2p = sum(votes))
# }
#
# tv_2p_2000 <- total_2p_vote_func(statewide_2000)
# tv_2p_2002 <- total_2p_vote_func(statewide_2002)
#
# # joining the two party votes and the total votes statewide to the statewide data
#
# statewide_2000 <- statewide_2000 %>%
#   left_join(tv_2000, by = "office") %>%
#   left_join(tv_2p_2000, by = "office")
#
# statewide_2002 <- statewide_2002 %>%
#   left_join(tv_2002, by = "office") %>%
#   left_join(tv_2p_2002, by = "office")
#
# # but we don't want to remove other parties YET
#
# ## BASELINE
#
# # they calculated a district's "baseline" averaging how each district voted in elections of interest in a few years
# # and then dropped the highest Dem/rep percents respectively -> trimmed mean
#
# # I think this is a good methodology to estimate how districts will perform
# # in this they use ALL races including how they voted
#
# # we can calculate "baselines" for each district by looking at how they've voted in all of the elections
# # and averaging it. Fortunately, this is simpler than what I thought had to be done.
#
# # this will need to be done for each CONGRESSIONAL and HOUSE DISTRICT with at least 3 electoral cycles worth of info
#
# # we want to have the info as calculated for statewide analysis, but right now our focus is on district analysis.
# # what that means is we want to design the functionality to work for each district.
#
# ## CONGRESSIONAL
#
# # this function doesn't work yet but I started messing around with it
#
# congress_votes <- function(x) { # the purpose of this function is to arrange all of the vote getting data in one func
#   xh <- x %>%
#     filter(office == "House")
#   for (i in xh) {
#     xh_1 <- xh %>%
#       filter(district == i)
#     xh_1tv <- tv_func(xh_1)
#     xh_12p <- tv_func_2p(xh_1)
#     xh_1 <- xh_1 %>%
#       left_join(xh_1tv, by = "office") %>%
#       left_join(xh_12p, by = "office") %>%
#       filter(party == "DEM" | party == "REP")
#     return(xh_1) # I don't think my return statements are accurate here, I think I'm missing a step
#   }
#   return(xh_1)
# }
#
# # below are the steps I took to isolate the house district info
#
# house_2000 <- wi_2000 %>%
#   filter(office == "House")
#
# # for the first district (nb: here it is contested)
# house_1_2000 <- house_2000 %>%
#   filter(district == 1)
#
# tv_house1_2000 <- total_vote_func(house_1_2000) # get total votes cast
# tv2p_house1_2000 <- total_2p_vote_func(house_1_2000) # 2 party vote totals
#
# house_1_2000 <- house_1_2000 %>%
#   left_join(tv_house1_2000, by = "office") %>%
#   left_join(tv2p_house1_2000, by = "office") %>%
#   filter(party == "DEM" | party == "REP")
#
# check_wards <- function(x) {
#   uwards <- (unique(x$ward)) # creates uwards as a character vector
#   return(uwards)
# }
#
# wards_house_1_2000 <- data.frame(ward = check_wards(house_1_2000)) # this gives us the wards in the 1st congressional district as DF
#
# statewide_1_2000 <- statewide_2000 %>%
#   right_join(wards_house_1_2000, by="ward")
#
# # I just realized that I already filtered s2000 to exclude all 3rd parties so naturally all the results here
# # for tv and 2p funcs are going to be the same. Hypothetically, I should save the other as a different object so
# # that I can then use it but I'll do that in the morning.
#
#
# statewide_1_2000 <- statewide_1_2000[-(12:13)]
# tv_statewide_1_2000 <- total_vote_func(statewide_1_2000)
# tv2p_statewide_1_2000 <- total_2p_vote_func(statewide_1_2000)
#
# statewide_1_2000 <- statewide_1_2000 %>%
#   left_join(tv_statewide_1_2000, by = "office") %>%
#   left_join(tv2p_statewide_1_2000, by = "office") %>%
#   filter(party == "DEM" | party == "REP")
#
# district_1_2000 <- rbind(statewide_1_2000, house_1_2000) # now in one DF we have for the 1st Cong district of WI in 2000 how they voted for
# # president, senate, and the house!
#
# cand_2000_1 <- district_1_2000 %>%
#   group_by(candidate) %>%
#   summarize(cand_total_votes = sum(votes))
#
# district_1_2000 <- district_1_2000 %>%
#   left_join(cand_2000_1, by = "candidate")
#
# district_1_2000 <- district_1_2000 %>%
#   mutate(prop = cand_total_votes/total_votes_2p) # now we have everything we need to start the baseline for 2000
#
# # I'm still not really sure how we'd deal with uncontested districts, but my guess is we'd build a separate
# # baseline for it.
#
# # For Wednesday, let's start with congressional data ONLY and getting the functions and stuff to work
#
# ## STATE ASSEMBLY
#
# # Let's try it with state assembly now!!
#
# sa2000 <- wi_2000 %>%
#   filter(office == "State Assembly")
#
# sa2000_1 <- sa2000 %>%
#   filter(district == 1)
#
# ##NB: future pivot_wider potentially??
#
#
#
# unique(s2000$office)
#
# check_offices <- function(x) {
#   uoff = unique(x$office)
#   return(uoff)
# }
#
# check_offices(s2000) # pres, sen
#
#
# # so in 2000 we only had races for president and senate.
#
# s2000$total_votes <- as.numeric(nrow(s2000)) # this isn't proper but idk
#
#
# wi_2000_house <- wi_2000 %>%
#   filter(office == "House")
# wi_2000_sa <- wi_2000 %>%
#   filter(office == "State Assembly")
#
# wi_2002_house <- wi_2002 %>%
#   filter(office == "House")
# wi_2002_sa <- wi_2002 %>%
#   filter(office == "State Assembly")
#
# check_wards <- function(x) {
#   uwards <- (unique(x$ward)) # creates uwards as a character vector
#   return(uwards)
# }
#
# check_districts <- function(x) {
#   ndis <- as.integer(unique(x$district))
#   return(ndis)
# }
#
# dis_h <- check_districts(wi_2000_house)
# dis_sa <- check_districts(wi_2000_sa)
#
# dis_h <- check_districts(wi_2002_house)
# dis_sa <- check_districts(wi_2002_sa)
#
# wi_2000_h1 <- wi_2000_house %>%
#   filter(district == dis_h[1])
#
# wi_2000_h4 <- wi_2000_house %>%
#   filter(district == dis_h[4])
#
# wi_2000_sa2 <- wi_2000_sa %>%
#   filter(district == dis_sa[2])
#
# wi_2000_sa3 <- wi_2000_sa %>%
#   filter(district == dis_sa[3])
#
# wards_wi_2000_sa2 <- data.frame(ward = check_wards(wi_2000_sa2))
#
# # state assembly 3 is the uncontested one
# wards_wi_2000_sa3 <- data.frame(ward = check_wards(wi_2000_sa3))
#
# s2000_<- s2000 %>%
#   right_join(wards_wi_2000_sa2, by="ward")
#
# s2000_ <- s2000 %>%
#   right_join(wards_wi_2000_sa3, by="ward")
#
# # this works! it gives the wards for SA district 2 (uncontested) for WI statewide races in 2000
#
# ## working on two party vote totals
#
# # the original wi_[year] gives county, ward, office, district, total.votes,
# # party, candidate, votes, contest_dem, contest_rep, year
#
# # to get 2 party vote totals
#
# # I just changed the function that makes s2000
#
#
# # now we have a column in s2000 (which is statewide offices for 2000 (pres and senate))
# # that has total 2 party vote totals and total total
#
# # 2:27pm 12/7 I don't know what's happening right now k bye
#
# # now what I want is to filter to two parties
#
# s2000_ <- s2000_ %>%
#   filter(party == "DEM" | party == "REP")
#
# # now I have the two party vote share for both statewide races in 2000, for each ward in
# # the state and I have filtered the set down to DEM and REPs.
#
# # Now what do I need to do? I need to model what vote share for specific districts could have been
# # had there been two candidates put forth
#
# # to check if someone is an incumbent or not, we can just force it to make compare the candidates
# # in a year to those in the previous year and if they match then it's an incumbent and
# # we can just make an indicator for that
#
# #split ticket
#
# # We ran regressions of vote choice in contested seats on incumbency status and district
# # presidential vote separately for each election year
# # they then used different imputation technique for state assembly because they didn't
# # have presidential data by state house district
#
# # we strongly discourage analysts from either dropping uncontested races from the
# # computation or treating them as if they produced unanimous support for a party
#
# # other methods of imputation include: examining how those districts have turned out
# # in post years when they were contested, impute certain vote share, etc.
#
# # I think what I WANT to do right now is to use statewide behavior in a specific district
# # to estimate what would've happened had it been contested
#
# # what if we just avg the vote share of the wards in pres and senate election
#
# # this is being done with 2000 state assembly district 2
#
# s2000_2p %>%
#   left_join(wards_wi_2000_sa3, by="ward")
#
#
# ## SCRATCH CODE BELOW NOT INCLUDED
#
# summary(wi_2000_house)
#
# wi_2000_house_2p <- wi_2000_house %>%
#   filter(party == "DEM" | party == "REP")
#
# wi_2000_house_2p$contested <- vector(mode = "logical", length=length(wi_2000_house_2p))
#
# # vectorized ifelse, any function; mutate(ifelse(any(ward), TRUE, FALSE))
#
#
# # this was experimenting in class the other day
# func <- function(x) {
#   wi_2000_h_gb <- x %>%
#     group_by(district, contest_dem, contest_rep) %>%
#     summarize(contested = sum(contest_dem))
#   return(wi_2000_h_gb)
# }
#
# func(wi_2000_house_2p)
#
# wi_2000_h_gb <- wi_2000_house_2p %>%
#   group_by(district) %>%
#   summarize(contested = contest_dem+contest_rep)
#
#
# for (ward in wi_2000_house_2p) {
#   for (party in wi_2000_house_2p$ward) {
#     ifelse(wi_2000_house_2p$contest_dem == 1 | wi_2000_house_2p$contest_rep == 1, wi_2000_house_2p$contested[party] == TRUE, wi_2000_house_2p$contested[party] == FALSE)
#   }
# }
#
# # I don't think empty offices is necessary anymore
# empty_offices <- list(office = c("Attorney General", "Governor", "President",
#                                  "Secretary of State", "Senate"),
#                       total_votes = vector(mode="numeric", length=5))
#
# head(tv_2000)
