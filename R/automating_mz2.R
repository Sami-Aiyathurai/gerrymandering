#Things that can be used to speed stuff up in the far future- getting rid of the districts column in our statewide offices


#calling of state wide fuctions- total vote, two party, vote join
ward_open_elections_factory <- function(dates) {

  read <- function(year){
    date <- dates[year]
    temp <- paste(date,"__wi__general__ward.csv", sep = "")
    url <- file.path("https://raw.githubusercontent.com/openelections/openelections-data-wi/master" , year, temp)
    base_data <- read.csv(url)

    for (district in base_data) {
      base_data$contest_dem <- ifelse(base_data$party == "DEM", 1, 0)
      base_data$contest_rep <- ifelse(base_data$party == "REP", 1, 0)
      base_data$year <- as.numeric(year) # need to create this because it doesn't save year as variable bc each set is separate
    }
    base_data
  }
}
# this creates a massive list of all the data, to access any one year you [[i]]
years <- as.character(c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016", "2018", "2020", "2022"))

everything <- map(.x=years, .f=oe_data_WI)

# offices we care about: President, State Attorney General, State Assembly, State Senate, Senate, House
oe_data_WI <- ward_open_elections_factory(
  dates = c("2000"="20001107", "2002"="20021105", "2004"="20041102", "2006"="20061107", "2008"="20081104", "2010"="20101102", "2012"="20121106", "2014"="20141104", "2016"="20161108", "2018"="20181106", "2020"="20201103", "2022"="20221108")
)
#ask whopper or grace for way to automate
wi_2000 <- oe_data_WI("2000")
wi_2002 <- oe_data_WI("2002")
wi_2004 <- oe_data_WI("2004")
wi_2006 <- oe_data_WI("2006")
wi_2008 <- oe_data_WI("2008")
wi_2010 <- oe_data_WI("2010")
wi_2012 <- oe_data_WI("2012")
wi_2014 <- oe_data_WI("2014")
wi_2016 <- oe_data_WI("2016")
wi_2018 <- oe_data_WI("2018")
wi_2020 <- oe_data_WI("2020")
wi_2022 <- oe_data_WI("2022")


filter_statewide <- function(x) {
  x <- x %>%
    filter(office == "Senate" | office == "President" | office == "Attorney General" |
             office == "Secretary of State" | office == "Governor")
  #x <- x[-c(1, 5:6)]
  return(x)
}

total_vote_func <- function(x) {
  x %>%
    group_by(office) %>%
    summarize(total_votes = sum(votes))
}

total_2p_vote_func <- function(x) {
  x %>%
    filter(party == "DEM" | party == "REP") %>%
    group_by(office) %>%
    summarize(total_votes_2p = sum(votes))
}

vote_join <- function(x, y, z) {
  x <- x %>%
    left_join(y, by = "office") %>%
    left_join(z, by = "office")
  return(x)
}

statewide_modifer <- function(x, ...){
  x <- filter_statewide(x)
  tv_result <- total_vote_func(x)
  tv2_result <- total_2p_vote_func(x)
  x <- vote_join(x, tv_result, tv2_result)
  return(x)
}

everything_statewide <- map(.x=everything, .f=statewide_modifer)

filter_house <- function(x, ...) {
  x <- x %>%
    filter(office == "House")
  return(x)
}

district_separator <- function(x, ...){
  districts <- unique(x$district)
  for (i in vector) {
    x_i <- x %>%
      filter(district == i)
    return(x)
  }
}

everything_house <- map(.x =everything, .f=filter_house)

house_district_separator <- function(x){

}

#Use similar function to map to isolate house district info, data frames for all district - return an individual

#filter out office as house, break out each district into its own data frame


#### Molly's code that I (Mia) am going to work on automating
# joining the two party votes and the total votes statewide to the statewide data

# but we don't want to remove other parties YET

## BASELINE

# they calculated a district's "baseline" averaging how each district voted in elections of interest in a few years
# and then dropped the highest Dem/rep percents respectively -> trimmed mean

# I think this is a good methodology to estimate how districts will perform
# in this they use ALL races including how they voted

# we can calculate "baselines" for each district by looking at how they've voted in all of the elections
# and averaging it. Fortunately, this is simpler than what I thought had to be done.

# this will need to be done for each CONGRESSIONAL and HOUSE DISTRICT with at least 3 electoral cycles worth of info

# we want to have the info as calculated for statewide analysis, but right now our focus is on district analysis.
# what that means is we want to design the functionality to work for each district.

## CONGRESSIONAL

# this function doesn't work yet but I started messing around with it

# below are the steps I took to isolate the house district info

house_2000 <- wi_2000 %>%
  filter(office == "House")

# for the first district (nb: here it is contested)
house_1_2000 <- house_2000 %>%
  filter(district == 1)

check_districts <- function(x) {
  ndis <- as.integer(unique(x$district))
  return(ndis)
}

cd <- check_districts(house_2000)

candidate_function <- function(x, ...) {
  cand_x_year <- x %>%
    group_by(candidate) %>%
    summarize(cand_total_votes = sum(votes))
  x <- x %>%
    left_join(cand_x_year, by = "candidate") %>%
    mutate(prop = cand_total_votes/total_votes_2p)
  return(x)
}

# for the first district (nb: here it is contested)
house_1_2000 <- house_2000 %>%
  filter(district == 1)

house_1_2002 <- house_2002 %>%
  filter(district == 1)

t2 <- map(.x=check_districts(house_2000), .f=house_district_func())

filter_district <- function(x) {
  single_dis <- x %>%
    filter(district == 1)
  return(single_dis)
}

d2 <- filter_district(house_2000)

house_district_func <- function(x, y,...) {
  tv_hx_year <- total_vote_func(x)
  tv2p_hx_year <- total_2p_vote_func(x)
  hx_year <- vote_join(x, tv_hx_year, tv2p_hx_year) %>%
    filter(party == "DEM" | party == "REP")
  wards_hx_year <- data.frame(ward = check_wards(x))
  statewide_x_year <- y %>%
    right_join(wards_hx_year, by = "ward")
  statewide_x_year <- statewide_x_year[-(12:13)]
  tv_statewide_x_year <- total_vote_func(statewide_x_year)
  tv2p_statewide_x_year <- total_2p_vote_func(statewide_x_year)
  statewide_x_year <- vote_join(statewide_x_year, tv_statewide_x_year, tv2p_statewide_x_year) %>%
    filter(party == "DEM" | party == "REP")
  district_x_year <- rbind(statewide_x_year, hx_year)
  district_x_year <- candidate_function(district_x_year)
}

mother <- function(x, y,...) {
  dis <- filter_district(x) %>%
    house_district_func(y)
  return(dis)
}

m1 <- mother(x=house_2000, y=statewide_2000)


test <- house_district_func(house_1_2000, statewide_2000)



tv_house1_2000 <- total_vote_func(house_1_2000) # get total votes cast
tv2p_house1_2000 <- total_2p_vote_func(house_1_2000) # 2 party vote totals

house_1_2000 <- house_1_2000 %>%
  left_join(tv_house1_2000, by = "office") %>%
  left_join(tv2p_house1_2000, by = "office") %>%
  filter(party == "DEM" | party == "REP")

check_wards <- function(x) {
  uwards <- (unique(x$ward)) # creates uwards as a character vector
  return(uwards)
}

wards_house_1_2000 <- data.frame(ward = check_wards(house_1_2000)) # this gives us the wards in the 1st congressional district as DF

statewide_1_2000 <- statewide_2000 %>%
  right_join(wards_house_1_2000, by="ward")

# I just realized that I already filtered s2000 to exclude all 3rd parties so naturally all the results here
# for tv and 2p funcs are going to be the same. Hypothetically, I should save the other as a different object so
# that I can then use it but I'll do that in the morning.


statewide_1_2000 <- statewide_1_2000[-(12:13)]
tv_statewide_1_2000 <- total_vote_func(statewide_1_2000)
tv2p_statewide_1_2000 <- total_2p_vote_func(statewide_1_2000)

statewide_1_2000 <- statewide_1_2000 %>%
  left_join(tv_statewide_1_2000, by = "office") %>%
  left_join(tv2p_statewide_1_2000, by = "office") %>%
  filter(party == "DEM" | party == "REP")

district_1_2000 <- rbind(statewide_district_1, house_1_2000) # now in one DF we have for the 1st Cong district of WI in 2000 how they voted for
# president, senate, and the house!

cand_2000_1 <- district_1_2000 %>%
  group_by(candidate) %>%
  summarize(cand_total_votes = sum(votes))

district_1_2000 <- district_1_2000 %>%
  left_join(cand_2000_1, by = "candidate")

district_1_2000 <- district_1_2000 %>%
  mutate(prop = cand_total_votes/total_votes_2p) # now we have everything we need to start the baseline for 2000

# I'm still not really sure how we'd deal with uncontested districts, but my guess is we'd build a separate
# baseline for it.

# For Wednesday, let's start with congressional data ONLY and getting the functions and stuff to work

## STATE ASSEMBLY

# Let's try it with state assembly now!!

sa2000 <- wi_2000 %>%
  filter(office == "State Assembly")

sa2000_1 <- sa2000 %>%
  filter(district == 1)

##NB: future pivot_wider potentially??



unique(s2000$office)

check_offices <- function(x) {
  uoff = unique(x$office)
  return(uoff)
}

check_offices(s2000) # pres, sen


# so in 2000 we only had races for president and senate.

s2000$total_votes <- as.numeric(nrow(s2000)) # this isn't proper but idk


wi_2000_house <- wi_2000 %>%
  filter(office == "House")
wi_2000_sa <- wi_2000 %>%
  filter(office == "State Assembly")

wi_2002_house <- wi_2002 %>%
  filter(office == "House")
wi_2002_sa <- wi_2002 %>%
  filter(office == "State Assembly")

check_wards <- function(x) {
  uwards <- (unique(x$ward)) # creates uwards as a character vector
  return(uwards)
}

check_districts <- function(x) {
  ndis <- as.integer(unique(x$district))
  return(ndis)
}

dis_h <- check_districts(wi_2000_house)
dis_sa <- check_districts(wi_2000_sa)

dis_h <- check_districts(wi_2002_house)
dis_sa <- check_districts(wi_2002_sa)

wi_2000_h1 <- wi_2000_house %>%
  filter(district == dis_h[1])

wi_2000_h4 <- wi_2000_house %>%
  filter(district == dis_h[4])

wi_2000_sa2 <- wi_2000_sa %>%
  filter(district == dis_sa[2])

wi_2000_sa3 <- wi_2000_sa %>%
  filter(district == dis_sa[3])

wards_wi_2000_sa2 <- data.frame(ward = check_wards(wi_2000_sa2))

# state assembly 3 is the uncontested one
wards_wi_2000_sa3 <- data.frame(ward = check_wards(wi_2000_sa3))

s2000_<- s2000 %>%
  right_join(wards_wi_2000_sa2, by="ward")

s2000_ <- s2000 %>%
  right_join(wards_wi_2000_sa3, by="ward")

# this works! it gives the wards for SA district 2 (uncontested) for WI statewide races in 2000

## working on two party vote totals

# the original wi_[year] gives county, ward, office, district, total.votes,
# party, candidate, votes, contest_dem, contest_rep, year

# to get 2 party vote totals

# I just changed the function that makes s2000


# now we have a column in s2000 (which is statewide offices for 2000 (pres and senate))
# that has total 2 party vote totals and total total

# 2:27pm 12/7 I don't know what's happening right now k bye

# now what I want is to filter to two parties

s2000_ <- s2000_ %>%
  filter(party == "DEM" | party == "REP")

# now I have the two party vote share for both statewide races in 2000, for each ward in
# the state and I have filtered the set down to DEM and REPs.

# Now what do I need to do? I need to model what vote share for specific districts could have been
# had there been two candidates put forth

# to check if someone is an incumbent or not, we can just force it to make compare the candidates
# in a year to those in the previous year and if they match then it's an incumbent and
# we can just make an indicator for that

#split ticket

# We ran regressions of vote choice in contested seats on incumbency status and district
# presidential vote separately for each election year
# they then used different imputation technique for state assembly because they didn't
# have presidential data by state house district

# we strongly discourage analysts from either dropping uncontested races from the
# computation or treating them as if they produced unanimous support for a party

# other methods of imputation include: examining how those districts have turned out
# in post years when they were contested, impute certain vote share, etc.

# I think what I WANT to do right now is to use statewide behavior in a specific district
# to estimate what would've happened had it been contested

# what if we just avg the vote share of the wards in pres and senate election

# this is being done with 2000 state assembly district 2

s2000_2p %>%
  left_join(wards_wi_2000_sa3, by="ward")


## SCRATCH CODE BELOW NOT INCLUDED

summary(wi_2000_house)

wi_2000_house_2p <- wi_2000_house %>%
  filter(party == "DEM" | party == "REP")

wi_2000_house_2p$contested <- vector(mode = "logical", length=length(wi_2000_house_2p))

# vectorized ifelse, any function; mutate(ifelse(any(ward), TRUE, FALSE))


# this was experimenting in class the other day
func <- function(x) {
  wi_2000_h_gb <- x %>%
    group_by(district, contest_dem, contest_rep) %>%
    summarize(contested = sum(contest_dem))
  return(wi_2000_h_gb)
}

func(wi_2000_house_2p)

wi_2000_h_gb <- wi_2000_house_2p %>%
  group_by(district) %>%
  summarize(contested = contest_dem+contest_rep)


for (ward in wi_2000_house_2p) {
  for (party in wi_2000_house_2p$ward) {
    ifelse(wi_2000_house_2p$contest_dem == 1 | wi_2000_house_2p$contest_rep == 1, wi_2000_house_2p$contested[party] == TRUE, wi_2000_house_2p$contested[party] == FALSE)
  }
}

# I don't think empty offices is necessary anymore
empty_offices <- list(office = c("Attorney General", "Governor", "President",
                                 "Secretary of State", "Senate"),
                      total_votes = vector(mode="numeric", length=5))

head(tv_2000)
