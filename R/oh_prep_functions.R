library(rio)
library(qdapRegex)
library(readxl)
library(forstringr)

oh_2004 <- import("oh_2004_mod.csv")
oh_2006 <- import("oh_2006_mod.csv")
oh_2008 <- import("oh_2008_mod.csv")
oh_2010 <- import("oh_2010_mod.csv")
oh_2012 <- import("oh_2012_mod.csv")
oh_2014 <- import("oh_2014_mod.xlsx", sheet=4)
oh_2016 <- import("oh_2016_mod.csv")
oh_2018 <- import("oh_2018_mod.xlsx", sheet=3)
oh_2020 <- import("oh_2020_mod.csv")
oh_2022 <- import("oh_2022_mod.csv")

oh_2004_prep <- function(data) {
  names(data)[names(data) == "COUNTY NAME"] <- "county"
  names(data)[names(data) == "STATE PRECINCT CODE"] <- "precinct_code"
  names(data)[names(data) == "PRECINCT NAME"] <- "precinct"
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "HOUSE"] <- "district"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Votes"] <- "votes"
  names(data)[names(data) == "Candidate"] <- "candidate"

  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office[data$office == "PRES"] <- "President"
  data$office[data$office == "USSEN"] <- "U.S. Senate"
  data$office[data$office == "STREP"] <- "State House"

  data$county <- str_to_lower(data$county)
  data$precinct <- str_to_lower(data$precinct)
  data$precinct <- str_squish(data$precinct)
  data$candidate <- str_to_lower(data$candidate)
  data$precinct_code <- str_to_lower(data$precinct_code)

  data$district <- as.character(data$district)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$year <- as.numeric(data$year)
  data <- oh_prep2(data)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)

  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}
oh04 <- oh_2004_prep(oh_2004)

## no dis number for 06
oh_2006_prep <- function(data) {
  names(data)[names(data) == "COUNTY_NAME"] <- "county"
  names(data)[names(data) == "STATE_PRECINCT_CODE"] <- "precinct_code"
  names(data)[names(data) == "PRECINCT_NAME"] <- "precinct"
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "OH_HOUSE_DISTRICT"] <- "district"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Votes"] <- "votes"
  names(data)[names(data) == "Candidate"] <- "candidate"

  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office[data$office == "AG"] <- "Attorney General"
  data$office[data$office == "US_SENATE"] <- "U.S. Senate"
  data$office[data$office == "OH_REP"] <- "State House"
  data$office[data$office == "GOV"] <- "Governor"
  data$office[data$office == "SOS"] <- "Secretary of State"
  #print(head(data))
  data$county <- str_to_lower(data$county)
  data$precinct <- str_to_lower(data$precinct)
  data$precinct <- str_squish(data$precinct)
  data$candidate <- str_to_lower(data$candidate)
  data$candidate <- str_squish(data$candidate)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$year <- as.numeric(data$year)

  data$district <- as.character(nrow(data))
  data <- oh_prep2(data)

  print(unique(data$district))
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  ## john p. hagan isn't working for some reason sigh
  # data$district <- ifelse(data$party == "REP" & data$candidate == "todd book", "89", data$district)
  # data$district <- ifelse(data$party == "DEM" & data$candidate == "tim knauff", "89", data$district)

  data$district <- ifelse(data$party == "REP" & data$candidate == "jim hoppel", "1", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "linda s. bolon", "1", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jon peterson", "2", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "bruce edward burnworth", "2", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jim carmichael", "3", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "matt huffman", "4", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "dennis shreefer", "4", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "gerald l. stebelton", "5", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "r. kelly kirk", "5", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "robert e. latta", "6", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "jeffery p. bretz", "6", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "beverly valencic", "7", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "kenny yuko", "7", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "armond budish", "8", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jimmie hicks", "9", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "barbara boyd", "9", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "rosalind a. mcallister", "10", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "eugene r. miller", "10", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "sandra williams", "11", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "anthony cefaratti", "12", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "michael debose", "12", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "john patrick hildebrand", "13", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "michael j. skindell", "13", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "william j. mcgivern", "14", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "mike foley", "14", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "david g. fago", "15", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "timothy j. degeeter", "15", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "edward fitzpatrick herman", "16", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "jennifer brady", "16", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "josh mandel", "17", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "roger j. goudy", "17", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "thomas f. patton", "18", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "john m. celebrezze", "18", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "larry l. flowers", "19", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "marian harris", "19", data$district)

  data$district <- ifelse(data$party == "REP" & data$candidate == "jim mcgregor", "20", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "beverly campbell", "20", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "kevin bacon", "21", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "dean c. hernandez", "21", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jim hughes", "22", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "john patrick carney", "22", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "larry wolpert", "23", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "michael murphy", "23", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "geoffrey c. smith", "24", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "ted celeste", "24", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "michael d. wiles", "25", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "dan stewart", "25", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "michael d. elicson", "26", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "tracy heard", "26", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "john edward jufko", "27", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "joyce beatty", "27", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jim raussen", "28", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "connie pillich", "28", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "louis w. blessing", "29", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "brent gray", "29", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "bill seitz", "30", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "scott gehring", "31", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "steve driehaus", "31", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "kimberly hale", "32", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "dale mallory", "32", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "rob thornton", "33", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "tyrone k. yates", "33", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "tom brinkman", "34", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "stephen e. silver", "34", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "michelle g. schneider", "35", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "karen j. adams", "35", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "arlene j. setzer", "36", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "beverly smith", "36", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jon husted", "37", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "john j. white", "38", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "carolyn rice", "38", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "george b. coles", "39", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "clayton luckie", "39", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "martin arbagi", "40", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "fred strahorn", "40", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "thomas cousineau", "41", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "brian g. williams", "41", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "john widowfield", "42", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "paul colavecchio", "42", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "christine croce", "43", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "stephen dyer", "43", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "joseph d. crawford", "44", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "vernon sykes", "44", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "joshua l. jones", "45", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "robert j. otterman", "45", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "mark wagoner", "46", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "mark p. dansack", "46", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "ernie berry", "47", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "peter ujvagi", "47", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "carolyn j. eyre", "48", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "edna brown", "48", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "steve hornyak", "49", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "matt szollosi", "49", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "john p. hagan", "50", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "john johnson", "50", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "eric waldrop", "52", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "william j. healy", "52", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "scott oelslager", "51", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "kody v. gonzalez", "51", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "shawn n. webster", "53", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "glenda a. smith", "53", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "courtney e. combs", "54", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "kenneth d. keith", "54", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "bill coley", "55", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "joseph f. koziura", "56", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "earl j. martin", "57", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "matt lundy", "57", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "dan white", "58", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "matthew h. barrett", "58", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "kasey shidel", "59", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "kenneth a. carano", "59", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "robert f. hagan", "60", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "brant luther", "61", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "mark d. okey", "61", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "greg schmidt", "62", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "lorraine m. fende", "62", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "carol-ann schindel", "63", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "timothy j. cassell", "63", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "randy law", "64", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "tom letson", "64", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "arno a. hill", "65", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "sandra stabile harwood", "65", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "joseph w. uecker", "66", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "william newby", "66", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "shannon jones", "67", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "jeffrey a. ruppert", "67", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "bill davis", "68", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "kathleen chandler", "68", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "william g. batchelder", "69", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "jack schira", "69", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "kevin dewine", "70", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "kevin o'brien", "70", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jay hottinger", "71", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "thomas holliday", "71", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "ross w. mcgregor", "72", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "dale henry", "72", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "philip r. holloway", "73", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "jay p. goyal", "73", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "bruce w. goodwin", "74", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "ben mccullough", "74", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "lynn r. wachtmann", "75", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "angie b. byrne", "75", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "cliff hite", "76", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "john f. kostyo", "76", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "keith faber", "77", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "betsy marshall", "77", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "john adams", "78", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "adam ward", "78", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "diana fessler", "79", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "dave fisher", "79", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "edward j. enderle", "80", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "chris redfern", "80", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jeff wagner", "81", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "darrell w. opfer", "81", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "steve reinhard", "82", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "anthony e. core", "83", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "shawn allen", "83", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "christopher r. widener", "84", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "connie crockett", "84", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "john m. schlichter", "85", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "raymond allen pryor", "85", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "david t. daniels", "86", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "bill horne", "86", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "clyde evans", "87", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "nick d. rupert", "87", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "danny r. bubp", "88", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "tim knauff", "89", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "todd book", "89", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "thom collier", "90", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "duane grassbaugh", "90", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "william c. hayes", "91", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "dan dodd", "91", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jimmy stewart", "92", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "debbie phillips", "92", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "donald j. gadd", "93", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "jennifer d. garrison", "93", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "jim aslanides", "94", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "aaron b. phillips", "94", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "a. j. voytecek", "95", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "john domenick", "95", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "shane m. thompson", "96", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "allan r. sayre", "96", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "bob gibbs", "97", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "james e. riley", "97", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "matthew j. dolan", "98", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "raymond ku", "98", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "william pikor", "99", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "l. george distel", "99", data$district)

  print(unique(data$district))
  data$district <- as.numeric(data$district)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  #print(unique(data$district))
  data$district <- as.integer(data$district)
  # data$votes <- as.numeric(data$votes)
  # data$votes <- as.integer(data$votes)
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}
oh06 <- oh_2006_prep(oh_2006)

nodis <- oh06 %>%
  filter(is.na(district)) %>%
  filter(office == "State House")


oh_2008_prep <- function(data) {
  print(str(data))
  names(data)[names(data) == "COUNTY NAME"] <- "county"
  names(data)[names(data) == "STATE PRC CODE"] <- "precinct_code"
  names(data)[names(data) == "PRECINCT NAME"] <- "precinct"
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Votes"] <- "votes"
  data <- data %>%
    filter(Race != "") %>%
    filter(office != "")
  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office[data$office == "President/Vice-President"] <- "President"
  data$office[data$office == "State Representative"] <- "State House"
  data$county <- str_to_lower(data$county)
  data$precinct <- str_to_lower(data$precinct)
  data$precinct <- str_squish(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$candidate <- str_squish(data$candidate)
  data$candidate <- str_to_lower(data$candidate)
  data$precinct_code <- str_extract_part(data$precinct_code, pattern="-", before=FALSE)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))
  print(head(data))
  data <- oh_prep2(data)
  data$district <- ifelse(data$district == "60383", NA, data$district)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)

  data$district <- ifelse(data$party == "REP" & data$candidate == "kirk, mary-louise",
                          "13", data$district)
  data$district <- ifelse(data$party == "REP" & data$candidate == "schindel, carol-ann",
                          "63", data$district)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}

oh08 <- oh_2008_prep(oh_2008)

oh08 %>%
  filter(is.na(district))

oh08 <- oh_2008_prep(oh_2008)

oh_2010_prep <- function(data) {
  names(data)[names(data) == "COUNTY NAME"] <- "county"
  names(data)[names(data) == "PRECINCT_CODE"] <- "precinct_code"
  names(data)[names(data) == "PRECINCT_NAME"] <- "precinct"
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Votes"] <- "votes"

  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office[data$office == "State Representative"] <- "State House"
  data$office[data$office == "Governor/Lieutenant Governor"] <- "Governor"
  data$county <- str_to_lower(data$county)
  data$precinct <- str_to_lower(data$precinct)
  data$candidate <- str_to_lower(data$candidate)
  data$candidate <- str_squish(data$candidate)
  data$precinct <- str_squish(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))

  data <- oh_prep2(data)

  data$district <- ifelse(data$district == "208644", NA, data$district)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data <- insert_parties_2010(data)
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  data$party <- ifelse(is.na(data$party), "NPA", data$party)

  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}

oh10 <- oh_2010_prep(oh_2010)

oh_2012_prep <- function(data) {
  names(data)[names(data) == "County Name"] <- "county"
  names(data)[names(data) == "Precinct Code"] <- "precinct_code"
  names(data)[names(data) == "Precinct Name"] <- "precinct"
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Votes"] <- "votes"

  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office <- gsub("\n", "", data$office)
  data$office[data$office == "State Representative"] <- "State House"
  data$office[data$office == "U.S. Senator"] <- "U.S. Senate"
  data$county <- str_to_lower(data$county)
  data$precinct <- str_to_lower(data$precinct)
  data$precinct <- str_squish(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))

  data <- oh_prep2(data)

  data$district <- ifelse(data$district == "54243", NA, data$district)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)

  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}

#oh12 <- oh_2012_prep(oh_2012)

oh_2014_prep <- function(data) {
  names(data)[names(data) == "County"] <- "county"
  names(data)[names(data) == "Precinct Code"] <- "precinct_code"
  names(data)[names(data) == "Precinct Name"] <- "precinct"
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Votes"] <- "votes"

  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office[data$office == "State House of Representatives"] <- "State House"
  data$office[data$office == "Governor/Lieutenant Governor"] <- "Governor"

  data$county <- str_to_lower(data$county)
  data$precinct <- str_to_lower(data$precinct)
  data$precinct <- str_squish(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))
  data$candidate <- str_squish(data$candidate)
  data$candidate <- str_to_lower(data$candidate)

  print(head(data))
  data <- oh_prep2(data)
  data$district <- ifelse(data$district == "89999", NA, data$district)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)

  data$district <- ifelse(data$party == "REP" & data$candidate == "patty gascoyne -telischak",
                          "15", data$district)
  data$district <- ifelse(data$party == "DEM" & data$candidate == "michele lepore-hagan",
                          "58", data$district)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}
oh14 <- oh_2014_prep(oh_2014)

oh_2016_prep <- function(data) {
  names(data)[names(data) == "County Name"] <- "county"
  names(data)[names(data) == "Precinct Code"] <- "precinct_code"
  names(data)[names(data) == "Precinct Name"] <- "precinct"
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Votes"] <- "votes"

  data$office <- gsub("\n", "", data$office)
  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office[data$office == "State Representative"] <- "State House"
  data$office[data$office == "U.S. Senator"] <- "U.S. Senate"

  data$county <- str_to_lower(data$county)
  data$precinct <- str_to_lower(data$precinct)
  data$precinct <- str_squish(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  #data$precinct_code <- str_extract_part(data$precinct_code, pattern="-", before=FALSE)
  print(unique(data$precinct_code))
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$candidate <- str_to_lower(data$candidate)
  data$candidate <- str_squish(data$candidate)
  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))
  print(head(data))
  data <- oh_prep2(data)
  data$district <- ifelse(data$district == "50967", NA, data$district)
  data$district <- ifelse(data$candidat == "michele lepore-hagan" & data$party == "DEM", "58", data$district)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}
oh16 <- oh_2016_prep(oh_2016)

oh_2018_prep <- function(data) {
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Votes"] <- "votes"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "County Name"] <- "county"
  names(data)[names(data) == "Precinct Name"] <- "precinct"
  names(data)[names(data) == "Precinct Code"] <- "precinct_code"
  data$office <- gsub("\r", "", data$office)
  data$office <- gsub("\n", "", data$office)
  print(unique(data$office))
  data$office[data$office == "Governor and Lieutenant Governor"] <- "Governor"
  data$office[data$office == "State Representative"] <- "State House"
  data$office[data$office == "U.S. Senator"] <- "U.S. Senate"
  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office <- str_squish(data$office)
  data$precinct <- as.character(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$precinct <- str_squish(data$precinct)
  data$precinct <- str_to_lower(data$precinct)
  data$county <- str_to_lower(data$county)
  data$candidate <- str_to_lower(data$candidate)
  data$candidate <- str_squish(data$candidate)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))
  data <- oh_prep2(data)
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)
  data$district <- ifelse(data$district == "33860", NA, data$district)
  data$district <- ifelse(data$candidate == "samantha thomas-bush" & data$party == "DEM", "93", data$district)
  data$district <- ifelse(data$candidate == "mary e. pierce- broadwater" & data$party == "DEM", "87", data$district)
  data$district <- ifelse(data$candidate == "paula hicks-hudson" & data$party == "DEM", "87", data$district)
  data$district <- ifelse(data$candidat == "michele lepore-hagan" & data$party == "DEM", "58", data$district)
  data$district <- ifelse(data$candidat == "aidan hubbell-staeble" & data$party == "DEM", "3", data$district)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  print(unique(data$office))
  data <- data %>%
    filter(!is.na(office)) %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}

oh_2020_prep <- function(data) {
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Votes"] <- "votes"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "County Name"] <- "county"
  names(data)[names(data) == "Precinct Name"] <- "precinct"
  names(data)[names(data) == "Precinct Code"] <- "precinct_code"
  data$office <- gsub("\n", "", data$office)
  data$office[data$office == "President and Vice President"] <- "President"
  data$office[data$office == "State Representative"] <- "State House"
  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office <- str_squish(data$office)
  data$precinct <- as.character(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$precinct <- str_squish(data$precinct)
  data$precinct <- str_to_lower(data$precinct)
  data$county <- str_to_lower(data$county)
  data$candidate <- str_to_lower(data$candidate)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")

  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))
  data <- oh_prep2(data)
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)
  data$district <- ifelse(data$district == "33860", NA, data$district)
  data$district <- ifelse(data$candidate == "nancy day-achauer" & data$party == "DEM", "23", data$district)
  data$district <- ifelse(data$candidate == "paula hicks-hudson" & data$party == "DEM", "44", data$district)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  print(unique(data$office))
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}

oh_2022_prep <- function(data) {
  names(data)[names(data) == "Year"] <- "year"
  names(data)[names(data) == "Votes"] <- "votes"
  names(data)[names(data) == "Office"] <- "office"
  names(data)[names(data) == "Party"] <- "party"
  names(data)[names(data) == "Candidate"] <- "candidate"
  names(data)[names(data) == "County Name"] <- "county"
  names(data)[names(data) == "Precinct Name"] <- "precinct"
  names(data)[names(data) == "Precinct Code"] <- "precinct_code"
  data$office <- gsub("\n", "", data$office)
  print(unique(data$office))
  data$office[data$office == "Governor and Lieutenant Governor"] <- "Governor"
  data$office[data$office == "State Representative"] <- "State House"
  data$office[data$office == "U.S. Senator"] <- "U.S. Senate"
  data$party[data$party == "D"] <- "DEM"
  data$party[data$party == "R"] <- "REP"
  data$office <- str_squish(data$office)
  data$precinct <- as.character(data$precinct)
  data$precinct_code <- str_to_lower(data$precinct_code)
  data$precinct <- str_squish(data$precinct)
  data$precinct <- str_to_lower(data$precinct)
  data$county <- str_to_lower(data$county)
  data$candidate <- str_to_lower(data$candidate)
  data$cw_concat <- paste(data$county, data$precinct_code, sep=" ")
  data$votes <- as.numeric(data$votes)
  data$votes <- as.integer(data$votes)
  data$year <- as.numeric(data$year)
  data$district <- as.character(nrow(data))
  data <- oh_prep2(data)
  data$contest_dem <- ifelse(data$party == "DEM", 1, 0)
  data$contest_rep <- ifelse(data$party == "REP", 1, 0)
  data$district <- ifelse(data$district == "104625", NA, data$district)
  data$votes <- ifelse(is.na(data$votes), 0, data$votes)
  data$district <- as.numeric(data$district)
  data$district <- as.integer(data$district)
  print(unique(data$office))
  data <- data %>%
    filter(office != "") %>%
    filter(precinct != "absentee") %>%
    dplyr::select(c(county, precinct_code, precinct, votes, year, office, cw_concat,
                    candidate, party, district, contest_dem, contest_rep))
  return(data)
}

oh_prep2 <- function(data) {
  print("function triggered")
  df2 <- data %>%
    filter(office == "State House")
  df1 <- data %>%
    filter(office != "State House")
  if (data$year[1] == 2018) {
    df2$Race <- gsub("\r\n", "", df2$Race)
    df2$district <- ex_between(df2$Race, "-", "-")
  }
  else {
    df2$Race <- gsub("\n", "", df2$Race)
    df2$district <- ex_between(df2$Race, "-", "-")
    #print(df2$district)
  }

  df2$district <- gsub("District ", "", df2$district)
  df2$district <- str_squish(df2$district)
  df2$district <- as.numeric(df2$district)
  df3 <- rbind(df1, df2)
  return(df3)
}

insert_parties_2010 <- function(oh_2010m) {
  for (district in oh_2010m) {
    oh_2010m$party[oh_2010m$candidate == "bolon, linda"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "newbold, craig"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "bird, richard"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "brenner, andrew"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "amstutz, ron"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "miller, connie"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "huffman, matt"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "stebelton, gerald"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "brown, jackie"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "gardner, randy"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "yuko, kenny"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "hocevar, tony"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "budish, armond"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "powell, jerry"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "boyd, barbara"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "perkel, charles"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "patmon, bill"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "williams, sandra"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "mcallister, roz"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "barnes, john"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "antonio, nickie"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "foley, mike"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "burke, kevin"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "degeeter, timothy"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "brady, jennifer"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "baker, nan"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "perk, kelli"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "anielski, marlene"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "patten, matt"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "dovilla, mike"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "harris, marian"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "gonzales, anne"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "garland, nancy"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "carle, matt"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "robinson, david"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "duffey, mike"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "carney, john"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "rhodes, angel"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "harp, steven"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "grossman, cheryl"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "celeste, ted"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "larger, nathan"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "stinziano, michael"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "heard, tracy"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "healy, joseph"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "weddington, w."] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "cyrus, meagan"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "pillich, connie"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "wilson, mike"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "ping, liz"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "blessing, louis"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "luken, richard"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "mecklenborg, bob"] <- "REP" ## 30
    oh_2010m$party[oh_2010m$candidate == "driehaus, denise"] <- "DEM" ## 31
    oh_2010m$party[oh_2010m$candidate == "robison, mike"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "mallory, dale"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "nebergall, erik"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "reece, alicia"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "stith, jim"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "kinman, max"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "stautberg, peter"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "garrison, marcia"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "maag, ron"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "fisher, carl"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "henne, michael"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "byington, steven"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "lehner, peggy"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "doll, john"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "blair, terry"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "luckie, clayton"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "winburn, roland"] <- "DEM" ## 40
    oh_2010m$party[oh_2010m$candidate == "williams, brian"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "slaby, lynn"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "moran, mike"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "roegner, kristina"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "dyer, steve"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "mckenney, todd"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "sykes, vernon"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "sines, josh"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "milkovich, zack"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "lasher, charles"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "barlos, harry"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "sears, barbara"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "fedor, teresa"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "bush, rick"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "ashford, michael"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "eyre, carolyn"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "szollosi, matthew"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "wingate, jeff"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "bosley, todd"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "snitchler, todd"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "haines, andrew"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "schuring, kirk"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "slesnick, stephen"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "secrest, travis"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "shew, james"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "derickson, timothy"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "carter, bruce"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "combs, courtney"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "rubin, suzi"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "coley, bill"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "ramos, dan"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "lewandowski, henry"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "lundy, matt"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "brady, rae"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "davidson, gregory"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "boose, terry"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "gerberry, ronald"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "pestian, mike"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "hagan, robert"] <- "DEM" ## 60
    oh_2010m$party[oh_2010m$candidate == "thimons, daniel"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "okey, mark"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "foit, michael"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "fende, lorraine"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "fiebig, david"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "schneider, mark"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "young, ron"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "letson, tom"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "haberstroh, albert"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "o'brien, sean"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "capone, geno"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "uecker, joe"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "howard, anne"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "beck, pete"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "clyde, kathleen"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "zeller, w. roak"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "schira, jack"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "batchelder, william"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "watters, michael"] <- "DEM" # 70
    oh_2010m$party[oh_2010m$candidate == "martin, jarrod"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "mcmann, nathan"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "hottinger, jay"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "krouse, gregory"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "mcgregor, ross"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "goyal, jay"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "nitzsche, david"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "pilliod, david"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "goodwin, bruce"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "schindler, cletus"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "wachtmann, lynn"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "detmer, jeffrey"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "hite, cliff"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "zehringer, james"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "adams, john"] <- "REP" ## 78 - 99
    oh_2010m$party[oh_2010m$candidate == "ehresmann, anthony"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "adams, richard"] <- "REP" #79
    oh_2010m$party[oh_2010m$candidate == "payne, howard"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "murray, dennis"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "krabill, jeff"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "nutter, benjamin"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "damschroder, rex"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "mcclain, jeffrey"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "burke, dave"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "kilbarger, aaron"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "hackett, robert"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "pryor, raymond"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "peterson, bob"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "horne, bill"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "rosenberger, cliff"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "deel, fred"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "carey, john"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "bubp, danny"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "hadsell, ron"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "johnson, terry"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "ryerson, john"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "ruhl, margaret"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "dodd, dan"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "hayes, bill"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "phillips, debbie"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "hunter, mike"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "secrest, linda"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "thompson, andy"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "ackers, john"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "balderson, troy"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "gentile, lou"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "o'farrell, joshua"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "landis, al"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "powers, edward"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "hall, dave"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "briggs, mary"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "grendell, timothy"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "newcomb, deborah"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "kozlowski, casey"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "dewine, mike"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "portman, rob"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "strickland, ted"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "kasich, john"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "cordray, richard"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "fisher, lee"] <- "DEM"
    oh_2010m$party[oh_2010m$candidate == "husted, jon"] <- "REP"
    oh_2010m$party[oh_2010m$candidate == "o'shaughnessy, maryellen"] <- "DEM"
  }
  oh_2010m$contest_dem <- ifelse(oh_2010m$party == "DEM", 1, 0)
  oh_2010m$contest_rep <- ifelse(oh_2010m$party == "REP", 1, 0)
  return(oh_2010m)
}

oh2004 <- oh_2004_prep(oh_2004)
oh2006 <- oh_2006_prep(oh_2006)
oh2008 <- oh_2008_prep(oh_2008)
oh2010 <- oh_2010_prep(oh_2010)
oh2012 <- oh_2012_prep(oh_2012)
oh2014 <- oh_2014_prep(oh_2014)
oh2016 <- oh_2016_prep(oh_2016)
oh2018 <- oh_2018_prep(oh_2018)
oh2020 <- oh_2020_prep(oh_2020)
oh2022 <- oh_2022_prep(oh_2022)

oh_data <- list("2004"=oh2004, "2006"=oh2006, "2008"=oh2008, "2010"=oh2010, "2012"=oh2012,
                "2014"=oh2014, "2016"=oh2016, "2018"=oh2018, "2020"=oh2020, "2022"=oh2022)

