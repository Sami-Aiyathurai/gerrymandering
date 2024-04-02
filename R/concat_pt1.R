## CHECKING CO
co_2022 <- access_state_year("2022", co_data)
co_2020 <- access_state_year("2020", co_data)
co_2018 <- access_state_year("2018", co_data)
co_2016 <- access_state_year("2016", co_data)
co_2014 <- access_state_year("2014", co_data)
co_2012 <- access_state_year("2012", co_data)
co_2010 <- access_state_year("2010", co_data)
co_2008 <- access_state_year("2008", co_data)
co_2006 <- access_state_year("2006", co_data)
co_2004 <- access_state_year("2004", co_data)
str(co_2022) # precincts are characters
cc <- str_detect(co_2022, "1260116126")
cohd122 <- co_2022 %>%
  filter(district == 1) %>%
  filter(office == "State House")
mihd12010 <- mi_data[[6]] %>%
  filter(district == 1) %>%
  filter(office == "State House")
## NEW PLAN
disnum <- as.numeric(unique(mi_data[[6]]$district))
year_data <- mi_data[[6]]
for (i in seq_along(disnum)) {
  districts <- year_data$district[i]
  print(districts)
}
concat_precinct <- function(year_data) { #
  strp <- year_data$precinct
  for (i in strp) {
    stru <- str_to_upper(strp)
    str_nc <- gsub('[^:alnum:] ]', "", stru)
    str_ns <- str_replace_all(str_nc, " ", repl="")
    year_data$precinct <- gsub(str_ns, strp, year_data$precinct) # I'm pretty sure this keeps looping
    print(unique(year_data$precinct))
    # str2 <- str_transform(year_data, i)
    # year_data$precinct[i] <- str2
  }
  # return(year_data)
}
cp <- concat_precinct(year_data)
## trying to make a function that will take in the strings and spit out the properly formatted ones
str_transform <- function(year_data, i) {
  stri <- year_data$precinct[i] # for each precinct in year_data$precinct
  stru <- str_to_upper(str1) # make upper case
  str_nc <- gsub('[^:alnum:] ]', "", stru) # remove special characters
  str_ns <- str_replace_all(str_nc, " ", repl="")
  return(str_ns)
}
strp <- year_data$precinct
stru <- str_to_upper(strp)
str_nc <- gsub('[^:alnum:] ]', "", stru)
str_ns <- str_replace_all(str_nc, " ", repl="") # this string is produced correctly!!
year_data$precinct2 <- gsub(as.character(str_ns), strp, year_data$precinct) # this isn't working
# this is the precinct replacing code that works but now I just need to append it in the right place, current issue of repeating
str1 <- mihd12010$precinct[1]
stru <- str_to_upper(str1)
str_mod <- gsub('[^:alnum:] ]', "", stru)
str_ns <- str_replace_all(str_mod, " ", repl="")
year_data$precinct <- gsub(str_ns, strp, year_data)
## this needs to be fixed
## idea: for every state house district, concatenate district number with candidate name to track incumbents but this doesn't
## account for redistricted
favorite_races <- year_data %>%
  dplyr::filter(office == "President" | office == "Governor" | office == "State House" | office == "U.S. Senate" |
                  office == "Secretary of State" | office == "Attorney General")
favorite_races$candcon <- as.character(nrow(favorite_races))
for (i in favorite_races) {
  favorite_races$candcon <- paste(favorite_races$candidate, favorite_races$district, sep="-")
  # ifelse(is.na(favorite_races$district),
  #        favorite_races$candcon <- paste(favorite_races$candidate, favorite_races$district, sep="-"), ## 9999 isn't going in but NA is so I guess that's a win??
  #        favorite_races$candcon <- paste(favorite_races$candidate, favorite_races$district, sep="-"))
  favorite_races$candcon <- str_to_upper(favorite_races$candcon)
  favorite_races$candcon <- str_replace_all(favorite_races$candcon, " ", repl="")
  # if (is.na(favorite_races$district)) {
  #   favorite_races$candcon <- paste0(candidate, 9999)
  # }
}
for (i in favorite_races) {
  favorite_races$precinct2 <- str_to_upper(favorite_races$precinct2)
  favorite_races$precinct2 <- str_replace_all(favorite_races$precinct2, " ", repl="")
  favorite_races$precinct <- gsub('[^:alnum:] ]', "", favorite_races$precinct)
}

## THIS IS THE ONE THAT WORKS!!!

concat_func <- function(year_data) {
  favorite_races <- year_data %>%
    dplyr::filter(office == "President" | office == "Governor" | office == "State House" | office == "U.S. Senate" |
                    office == "Secretary of State" | office == "Attorney General")
  for (i in favorite_races) {
    favorite_races$candcon <- paste(favorite_races$candidate, favorite_races$district, sep="-")
    favorite_races$candcon <- str_to_upper(favorite_races$candcon) # put in a gsub that takes out middle initials (before . and after " ")
    favorite_races$candcon <- str_replace_all(favorite_races$candcon, " ", repl="")
    favorite_races$precinct2 <- str_to_upper(favorite_races$precinct2)
    favorite_races$precinct2 <- str_replace_all(favorite_races$precinct2, " ", repl="")
    favorite_races$precinct <- gsub('[^:alnum:] ]', "", favorite_races$precinct)
  }
  return(favorite_races)
}
concat_func(year_data)
## if doing this, need to produce some sort of conditionals where statewide races need to be -99 or something to flag that it's different
## need to filter the whole DF for only the races that I care about to limit the possibility of other districts getting in the way
## but grep() returns the index at which the pattern is found in the vector, taes pattern, string, ignore.case=FALSE
## or str_detect which returns TF
sh_data <- year_data %>%
  filter(office == "State House")
sh_data$concand <- paste0()
