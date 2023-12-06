contested_part_2000 <-

#Creating a function (I think I want to see if I can make this a function factory at some point later)

off_sub_year <- function(year_data){
  year_data <- year_data %>%
    filter(office == "House" | office == "State Senate" | office == "State Assembly" | office == "Senate" | office == "President" | office == "Attorney General" | office == "Secretary of State")
  return(year_data)
}

#Maybe this can be automated?

wi_2000 <- off_sub_year(wi_2000)
wi_2002 <- off_sub_year(wi_2002)
wi_2004 <- off_sub_year(wi_2004)
wi_2006 <- off_sub_year(wi_2006)
wi_2008 <- off_sub_year(wi_2008)
wi_2010 <- off_sub_year(wi_2010)
wi_2012 <- off_sub_year(wi_2012)
wi_2014 <- off_sub_year(wi_2014)
wi_2016 <- off_sub_year(wi_2016)
wi_2018 <- off_sub_year(wi_2018)
wi_2020 <- off_sub_year(wi_2020)
wi_2022 <- off_sub_year(wi_2022)

## Making a vector of positions in each district

off_list <- function(election){
  pos_types <- unique(election$office)
  positions <- c()
  if ("President" %in% pos_types){
    positions <- append(x = positions, values = "President")
  }
  #This might need to be edited
  if ("Senate" %in% pos_types){
    positions <- append(x = positions, values = "Senate")
  }
  if ("Attorney General" %in% pos_types){
    positions <- append(x = positions, values = "Attorney General")
  }
  if ("Secretary of State" %in% pos_types){
    positions <- append(x = positions, values = "Secretary of State")
  }
  return(positions)
}

off_list(wi_2000)
off_list(wi_2002)

