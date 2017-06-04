library(readr)
library(dplyr)
library(tidyr)
library(rms)
library(noncensus)
library(maps)
library(stringr)

# Function to extract data

data(county.fips)
county.fips$state <- sapply(str_split(county.fips$polyname, ","),'[',1)
county.fips$county <- sapply(str_split(county.fips$polyname, ","),'[',2)
county.fips <- select(county.fips, fips, county, state)

# Extract NASS crop data
extract_d_state <- function(x){
  crop <- select(x, year, state, data_item, value)
  names(crop) <- tolower(names(crop))
  crop$state <- tolower(crop$state)
  crop$row <- 1:nrow(crop)   # unique identifer
  crop <- spread(crop, data_item, value = value, fill = NA)
  crop$row <- NULL
  #crop <- filter(crop, year >= 1900)
  #crop <- crop[,c(1,2,4,7,10,13,16)]
  crop <- crop %>% 
    group_by(state, year) %>% 
    summarise_each(funs(sum(., na.rm=TRUE))) 
  return(crop)
}

corn <- read_csv("data/corn_statelevel_1866-2016.csv")
wheat <- read_csv("data/wheat_statelevel_1866-2016.csv")
cotton <- read_csv("data/cotton_statelevel_1917-2016.csv")
hay <- read_csv("data/hay_statelevel_1908-2016.csv")
soybeans <- read_csv("data/soybean_statelevel_1913-2016.csv")

corn <- extract_d_state(corn)
wheat <- extract_d_state(wheat)
hay <- extract_d_state(hay)
soybeans <- extract_d_state(soybeans)
cotton <- extract_d_state(cotton)

# Get all combinations of years and states
data(states)

newgrid <- expand.grid(tolower(states$state), 1900:2016, stringsAsFactors = FALSE)
names(newgrid) <- c("state", "year")

crop <- left_join(newgrid, wheat, by = c("year", "state"))
crop <- left_join(crop, corn, by = c("year", "state")) 
crop <- left_join(crop, hay, by = c("year", "state")) 
crop <- left_join(crop, soybeans, by = c("year", "state")) 
crop <- left_join(crop, cotton, by = c("year", "state"))

crop <- filter(crop, year >= 1900)

# Get prices
crop <- crop[,c(1,2,4,7,10,13,16)]

names(crop) <- c("state", "year", "wheat_price", "corn_price", "hay_price", "soybean_price", "cotton_price")
crop <- as.data.frame(crop)
crop <- select(crop, year, state, wheat_price, corn_price, hay_price, soybean_price, cotton_price)
write.csv(crop, "data/commodity_statelevel_prices.csv", row.names = FALSE)
