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

# Extract NASS crop data at state level
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

# Extract NASS crop data at county level
extract_d_county <- function(x){
  names(x) <- c("year", "state", "county", "data_item", "value")
  crop <- x
  crop$state <- tolower(crop$state)
  crop$county <- tolower(crop$county)
  crop$row <- 1:nrow(crop)   # unique identifer
  crop <- spread(crop, data_item, value = value, fill = NA)
  crop$row <- NULL
  #crop <- filter(crop, year >= 1900)
  #crop <- crop[,c(1,2,4,7,10,13,16)]
  crop <- crop %>% 
    group_by(state, county, year) %>% 
    summarise_each(funs(sum(., na.rm=TRUE))) 
  return(crop)
}


# Aggregate state level prices --------------------------------------------

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
crop_prices <- crop[,c(1,2,4,7,10,13,16)]

names(crop_prices) <- c("state", "year", "wheat_price", "corn_price", "hay_price", "soybean_price", "cotton_price")
crop_prices <- as.data.frame(crop_prices)
crop_prices <- select(crop_prices, year, state, wheat_price, corn_price, hay_price, soybean_price, cotton_price)
crop_prices

# Aggregate county level ag data ------------------------------------------


# Load crop data (balanced years 1927-2007)
corn <- read_csv("data/corn_1910-2016.csv")
corn <- extract_d_county(corn)
names(corn) <- c("state", "county", "year", "corn_grain_a", "corn_grain_p")

cotton <- read_csv("data/cotton_1919-2015.csv")
cotton <- extract_d_county(cotton)
names(cotton) <- c("state", "county", "year" , "cotton_a", "cotton_p")

hay <- read_csv("data/hay_1919-2008.csv")
hay <- extract_d_county(hay)
names(hay) <- c("state", "county", "year" , "hay_a", "hay_p")

wheat <- read_csv("data/wheat_1909-2007.csv")
wheat <- extract_d_county(wheat)
names(wheat) <- c("state", "county", "year" , "wheat_a", "wheat_p")

soybean <- read_csv("data/soybean_1927-2016.csv")
soybean <- extract_d_county(soybean)
names(soybean) <- c("state", "county", "year" , "soybean_a", "soybean_p")

# Get all combinations of years and states
newgrid <- expand.grid(county.fips$fips, 1900:2016, stringsAsFactors = FALSE)
mergdat <- data.frame(county = county.fips$fips, name = county.fips$state)
statedat <- select(states, state, name)
statedat$state <- tolower(statedat$state)
statedat$name <- tolower(statedat$name)
mergdat <- left_join(mergdat, statedat, by = c("name"))
mergdat <- select(mergdat, county, state)
names(newgrid) <- c("county", "year")
newgrid <- left_join(newgrid, mergdat, by = "county")


newgrid$county <- as.character(newgrid$county)


cropdat <- left_join(newgrid, corn, by = c("state", "county", "year"))
cropdat <- left_join(cropdat, cotton, by = c("state", "county", "year"))
cropdat <- left_join(cropdat, hay, by = c("state", "county", "year"))
cropdat <- left_join(cropdat, wheat, by = c("state", "county", "year"))
cropdat <- left_join(cropdat, soybean, by = c("state", "county", "year"))

names(cropdat)[1] <- "fips"
cropdat$fips <- as.integer(cropdat$fips)

cropdat$corn_yield <- cropdat$corn_grain_p/cropdat$corn_grain_a
cropdat$cotton_yield <- cropdat$cotton_p/cropdat$cotton_a
cropdat$hay_yield <- cropdat$hay_p/cropdat$hay_a
cropdat$wheat_yield <- cropdat$wheat_p/cropdat$wheat_a
cropdat$soybean_yield <- cropdat$soybean_p/cropdat$soybean_a

rm(list=setdiff(ls(), c("crop_prices", "cropdat", "extract_d_state", "extract_d_county", "dd_dat")))
gc()

# Aggregate county-level degree days -----------------------------------------------

#dd <- read_csv("/run/media/john/1TB/MEGA/Projects/Adaptation and an Envelope/data/fips_degree_days_1900-2013.csv")
#prec <- read_csv("/run/media/john/1TB/MEGA/Projects/Adaptation and an Envelope/data/fips_precipitation_1900-2013.csv")

dd <- read_csv("/home/john/MEGA/Projects/adaptation-and-an-envelope/data/fips_degree_days_1900-2013.csv")
prec <- read_csv("/home/john/MEGA/Projects/adaptation-and-an-envelope/data/fips_precipitation_1900-2013.csv")


dd_dat <- left_join(dd, prec, by = c("fips", "year", "month"))
dd_dat$X1 <- NULL

dd_dat <- dd_dat %>% 
    group_by(year, fips) %>% 
    summarise(dday0C = sum(dday0C),
            dday1C = sum(dday1C),
            dday2C = sum(dday2C),
            dday3C = sum(dday3C),
            dday4C = sum(dday4C),
            dday5C = sum(dday5C),
            dday6C = sum(dday6C),
            dday7C = sum(dday7C),
            dday8C = sum(dday8C),
            dday9C = sum(dday9C),
            dday10C = sum(dday10C),
            dday11C = sum(dday11C),
            dday12C = sum(dday12C),
            dday13C = sum(dday13C),
            dday14C = sum(dday14C),
            dday15C = sum(dday15C),
            dday16C = sum(dday16C),
            dday17C= sum(dday17C),
            dday18C = sum(dday18C),
            dday19C = sum(dday19C),
            dday20C = sum(dday20C),
            dday21C = sum(dday21C),
            dday22C = sum(dday22C),
            dday23C = sum(dday23C),
            dday24C = sum(dday24C),
            dday25C = sum(dday25C),
            dday26C = sum(dday26C),
            dday27C = sum(dday27C),
            dday28C = sum(dday28C),
            dday29C = sum(dday29C),
            dday30C = sum(dday30C),
            dday31C = sum(dday31C),
            dday32C = sum(dday32C),
            dday33C = sum(dday33C),
            dday34C = sum(dday34C),
            dday35C = sum(dday35C),
            ndday0C = sum(ndday0C),
            prec = sum(ppt),
            tmin = mean(tmin),
            tmax = mean(tmax),
            tavg = mean(tavg))

rm(list=setdiff(ls(), c("crop_prices", "cropdat", "extract_d_state", "extract_d_county", "dd_dat")))
gc()



# Merge ag prices, ag crop data, and degree day data ----------------------

fulldat <- left_join(cropdat, crop_prices, by = c("year", "state"))

# Organize before degree day merge
fulldat <- select(fulldat, year, state, fips, corn_grain_a, corn_grain_p, corn_price, corn_yield,
                  cotton_a, cotton_p, cotton_price, cotton_yield,
                  hay_a, hay_p, hay_price, hay_yield,
                  wheat_a, wheat_p, wheat_price, wheat_yield,
                  soybean_a, soybean_p, soybean_price, soybean_yield)

# Merge degree days
fulldat <- left_join(fulldat, dd_dat, by = c("year", "fips"))

# Convert inf to NA
fulldat <- do.call(data.frame,lapply(fulldat, function(x) replace(x, is.infinite(x),NA)))

# # Convert to numeric
# for (i in 4:64){
#   fulldat[,i] <- as.numeric(as.character(fulldat[,i]))
#   print(i)
# }

write.csv(fulldat, "data/full_ag_data.csv", row.names = FALSE)
saveRDS(fulldat, "data/full_ag_data.rds")
fulldat <- read_csv("data/full_ag_data.csv")
