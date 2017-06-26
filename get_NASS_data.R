library(rnassqs)
library(tidyverse)

NASSQS_TOKEN = "2BD928ED-10EB-3FB3-9B42-F3F237A067AE"

# State-level Prices ------------------------------------------------------

states <- state.abb
dat <- data.frame()

# Loop through all states, query database, and bind together
for (i in unique(states)){
  params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="CORN", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      cdat <- data.frame()
      cdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="COTTON", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="COTTON, UPLAND - PRICE RECEIVED, MEASURED IN $ / LB")
    b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      ctdat <- data.frame()
      ctdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="HAY", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="HAY - PRICE RECEIVED, MEASURED IN $ / TON")
    c <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat <- data.frame()
      hdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="WHEAT", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="WHEAT - PRICE RECEIVED, MEASURED IN $ / BU")
    d <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      wdat <- data.frame()
      wdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="SOYBEANS", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU")
    e <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      sdat <- data.frame()
      sdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

    dat <- rbind(dat, cdat, ctdat, hdat, wdat, sdat)
    # dat <- filter(dat, data.reference_period_desc == "MARKETING YEAR")
    print(i)
}

# If only monthly observations exist, then use average
dat$data.Value <- as.numeric(dat$data.Value)
newdat <- dat %>%
  group_by(data.year, data.state_alpha, data.short_desc) %>%
  summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                           data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                           mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
  ungroup()

# Tidy up data
newdat <- select(newdat, data.year, data.state_alpha, data.short_desc, data.Value)
names(newdat) <- c("year", "state", "type", "value")
newdat <- spread(newdat, type, value)
names(newdat) <- c("year", "state", "corn_nprice", "cotton_nprice", "hay_nprice", "soybean_nprice", "wheat_nprice")
newdat$year <- as.integer(newdat$year)
newdat$state <- tolower(newdat$state)

# Save to 'data/'
saveRDS(newdat, "/run/media/john/1TB/MEGA/Projects/adaptation-and-an-envelope/data/crop_statelevel_prices.RDS")

################################
states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  
# Get acres harvested
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="CORN", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="CORN, GRAIN - ACRES HARVESTED")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat = nassqs_parse(req)
      check <- 1
    },error=function(e) e)
  
  # Get Production
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="CORN", "source_desc"="SURVEY", "statisticcat_desc" = "PRODUCTION", "short_desc"="CORN, GRAIN - PRODUCTION, MEASURED IN BU")
      b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      pdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
  if(check == 2){
    dat <- rbind(dat, hdat, pdat)
    print(i)
  }
  }


corn <- select(dat, data.year, data.state_alpha, data.state_fips_code, data.county_code, data.county_name, data.short_desc, data.Value)
names(corn) <- c("year", "state", "state_fips", "county_fips", "county", "data_item", "value")
corn$fips <- paste(corn$state_fips, corn$county_fips, sep = "")
corn <- select(corn, year, state, fips, data_item, value)
head(corn)
min(corn$year)
max(corn$year)

write_csv(corn, "corn_1919-2016.csv")



soybean <- select(dat, data.year, data.state_alpha, data.state_fips_code, data.county_code, data.county_name, data.short_desc, data.Value)
names(soybean) <- c("year", "state", "state_fips", "county_fips", "county", "data_item", "value")
soybean$fips <- paste(soybean$state_fips, soybean$county_fips, sep = "")
soybean <- select(soybean, year, state, fips, data_item, value)
write_csv(soybean, "soybean_statelevel_1927-2016.csv")

# State
cotton <- filter(dat, data.reference_period_desc == "YEAR" | data.reference_period_desc == "MARKETING YEAR")
cotton <- select(cotton, data.year, data.state_alpha,  data.short_desc, data.Value)
names(cotton) <- c("year", "state", "data_item", "value")
cotton <- select(cotton, year, state, data_item, value)
which(is.character(cotton$value))
min(cotton$year)
head(cotton)
cotton$value <- str_trim(cotton$value)
cotton <- filter(cotton, value != "(D)")
write_csv(cotton, "cotton_statelevel_1917-2016.csv")
