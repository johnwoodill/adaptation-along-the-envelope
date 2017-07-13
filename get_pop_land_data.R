library(tidyverse)

# https://www.bea.gov/api/data/?UserID=543C6E18-FF72-4461-9365-4FFA458CFBD4&
#   method=GetData&datasetname=RegionalIncome&TableName=CA1&LineCode=1&Year=2012,2013&GeoFips=COUNTY&ResultFormat=csv


library(bea.R)
beaKey = "543C6E18-FF72-4461-9365-4FFA458CFBD4"

#beaSearch('population density', beaKey = beaKey)


beaUpdateMetadata('RegionalData',  beaKey = "543C6E18-FF72-4461-9365-4FFA458CFBD4")
dat <- data.frame()

years <- 1969:2015
for (i in years){
  
  pop_beaSpecs <- list(
  	'UserID' = "543C6E18-FF72-4461-9365-4FFA458CFBD4",
  	'method' = 'GetData',
  	'datasetname' = 'RegionalIncome',
  	'TableName' = 'CA1',
  	'LineCode' = '2', # Population
  	'Year' = i,
  	'GeoFips' = 'COUNTY',
  	'ResultFormat' = 'json'
  )
  
    ipc_beaSpecs <- list(
  	'UserID' = "543C6E18-FF72-4461-9365-4FFA458CFBD4",
  	'method' = 'GetData',
  	'datasetname' = 'RegionalIncome',
  	'TableName' = 'CA1',
  	'LineCode' = '3',  # Income per capita
  	'Year' = i,
  	'GeoFips' = 'COUNTY',
  	'ResultFormat' = 'json'
  )

    pop <- beaGet(pop_beaSpecs)
    pop <- pop[, c(2,6)]
    names(pop) <- c("fips", "population")
    ipc <- beaGet(ipc_beaSpecs)
    ipc <- ipc[, c(2, 6)]
    names(ipc) <- c("fips", "ipc")
    comb <- left_join(pop, ipc, by = "fips")
    comb$year <- i
    dat <- rbind(dat, comb)
}

dat <- select(dat, year, fips, population, ipc)
dat$fips <- as.numeric(dat$fips)
saveRDS(dat, "data/pop_ipc.rds")
dat <- readRDS("data/pop_ipc.rds")

# Get population and land data from census 1960-2000
popland <- read_csv("http://www2.census.gov/library/publications/2010/demo/p25-1139st1.csv")

# or
#popland <- read_csv("data/census_pop_land_1960-2000.csv")

popland <- select(popland, ',STCOU', 'LANDAREA1960', 'LANDAREA1970', 'LANDAREA1980', 'LANDAREA1990', 'LANDAREA2000')
names(popland) <- c("fips", "1960", "1970", "1980", "1990", "2000")  
popland <- gather(popland, key = merge_year, value = land_sqm, -fips)
popland$merge_year <- as.numeric(popland$merge_year)
popland$fips <- as.numeric(popland$fips)
popland$land_sqm <- as.numeric(popland$land_sqm)
dat$merge_year <- (dat$year %/% 10) * 10

dat <- left_join(dat, popland, by = c("merge_year", "fips"))

dat$pop_dens <- dat$population/dat$land_sqm
dat <- select(dat, year, fips, population, pop_dens, land_sqm, ipc)
saveRDS(dat, "data/pop_ipc.rds")

# # Get 2010 gazateer data
# land <- readRDS("data/land_area_2010.rds")
# land$year <- 2010



land <- readRDS("data/land_area_2010.rds")
dat <- left_join(dat, land, by = "fips")
library(censusapi)
apis <- listCensusApis()
View(apis)
