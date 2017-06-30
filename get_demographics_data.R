library(tidyverse)

# https://www.bea.gov/api/data/?UserID=543C6E18-FF72-4461-9365-4FFA458CFBD4&
#   method=GetData&datasetname=RegionalIncome&TableName=CA1&LineCode=1&Year=2012,2013&GeoFips=COUNTY&ResultFormat=csv


library(bea.R)
beaSearch('population density', beaKey = beaKey)

beaKey = "543C6E18-FF72-4461-9365-4FFA458CFBD4"
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
saveRDS(dat, "data/pop_ipc.rds")
dat <- readRDS("data/pop_ipc.rds")

land <- readRDS("data/land_area_2010.rds")
land <- left_join(dat, land, by = "fips")
library(censusapi)
apis <- listCensusApis()
View(apis)
