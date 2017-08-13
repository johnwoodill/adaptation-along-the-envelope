setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cs.dat <- readRDS("data/cross_section_regression_data.rds")
cs.dat <- select(cs.dat, -dday0C, -dday10C, -dday15C, -dday17C, -dday29C, -dday30C, -dday32C, -dday33C, -dday34C, -prec, -tavg)

p.dat <- readRDS("data/panel_regression_data.rds")
p.dat <- select(p.dat, -dday0C, -dday8C, -dday10C, -dday15C, -dday17C, -dday20C, -dday29C, -dday30C, -dday32C, -dday33C, -dday34C, -dday35C, -prec, -tavg, -ndday0C)

#  
dd <- read_csv("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/data/fips_degree_days_1900-2013.csv")
prec <- read_csv("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/data/fips_precipitation_1900-2013.csv")

 dd$year <- as.integer(dd$year)
 dd$fips <- as.integer(dd$fips)
 dd_dat <- left_join(dd, prec, by = c("fips", "year", "month"))
 dd_dat <- filter(dd_dat, month >= 3 & month <= 9)
 #dd_dat <- filter(dd_dat, month <= 5 | month >= 9)
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

cs.dd <- dd_dat %>%
  filter(year >= 1930 & year <= 2010) %>% 
    group_by(fips) %>%
    summarise(dday0C = mean(dday0C),
            dday1C = mean(dday1C),
            dday2C = mean(dday2C),
            dday3C = mean(dday3C),
            dday4C = mean(dday4C),
            dday5C = mean(dday5C),
            dday6C = mean(dday6C),
            dday7C = mean(dday7C),
            dday8C = mean(dday8C),
            dday9C = mean(dday9C),
            dday10C = mean(dday10C),
            dday11C = mean(dday11C),
            dday12C = mean(dday12C),
            dday13C = mean(dday13C),
            dday14C = mean(dday14C),
            dday15C = mean(dday15C),
            dday16C = mean(dday16C),
            dday17C= mean(dday17C),
            dday18C = mean(dday18C),
            dday19C = mean(dday19C),
            dday20C = mean(dday20C),
            dday21C = mean(dday21C),
            dday22C = mean(dday22C),
            dday23C = mean(dday23C),
            dday24C = mean(dday24C),
            dday25C = mean(dday25C),
            dday26C = mean(dday26C),
            dday27C = mean(dday27C),
            dday28C = mean(dday28C),
            dday29C = mean(dday29C),
            dday30C = mean(dday30C),
            dday31C = mean(dday31C),
            dday32C = mean(dday32C),
            dday33C = mean(dday33C),
            dday34C = mean(dday34C),
            dday35C = mean(dday35C),
            ndday0C = mean(ndday0C),
            prec = mean(prec),
            tmin = mean(tmin),
            tmax = mean(tmax),
            tavg = mean(tavg))
    
cs.dat <- left_join(cs.dat, cs.dd, by = "fips")
cs.dat$prec_sq <- cs.dat$prec^2
saveRDS(cs.dat, "data/cs_search.RDS")

p.dat <- left_join(p.dat, dd_dat, by = c("fips", "year"))
