cropland <- read_dta("data/DustBowl_All_base1910.dta")
cropland <- select(cropland, year, fips, cropland_fallow, pasture_cropland, woodland_nonpasture, farmland_other, corn_grain_a, cotton_a, hay_a, wheat_a)
head(cropland)

fips <- unique(cropland$fips)
year <- as.numeric(1910:1997)

mdat <- expand.grid(year, fips)
names(mdat) <- c("year", "fips")
head(mdat)
mdat <- left_join(mdat, cropland, by = c("year", "fips"))
mdat <- filter(mdat, fips %in% fipss)
mdat <- mdat[rowSums(is.na(mdat[,c(3,4,5,6)]))!=4, ]

mdatt <- mdat %>%   
  group_by(fips) %>% 
  arrange(year) %>% 
  mutate(corn_grain_a = na.approx(corn_grain_a, na.rm = FALSE),
         cotton_a = na.approx(cotton_a, na.rm = FALSE),
         hay_a = na.approx(hay_a, na.rm = FALSE),
         wheat_a = na.approx(wheat_a, na.rm = FALSE)) %>% 
   ungroup()

head(mdatt)
View(mdatt)


library(mlogit)
mdatt$choice1sum <- rowSums(mdatt[,c("cropland_fallow", "pasture_cropland", "woodland_nonpasture", "farmland_other")], na.rm = TRUE)

mdatt$p_cropland_fallow <- mdatt$cropland_fallow/mdatt$choice1sum
  
ifelse(mdatt$cropland_fallow/mdatt$choice1sum >=0.5)
agdat <- mlogit.data(mdatt, choice = "")