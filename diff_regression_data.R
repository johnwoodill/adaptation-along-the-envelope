library(tidyverse)

# Baseline for predictions
# 10-year interval 1960's and 2000's --------------------------------------
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

cropdat <- readRDS("data/full_ag_data.rds")

# Soil data
#soil <- read_dta("data/soilData.dta")
#soil <- readRDS("data/soilData.rds")
#soil$fips <- as.numeric(soil$fips)

cropdat$prec_sq <- cropdat$prec^2
cropdat$tavg_sq <- cropdat$tavg^2
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

# cropdat$corn_rrev <- ifelse(is.na(cropdat$corn_rrev), 0, cropdat$corn_rrev)
# cropdat$cotton_rrev <- ifelse(is.na(cropdat$cotton_rrev), 0, cropdat$cotton_rrev)
# cropdat$hay_rrev <- ifelse(is.na(cropdat$hay_rrev), 0, cropdat$hay_rrev)
# cropdat$wheat_rrev <- ifelse(is.na(cropdat$wheat_rrev), 0, cropdat$wheat_rrev)
# cropdat$soybean_rrev <- ifelse(is.na(cropdat$soybean_rrev), 0, cropdat$soybean_rrev)

# Log revenue
cropdat$ln_corn_rrev <- log(1 + cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(1 + cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(1 + cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(1 + cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(1 + cropdat$soybean_rrev)

# NA = 0 
# cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
# cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
# cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
# cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
# cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)
 cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)

weightdat <- cropdat %>% 
  select(year, fips, total_a, corn_grain_a, cotton_a, hay_a, wheat_a, soybean_a) %>% 
  filter(year >= 1950 & year < 1960) %>% 
  group_by(fips) %>% 
  summarise(total_w = mean(total_a, na.rm = TRUE),
          corn_w = mean(corn_grain_a, na.rm = TRUE),
          cotton_w = mean(cotton_a, na.rm = TRUE),
          hay_w = mean(hay_a, na.rm = TRUE),
          wheat_w = mean(wheat_a, na.rm = TRUE),
          soybean_w = mean(soybean_a, na.rm = TRUE))

cropdat <- left_join(cropdat, weightdat, by = "fips")



# Crop weights
# cropdat$corn_w <- cropdat$corn_grain_a
# cropdat$cotton_w <- cropdat$cotton_a
# cropdat$hay_w <- cropdat$hay_a
# cropdat$wheat_w <- cropdat$wheat_a
# cropdat$soybean_w <- cropdat$soybean_a



# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

decade_merge <- function(dat, begd, endd, int){
  mergdat <- data.frame()
  decades <- seq(begd, endd, int)
  for (i in decades){
    int.dat <- filter(dat, year >= i & year < (i + int))
    int.dat <- int.dat %>%
      mutate(ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
             ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
             ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
             ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
             ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(state, fips) %>% 
      summarise(ln_corn_rrev = mean(ln_corn_rrev, na.rm = TRUE),
                corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
                ln_cotton_rrev = mean(ln_cotton_rrev, na.rm = TRUE),
                cotton_a = mean(cotton_a, na.rm = TRUE),
                ln_hay_rrev = mean(ln_hay_rrev, na.rm = TRUE),
                hay_a = mean(hay_a, na.rm = TRUE),
                ln_wheat_rrev = mean(ln_wheat_rrev, na.rm = TRUE),
                wheat_a = mean(wheat_a, na.rm = TRUE),
                ln_soybean_rrev = mean(ln_soybean_rrev, na.rm = TRUE),
                soybean_a = mean(soybean_a, na.rm = TRUE),
                tavg = mean(tavg, na.rm = TRUE),
                dday0C = mean(dday0C, na.rm = TRUE),
                dday10C = mean(dday10C, na.rm = TRUE),
                dday30C = mean(dday30C, na.rm = TRUE),
                prec = mean(prec, na.rm = TRUE),
                corn_w = mean(corn_w, na.rm = TRUE),
                cotton_w = mean(cotton_w, na.rm = TRUE),
                hay_w = mean(hay_w, na.rm = TRUE),
                wheat_w = mean(wheat_w, na.rm = TRUE),
                soybean_w = mean(soybean_w, na.rm = TRUE),
                lat = mean(lat, na.rm = TRUE),
                long = mean(long, na.rm = TRUE)) %>% 
      ungroup() #%>% 
      #complete(fips)
    
    int.dat$year <- i
    mergdat <- rbind(mergdat, int.dat)
  }
  return(mergdat)
}

cropdat <- decade_merge(cropdat, 1950, 2000, 10)

cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Total acres
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)



# Get proportion of crop share
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a




# decadedat <- decadedat %>% 
#   group_by(fips) %>% 
#   mutate(total_w = mean(total_w, na.rm = TRUE),
#          corn_w = mean(corn_w, na.rm = TRUE),
#          cotton_w = mean(cotton_w, na.rm = TRUE),
#          hay_w = mean(hay_w, na.rm = TRUE),
#          wheat_w = mean(wheat_w, na.rm = TRUE),
#          soybean_w = mean(soybean_w, na.rm = TRUE))

# weightdat <- cropdat %>% 
#   select(year, fips, total_w, corn_w, cotton_w, hay_w, wheat_w, soybean_w) %>% 
#   filter(year >= 1930 & year <= 1950) %>% 
#   group_by(fips) %>% 
#   summarise(total_w = mean(total_w, na.rm = TRUE),
#           corn_w = mean(corn_w, na.rm = TRUE),
#           cotton_w = mean(cotton_w, na.rm = TRUE),
#           hay_w = mean(hay_w, na.rm = TRUE),
#           wheat_w = mean(wheat_w, na.rm = TRUE),
#           soybean_w = mean(soybean_w, na.rm = TRUE))
# decadedat <- anti_join(decadedat, weightdat, by = "fips")

cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat$tavg_sq <- cropdat$tavg^2
cropdat$prec_sq <- cropdat$prec^2
cropdat$`lat:long` <- cropdat$lat*cropdat$long

# Get proportion of crop share
cropdat$p_corn_share <- ifelse(is.na(cropdat$p_corn_share), 0, cropdat$p_corn_share)
cropdat$p_cotton_share <- ifelse(is.na(cropdat$p_cotton_share), 0, cropdat$p_cotton_share)
cropdat$p_hay_share <- ifelse(is.na(cropdat$p_hay_share), 0, cropdat$p_hay_share)
cropdat$p_wheat_share <- ifelse(is.na(cropdat$p_wheat_share), 0, cropdat$p_wheat_share)
cropdat$p_soybean_share <- ifelse(is.na(cropdat$p_soybean_share), 0, cropdat$p_soybean_share)

cropdat$ln_corn_rrev <- ifelse(is.na(cropdat$ln_corn_rrev), 0, cropdat$ln_corn_rrev)
cropdat$ln_cotton_rrev <- ifelse(is.na(cropdat$ln_cotton_rrev), 0, cropdat$ln_cotton_rrev)
cropdat$ln_hay_rrev <- ifelse(is.na(cropdat$ln_hay_rrev), 0, cropdat$ln_hay_rrev)
cropdat$ln_wheat_rrev <- ifelse(is.na(cropdat$ln_wheat_rrev), 0, cropdat$ln_wheat_rrev)
cropdat$ln_soybean_rrev <- ifelse(is.na(cropdat$ln_soybean_rrev), 0, cropdat$ln_soybean_rrev)

saveRDS(cropdat, "data/diff_regression_data.rds")


