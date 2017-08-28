library(tidyverse)

# Baseline for predictions
# 10-year interval 1960's and 2000's --------------------------------------
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

# Crop weights
cropdat$corn_w <- cropdat$corn_grain_a
cropdat$cotton_w <- cropdat$cotton_a
cropdat$hay_w <- cropdat$hay_a
cropdat$wheat_w <- cropdat$wheat_a
cropdat$soybean_w <- cropdat$soybean_a

# NA = 0 for tobit
cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Get proportion of crop share
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a


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
             ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
             p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
             p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
             p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
             p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
             p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(state, fips) %>% 
      summarise(p_corn_share = mean(p_corn_share, na.rm = TRUE),
                ln_corn_rrev = mean(ln_corn_rrev, na.rm = TRUE),
                corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
                p_cotton_share = mean(p_cotton_share, na.rm = TRUE),
                ln_cotton_rrev = mean(ln_cotton_rrev, na.rm = TRUE),
                cotton_a = mean(cotton_a, na.rm = TRUE),
                p_hay_share = mean(p_hay_share, na.rm = TRUE),
                ln_hay_rrev = mean(ln_hay_rrev, na.rm = TRUE),
                hay_a = mean(hay_a, na.rm = TRUE),
                p_wheat_share = mean(p_wheat_share, na.rm = TRUE),
                ln_wheat_rrev = mean(ln_wheat_rrev, na.rm = TRUE),
                wheat_a = mean(wheat_a, na.rm = TRUE),
                p_soybean_share = mean(p_soybean_share, na.rm = TRUE),
                ln_soybean_rrev = mean(ln_soybean_rrev, na.rm = TRUE),
                soybean_a = mean(soybean_a, na.rm = TRUE),
                tavg = mean(tavg, na.rm = TRUE),
                dday0C = mean(dday0C, na.rm = TRUE),
                dday10C = mean(dday10C, na.rm = TRUE),
                dday15C = mean(dday15C, na.rm = TRUE),
                dday17C = mean(dday17C, na.rm = TRUE),
                dday29C = mean(dday29C, na.rm = TRUE),
                dday30C = mean(dday30C, na.rm = TRUE),
                dday32C = mean(dday32C, na.rm = TRUE),
                dday34C = mean(dday34C, na.rm = TRUE),
                ndday0C = mean(ndday0C, na.rm = TRUE),
                prec = mean(prec, na.rm = TRUE),
                total_w = mean(total_a, na.rm = TRUE),
                corn_w = mean(corn_w, na.rm = TRUE),
                cotton_w = mean(cotton_w, na.rm = TRUE),
                hay_w = mean(hay_w, na.rm = TRUE),
                wheat_w = mean(wheat_w, na.rm = TRUE),
                soybean_w = mean(soybean_w, na.rm = TRUE),
                lat = mean(lat, na.rm = TRUE),
                long = mean(long, na.rm = TRUE)) %>% 
      ungroup()
    int.dat$year <- i
    mergdat <- rbind(mergdat, int.dat)
  }
  return(mergdat)
}


decadedat <- decade_merge(cropdat, 1930, 2000, 10)

decadedat <- decadedat %>% 
  group_by(fips) %>% 
  mutate(total_w = mean(total_w, na.rm = TRUE),
         corn_w = mean(corn_w, na.rm = TRUE),
         cotton_w = mean(cotton_w, na.rm = TRUE),
         hay_w = mean(hay_w, na.rm = TRUE),
         wheat_w = mean(wheat_w, na.rm = TRUE),
         soybean_w = mean(soybean_w, na.rm = TRUE))


# Set weights to 1930-1950 acres
# wt <- filter(decadedat, year == 1930) %>% 
#     dplyr::select(fips, total_w, corn_w, cotton_w, hay_w, wheat_w, soybean_w)
# decadedat <- dplyr::select(decadedat, -total_w, -corn_w, -cotton_w, -hay_w, -wheat_w, -soybean_w)
# decadedat <- left_join(decadedat, wt, by = "fips")
# decadedat <- filter(decadedat, fips %in% unique(wt$fips))


decadedat$dday0_10 <- decadedat$dday0C - decadedat$dday10C
decadedat$dday10_30 <- decadedat$dday10C - decadedat$dday30C

# Exposure weighted values equal zero
# decadedat$tavg <- decadedat$tavg - mean(decadedat$tavg, na.rm = TRUE)
# decadedat$dday0_10 <- decadedat$dday0_10 - mean(decadedat$dday0_10, na.rm = TRUE)
# decadedat$dday10_30 <- decadedat$dday10_30 - mean(decadedat$dday10_30, na.rm = TRUE)
# decadedat$dday30C <- decadedat$dday30C - mean(decadedat$dday30C, na.rm = TRUE)
# decadedat$prec <- decadedat$prec - mean(decadedat$prec, na.rm = TRUE)
decadedat$tavg_sq <- decadedat$tavg^2
decadedat$prec_sq <- decadedat$prec^2
cropdat$`lat:long` <- cropdat$lat*cropdat$long
cropdat$`(Intercept)` <- 1

saveRDS(decadedat, "data/baseline_diff_regression_data.rds")




# Center variables
# 10-year interval 1960's and 2000's --------------------------------------
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

# Crop weights
cropdat$corn_w <- cropdat$corn_grain_a
cropdat$cotton_w <- cropdat$cotton_a
cropdat$hay_w <- cropdat$hay_a
cropdat$wheat_w <- cropdat$wheat_a
cropdat$soybean_w <- cropdat$soybean_a

# NA = 0 for tobit
cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Get proportion of crop share
cropdat$total_a <- rowSums(cropdat[,c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a


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
         ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
         p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
         p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
         p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
         p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
         p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE)) %>% 
      ungroup() %>% 
   group_by(state, fips) %>% 
   summarise(p_corn_share = mean(p_corn_share, na.rm = TRUE),
            ln_corn_rrev = mean(ln_corn_rrev, na.rm = TRUE),
            corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            p_cotton_share = mean(p_cotton_share, na.rm = TRUE),
            ln_cotton_rrev = mean(ln_cotton_rrev, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            p_hay_share = mean(p_hay_share, na.rm = TRUE),
            ln_hay_rrev = mean(ln_hay_rrev, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            p_wheat_share = mean(p_wheat_share, na.rm = TRUE),
            ln_wheat_rrev = mean(ln_wheat_rrev, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            p_soybean_share = mean(p_soybean_share, na.rm = TRUE),
            ln_soybean_rrev = mean(ln_soybean_rrev, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            dday0C = mean(dday0C, na.rm = TRUE),
            dday10C = mean(dday10C, na.rm = TRUE),
            dday15C = mean(dday15C, na.rm = TRUE),
            dday17C = mean(dday17C, na.rm = TRUE),
            dday29C = mean(dday29C, na.rm = TRUE),
            dday30C = mean(dday30C, na.rm = TRUE),
            dday32C = mean(dday32C, na.rm = TRUE),
            dday34C = mean(dday34C, na.rm = TRUE),
            ndday0C = mean(ndday0C, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE),
            total_w = mean(total_a, na.rm = TRUE),
            corn_w = mean(corn_w, na.rm = TRUE),
            cotton_w = mean(cotton_w, na.rm = TRUE),
            hay_w = mean(hay_w, na.rm = TRUE),
            wheat_w = mean(wheat_w, na.rm = TRUE),
            soybean_w = mean(soybean_w, na.rm = TRUE),
            lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE)) %>% 
      ungroup()
    int.dat$year <- i
    mergdat <- rbind(mergdat, int.dat)
  }
  return(mergdat)
}


decadedat <- decade_merge(cropdat, 1930, 2000, 10)

decadedat <- decadedat %>% 
  group_by(fips) %>% 
  mutate(total_w = mean(total_w, na.rm = TRUE),
            corn_w = mean(corn_w, na.rm = TRUE),
            cotton_w = mean(cotton_w, na.rm = TRUE),
            hay_w = mean(hay_w, na.rm = TRUE),
            wheat_w = mean(wheat_w, na.rm = TRUE),
            soybean_w = mean(soybean_w, na.rm = TRUE))


# Set weights to 1930-1950 acres
# wt <- filter(decadedat, year == 1930) %>% 
#     dplyr::select(fips, total_w, corn_w, cotton_w, hay_w, wheat_w, soybean_w)
# decadedat <- dplyr::select(decadedat, -total_w, -corn_w, -cotton_w, -hay_w, -wheat_w, -soybean_w)
# decadedat <- left_join(decadedat, wt, by = "fips")
# decadedat <- filter(decadedat, fips %in% unique(wt$fips))


decadedat$dday0_10 <- decadedat$dday0C - decadedat$dday10C
decadedat$dday10_30 <- decadedat$dday10C - decadedat$dday30C

# Exposure weighted values equal zero
decadedat$tavg <- decadedat$tavg - mean(decadedat$tavg, na.rm = TRUE)
decadedat$dday0_10 <- decadedat$dday0_10 - mean(decadedat$dday0_10, na.rm = TRUE)
decadedat$dday10_30 <- decadedat$dday10_30 - mean(decadedat$dday10_30, na.rm = TRUE)
decadedat$dday30C <- decadedat$dday30C - mean(decadedat$dday30C, na.rm = TRUE)
decadedat$prec <- decadedat$prec - mean(decadedat$prec, na.rm = TRUE)
decadedat$tavg_sq <- decadedat$tavg^2
decadedat$prec_sq <- decadedat$prec^2

saveRDS(decadedat, "data/diff_regression_data.rds")


# Tobit data
# 10-year interval 1960's and 2000's --------------------------------------
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, abs(long) <= 100)

cropdat$ln_corn_rrev <- log(cropdat$corn_rrev)
cropdat$ln_cotton_rrev <- log(cropdat$cotton_rrev)
cropdat$ln_hay_rrev <- log(cropdat$hay_rrev)
cropdat$ln_wheat_rrev <- log(cropdat$wheat_rrev)
cropdat$ln_soybean_rrev <- log(cropdat$soybean_rrev)

# Crop weights
cropdat$corn_w <- cropdat$corn_grain_a
cropdat$cotton_w <- cropdat$cotton_a
cropdat$hay_w <- cropdat$hay_a
cropdat$wheat_w <- cropdat$wheat_a
cropdat$soybean_w <- cropdat$soybean_a

# Get proportion of crop share
cropdat$total_a <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)
cropdat$p_corn_share <- cropdat$corn_grain_a/cropdat$total_a
cropdat$p_cotton_share <- cropdat$cotton_a/cropdat$total_a
cropdat$p_hay_share <- cropdat$hay_a/cropdat$total_a
cropdat$p_wheat_share <- cropdat$wheat_a/cropdat$total_a
cropdat$p_soybean_share <- cropdat$soybean_a/cropdat$total_a

# NA = 0 for tobit
cropdat$p_corn_share <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$p_corn_share)
cropdat$p_cotton_share <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$p_cotton_share)
cropdat$p_hay_share <- ifelse(is.na(cropdat$hay_a), 0, cropdat$p_hay_share)
cropdat$p_wheat_share <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$p_wheat_share)
cropdat$p_soybean_share <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$p_soybean_share)

# Set weights
cropdat$total_w <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "wheat_a", "soybean_a")], na.rm = TRUE)


decade_merge <- function(dat, begd, endd, int){
  mergdat <- data.frame()
  decades <- seq(begd, endd, int)
  for (i in decades){
    int.dat <- filter(dat, year >= i & year < (i + int))
    int.dat <- int.dat %>%
      # mutate(ln_corn_rrev = ln_corn_rrev - mean(ln_corn_rrev, na.rm = TRUE),
      #        ln_cotton_rrev = ln_cotton_rrev - mean(ln_cotton_rrev, na.rm = TRUE),
      #        ln_hay_rrev = ln_hay_rrev - mean(ln_hay_rrev, na.rm = TRUE),
      #        ln_wheat_rrev = ln_wheat_rrev - mean(ln_wheat_rrev, na.rm = TRUE),
      #        ln_soybean_rrev = ln_soybean_rrev - mean(ln_soybean_rrev, na.rm = TRUE),
      #        p_corn_share = p_corn_share - mean(p_corn_share, na.rm = TRUE),
      #        p_cotton_share = p_cotton_share - mean(p_cotton_share, na.rm = TRUE),
      #        p_hay_share = p_hay_share - mean(p_hay_share, na.rm = TRUE),
      #        p_wheat_share = p_wheat_share - mean(p_wheat_share, na.rm = TRUE),
      #        p_soybean_share = p_soybean_share - mean(p_soybean_share, na.rm = TRUE)) %>% 
      # ungroup() %>% 
      group_by(state, fips) %>% 
      summarise(p_corn_share = mean(p_corn_share, na.rm = TRUE),
                ln_corn_rrev = mean(ln_corn_rrev, na.rm = TRUE),
                corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
                p_cotton_share = mean(p_cotton_share, na.rm = TRUE),
                ln_cotton_rrev = mean(ln_cotton_rrev, na.rm = TRUE),
                cotton_a = mean(cotton_a, na.rm = TRUE),
                p_hay_share = mean(p_hay_share, na.rm = TRUE),
                ln_hay_rrev = mean(ln_hay_rrev, na.rm = TRUE),
                hay_a = mean(hay_a, na.rm = TRUE),
                p_wheat_share = mean(p_wheat_share, na.rm = TRUE),
                ln_wheat_rrev = mean(ln_wheat_rrev, na.rm = TRUE),
                wheat_a = mean(wheat_a, na.rm = TRUE),
                p_soybean_share = mean(p_soybean_share, na.rm = TRUE),
                ln_soybean_rrev = mean(ln_soybean_rrev, na.rm = TRUE),
                soybean_a = mean(soybean_a, na.rm = TRUE),
                tavg = mean(tavg, na.rm = TRUE),
                dday0C = mean(dday0C, na.rm = TRUE),
                dday10C = mean(dday10C, na.rm = TRUE),
                dday15C = mean(dday15C, na.rm = TRUE),
                dday17C = mean(dday17C, na.rm = TRUE),
                dday29C = mean(dday29C, na.rm = TRUE),
                dday30C = mean(dday30C, na.rm = TRUE),
                dday32C = mean(dday32C, na.rm = TRUE),
                dday34C = mean(dday34C, na.rm = TRUE),
                ndday0C = mean(ndday0C, na.rm = TRUE),
                prec = mean(prec, na.rm = TRUE),
                corn_grian_a = mean(corn_grain_a, na.rm = TRUE),
                cotton_a = mean(cotton_a, na.rm = TRUE),
                hay_a = mean(hay_a, na.rm = TRUE),
                wheat_a = mean(wheat_a, na.rm = TRUE),
                soybean_a = mean(soybean_a, na.rm = TRUE),
                total_a = mean(total_a, na.rm = TRUE),
                total_w = mean(total_a, na.rm = TRUE),
                corn_w = mean(corn_w, na.rm = TRUE),
                cotton_w = mean(cotton_w, na.rm = TRUE),
                hay_w = mean(hay_w, na.rm = TRUE),
                wheat_w = mean(wheat_w, na.rm = TRUE),
                soybean_w = mean(soybean_w, na.rm = TRUE),
                lat = mean(lat, na.rm = TRUE),
                long = mean(long, na.rm = TRUE)) %>% 
      ungroup()
    int.dat$year <- i
    mergdat <- rbind(mergdat, int.dat)
  }
  return(mergdat)
}


decadedat <- decade_merge(cropdat, 1930, 2000, 10)

decadedat <- decadedat %>% 
  group_by(fips) %>% 
  mutate(total_w = mean(total_w, na.rm = TRUE),
         corn_w = mean(corn_w, na.rm = TRUE),
         cotton_w = mean(cotton_w, na.rm = TRUE),
         hay_w = mean(hay_w, na.rm = TRUE),
         wheat_w = mean(wheat_w, na.rm = TRUE),
         soybean_w = mean(soybean_w, na.rm = TRUE))


# Set weights to 1930-1950 acres
# wt <- filter(decadedat, year == 1930) %>% 
#     dplyr::select(fips, total_w, corn_w, cotton_w, hay_w, wheat_w, soybean_w)
# decadedat <- dplyr::select(decadedat, -total_w, -corn_w, -cotton_w, -hay_w, -wheat_w, -soybean_w)
# decadedat <- left_join(decadedat, wt, by = "fips")
# decadedat <- filter(decadedat, fips %in% unique(wt$fips))


decadedat$dday0_10 <- decadedat$dday0C - decadedat$dday10C
decadedat$dday10_30 <- decadedat$dday10C - decadedat$dday30C

# Exposure weighted values equal zero
# decadedat$tavg <- decadedat$tavg - mean(decadedat$tavg, na.rm = TRUE)
# decadedat$dday0_10 <- decadedat$dday0_10 - mean(decadedat$dday0_10, na.rm = TRUE)
# decadedat$dday10_30 <- decadedat$dday10_30 - mean(decadedat$dday10_30, na.rm = TRUE)
# decadedat$dday30C <- decadedat$dday30C - mean(decadedat$dday30C, na.rm = TRUE)
# decadedat$prec <- decadedat$prec - mean(decadedat$prec, na.rm = TRUE)
decadedat$tavg_sq <- decadedat$tavg^2
decadedat$prec_sq <- decadedat$prec^2
cropdat$`lat:long` <- cropdat$lat*cropdat$long
cropdat$`(Intercept)` <- 1

saveRDS(decadedat, "data/tobit_diff_regression_data.rds")



