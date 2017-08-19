library(cowplot)
library(ggthemes)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year >= 1940 & year <= 2010)
cropdat <- filter(cropdat, abs(long) <= 100)

cdat <- cropdat %>% 
  group_by(year) %>% 
  summarise(tmin = mean(tmin, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE))

cdat$tmin_dm <- cdat$tmin - mean(cdat$tmin, na.rm = TRUE)
cdat$tavg_dm <- cdat$tavg - mean(cdat$tavg, na.rm = TRUE)
cdat$tmax_dm <- cdat$tmax - mean(cdat$tmax, na.rm = TRUE)
cdat$prec_dm <- cdat$prec - mean(cdat$prec, na.rm = TRUE)

p1 <- ggplot(cdat, aes(year, tmin_dm)) + geom_line()  + 
  scale_x_continuous(breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) + theme_tufte() + 
  geom_hline(aes(yintercept = 0), linetype = "dotted") + geom_smooth(fill = "#C0CCD5", color = "orange1") + 
  ylim(-1.5, 1.5) + ggtitle("Degree (C) Deviation from Mean for U.S. counties east of 100th meridian \n (1930 - 2010)") + xlab(NULL) + ylab("Min Temp")
p1

p2 <- ggplot(cdat, aes(year, tavg_dm)) + geom_line()  + 
  scale_x_continuous(breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) + theme_tufte() + 
  geom_hline(aes(yintercept = 0), linetype = "dotted") + geom_smooth(fill = "#C0CCD5", color = "coral1") + 
  ylim(-1.5, 1.5) + xlab(NULL) + ylab("Avg Temp")
p2

p3 <- ggplot(cdat, aes(year, tmax_dm)) + geom_line()  + 
  scale_x_continuous(breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) + theme_tufte() + 
  geom_hline(aes(yintercept = 0), linetype = "dotted") + geom_smooth(fill = "#C0CCD5", color = "red") + 
  ylim(-1.5, 1.5) + xlab(NULL) + ylab("Max Temp")
p3

p4 <- ggplot(cdat, aes(year, prec_dm)) + geom_line()  + 
  scale_x_continuous(breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) + theme_tufte() + 
  geom_hline(aes(yintercept = 0), linetype = "dotted") + geom_smooth(fill = "#C0CCD5", color = "blue") + 
  xlab(NULL) + ylab("Precipitation")
p4

plot_grid(p1, p2, p3, p4, ncol = 1)


