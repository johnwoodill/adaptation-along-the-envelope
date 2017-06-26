library(ggthemes)

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year >= 1960)
cropdat <- filter(cropdat, abs(long) <= 100)

base <- cropdat %>%
  filter(year == 1960) %>% 
  summarise(dm = )

cdat <- cropdat %>% 
  group_by(year) %>% 
  summarise(tavg = mean(tavg, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE))

cdat$tavg_dmean <- cdat$tavg - mean(cdat$tavg, na.rm = TRUE)
cdat$tmax_dmean <- cdat$tmax - mean(cdat$tmax, na.rm = TRUE)



p1 <- ggplot(cdat, aes(year, tavg_dmean)) + geom_line()  + 
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) + theme_tufte() + 
  geom_hline(aes(yintercept = 0), linetype = "dotted") + geom_smooth(se = FALSE) + 
  ylim(-1.5, 1.5) + ggtitle("Degree (C) Deviation from Mean for U.S. counties east of 100th meridian \n (1960 - 2010)") + xlab(NULL) + ylab(NULL)



ggplot(cdat, aes(year, tmax_dmean)) + geom_line()+ scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) + theme_tufte() + geom_hline(aes(yintercept = 0), linetype = "dotted") + geom_smooth(se = FALSE)+ ylim(-1.5, 1.5)

