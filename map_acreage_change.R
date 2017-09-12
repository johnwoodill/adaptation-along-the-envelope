library(tidyverse)
library(choroplethr)

library(choroplethr)
library(ggthemes)
library(cowplot)



# Load Predicted acreage
cs.pred_acres <- readRDS("data/cs.predicted_acres.rds")
diff.pred_acres <- readRDS("data/diff.predicted_acres.rds")

cropdat <- readRDS("data/cross_section_regression_data.rds")

mapdat <- data.frame(region = rep(cropdat$fips, 30),
                    temp = cs.pred_acres$temp, 
                     crop = cs.pred_acres$crop,
                     acres = cs.pred_acres$acres
                     )

states <- c("alabama", "arkansas", "delaware", "florida", "georgia", "illinois", 
"indiana", "iowa", "kansas", "kentucky", "louisiana", "maryland", 
"michigan", "minnesota", "mississippi", "missouri", "nebraska", 
"new jersey", "north carolina", "north dakota", "ohio", "oklahoma", 
"pennsylvania", "south carolina", "south dakota", "tennessee", 
"texas", "virginia", "west virginia", "wisconsin")

# Corn
corndat <- filter(mapdat, temp == 0 & crop == "corn") %>%  rename(region = region, value = acres) %>% select(region, value)
cornfips <- corndat$region
corn_map <- county_choropleth(corndat,
                 title      = NULL, state_zoom = states)
     
                
corn_map <- corn_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Corn +0C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
corn_map

# Cotton
cottondat <- filter(mapdat, temp == 0 & crop == "cotton") %>%  rename(region = region, value = acres) %>% select(region, value)
cottonfips <- cottondat$region
cotton_map <- county_choropleth(cottondat,
                 title      = NULL, state_zoom = states)
     
                
cotton_map <- cotton_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Cotton +0C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
cotton_map

# Hay
haydat <- filter(mapdat, temp == 0 & crop == "hay") %>%  rename(region = region, value = acres) %>% select(region, value)
hayfips <- haydat$region
hay_map <- county_choropleth(haydat,
                 title      = NULL, state_zoom = states)
     
                
hay_map <- hay_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Hay +0C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
hay_map


# Wheat
wheatdat <- filter(mapdat, temp == 0 & crop == "wheat") %>%  rename(region = region, value = acres) %>% select(region, value)
wheatfips <- wheatdat$region
wheat_map <- county_choropleth(wheatdat,
                 title      = NULL, state_zoom = states)
     
                
wheat_map <- wheat_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Wheat +0C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
wheat_map


# Soybean
soybeandat <- filter(mapdat, temp == 0 & crop == "soybean") %>%  rename(region = region, value = acres) %>% select(region, value)
soybeanfips <- soybeandat$region
soybean_map <- county_choropleth(soybeandat,
                 title      = NULL, state_zoom = states)
     
                
soybean_map <- soybean_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Soybean +0C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
soybean_map


# +5C
# Corn
corndat5 <- filter(mapdat, temp == 5 & crop == "corn") %>%  rename(region = region, value = acres) %>% select(region, value)
cornfips5 <- corndat5$region
corn_map5 <- county_choropleth(corndat5,
                 title      = NULL, state_zoom = states)
     
                
corn_map5 <- corn_map5 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Corn +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
corn_map5

# Cotton
cottondat5 <- filter(mapdat, temp == 5 & crop == "cotton") %>%  rename(region = region, value = acres) %>% select(region, value)
cottonfips5 <- cottondat5$region
cotton_map5 <- county_choropleth(cottondat5,
                 title      = NULL, state_zoom = states)
     
                
cotton_map5 <- cotton_map5 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Cotton +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
cotton_map5

# Hay
haydat5 <- filter(mapdat, temp == 5 & crop == "hay") %>%  rename(region = region, value = acres) %>% select(region, value)
hayfips5 <- haydat5$region
hay_map5 <- county_choropleth(haydat5,
                 title      = NULL, state_zoom = states)
     
                
hay_map5 <- hay_map5 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Hay +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
hay_map5

# Wheat
wheatdat5 <- filter(mapdat, temp == 5 & crop == "wheat") %>%  rename(region = region, value = acres) %>% select(region, value)
wheatfips5 <- wheatdat5$region
wheat_map5 <- county_choropleth(wheatdat5,
                 title      = NULL, state_zoom = states)
     
                
wheat_map5 <- wheat_map5 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Wheat +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
wheat_map5


# Soybean
soybeandat5 <- filter(mapdat, temp == 5 & crop == "soybean") %>%  rename(region = region, value = acres) %>% select(region, value)
soybeanfips5 <- soybeandat5$region
soybean_map5 <- county_choropleth(soybeandat5,
                 title      = NULL, state_zoom = states)
     
                
soybean_map5 <- soybean_map5 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Soybean +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
soybean_map5

plot_grid(corn_map, corn_map5, ncol = 2) 
plot_grid(cotton_map, cotton_map5, ncol = 2) 
plot_grid(hay_map, hay_map5, ncol = 2) 
plot_grid(wheat_map, wheat_map5, ncol = 2) 
plot_grid(soybean_map, soybean_map5, ncol = 2) 
          
cotton_map, cotton_map5, 
          hay_map, hay_map5, 
          wheat_map, wheat_map5, 
          soybean_map, soybean_map5, ncol = 2)
plot_grid(a, b, c, d, e, ncol = 1, )

# 
# 
# # Cross-section Wheat change
# wheatfips <- wheat.c$region
# 
# wheat_cmap <- county_choropleth(wheat.c,
#                  title      = NULL, state_zoom = states)
#      
#                 
# wheat_cmap <- wheat_cmap + scale_fill_brewer(palette = "YlGnBu") + 
#   theme_tufte(base_size = 14)+ 
#   xlab("Wheat +5C") + ylab(NULL) + theme(legend.position = "none",
#                        axis.text.x = element_blank(),
#                        axis.text.y = element_blank(),
#                        axis.ticks.x = element_blank(),
#                        axis.ticks.y = element_blank(),
#                        panel.border = element_rect(fill = NA)) 
#   
# wheat_cmap
# 
# plot_grid(wheat_map, wheat_cmap)
# 
# 
# # Cotton
# cotton.pred$pred.acres$cs.acres
# cotton.c <- filter(cotton.pred$pred.acres$cs.acres, temp == 5)
# 
# cotton.c$fips <- cs.0C$fips
# cotton.c <- select(cotton.c, fips, acres)
# names(cotton.c) <- c("region", "value")
# 
# # Cross-section cotton change
# cottonfips <- cotton.c$region
# 
# cotton_cmap <- county_choropleth(cotton.c,
#                  title      = NULL, state_zoom = states)
#      
#                 
# cotton_cmap <- cotton_cmap + scale_fill_brewer(palette = "YlGnBu") + 
#   theme_tufte(base_size = 14)+ 
#   xlab("cotton +5C") + ylab(NULL) + theme(legend.position = "none",
#                        axis.text.x = element_blank(),
#                        axis.text.y = element_blank(),
#                        axis.ticks.x = element_blank(),
#                        axis.ticks.y = element_blank(),
#                        panel.border = element_rect(fill = NA)) 
#   
# cotton_cmap
# 
# plot_grid(cotton_map, cotton_cmap)
# 
# 
# # Corn
# corn.pred$pred.acres$cs.acres
# corn.c <- filter(corn.pred$pred.acres$cs.acres, temp == 5)
# 
# corn.c$fips <- cs.0C$fips
# corn.c <- select(corn.c, fips, acres)
# names(corn.c) <- c("region", "value")
# 
# # Cross-section corn change
# cornfips <- corn.c$region
# 
# corn_cmap <- county_choropleth(corn.c,
#                  title      = NULL, state_zoom = states)
#      
#                 
# corn_cmap <- corn_cmap + scale_fill_brewer(palette = "YlGnBu") + 
#   theme_tufte(base_size = 14)+ 
#   xlab("Corn +5C") + ylab(NULL) + theme(legend.position = "none",
#                        axis.text.x = element_blank(),
#                        axis.text.y = element_blank(),
#                        axis.ticks.x = element_blank(),
#                        axis.ticks.y = element_blank(),
#                        panel.border = element_rect(fill = NA)) 
#   
# corn_cmap
# 
# plot_grid(corn_map, corn_cmap)
