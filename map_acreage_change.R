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

mapdat0 <- filter(mapdat, temp == 0)
mapdat5 <- filter(mapdat, temp == 5)
newmapdat <- data.frame(region = mapdat0$region, 
                        crop = mapdat0$crop,
                        value = mapdat5$acres - mapdat0$acres)
newmapdat

states <- c("alabama", "arkansas", "delaware", "florida", "georgia", "illinois", 
"indiana", "iowa", "kansas", "kentucky", "louisiana", "maryland", 
"michigan", "minnesota", "mississippi", "missouri", "nebraska", 
"new jersey", "north carolina", "north dakota", "ohio", "oklahoma", "south carolina", "south dakota", "tennessee", 
"texas", "virginia", "west virginia", "wisconsin", "montana")



# Corn
corndat <- filter(newmapdat, crop == "corn") %>%  select(region, value)
cornfips <- corndat$region
corn_map <- county_choropleth(corndat,
                 title      = NULL, state_zoom = states)
     
                
corn_map <- corn_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Corn") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
corn_map

# Cotton
cottondat <- filter(newmapdat, crop == "cotton") %>% select(region, value)
cottonfips <- cottondat$region
cotton_map <- county_choropleth(cottondat,
                 title      = NULL, state_zoom = states)
     
                
cotton_map <- cotton_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Cotton") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
cotton_map

# Hay
haydat <- filter(newmapdat, crop == "hay") %>% select(region, value)
hay_map <- county_choropleth(haydat,
                 title      = NULL, state_zoom = states)
     
                
hay_map <- hay_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Hay") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
hay_map


# Wheat
wheatdat <- filter(newmapdat, crop == "wheat") %>% select(region, value)
wheatfips <- wheatdat$region
wheat_map <- county_choropleth(wheatdat,
                 title      = NULL, state_zoom = states)
     
                
wheat_map <- wheat_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Wheat") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
wheat_map


# Soybean
soybeandat <- filter(newmapdat, crop == "soybean") %>% select(region, value)
soybean_map <- county_choropleth(soybeandat,
                 title      = NULL, state_zoom = states)
     
                
soybean_map <- soybean_map + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Soybean") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
soybean_map

plot_grid(corn_map, cotton_map, hay_map, wheat_map, soybean_map)

