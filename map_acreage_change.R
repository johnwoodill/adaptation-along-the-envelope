

# map changes

wheat.pred$pred.acres$cs.acres
wheat.c <- filter(wheat.pred$pred.acres$cs.acres, temp == 5)

wheat.c$fips <- cs.0C$fips
wheat.c <- select(wheat.c, fips, acres)
names(wheat.c) <- c("region", "value")

# Cross-section Wheat change
wheatfips <- wheat.c$region

wheat_cmap <- county_choropleth(wheat.c,
                 title      = NULL, state_zoom = states)
     
                
wheat_cmap <- wheat_cmap + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Wheat +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
wheat_cmap

plot_grid(wheat_map, wheat_cmap)


# Cotton
cotton.pred$pred.acres$cs.acres
cotton.c <- filter(cotton.pred$pred.acres$cs.acres, temp == 5)

cotton.c$fips <- cs.0C$fips
cotton.c <- select(cotton.c, fips, acres)
names(cotton.c) <- c("region", "value")

# Cross-section cotton change
cottonfips <- cotton.c$region

cotton_cmap <- county_choropleth(cotton.c,
                 title      = NULL, state_zoom = states)
     
                
cotton_cmap <- cotton_cmap + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("cotton +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
cotton_cmap

plot_grid(cotton_map, cotton_cmap)


# Corn
corn.pred$pred.acres$cs.acres
corn.c <- filter(corn.pred$pred.acres$cs.acres, temp == 5)

corn.c$fips <- cs.0C$fips
corn.c <- select(corn.c, fips, acres)
names(corn.c) <- c("region", "value")

# Cross-section corn change
cornfips <- corn.c$region

corn_cmap <- county_choropleth(corn.c,
                 title      = NULL, state_zoom = states)
     
                
corn_cmap <- corn_cmap + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 14)+ 
  xlab("Corn +5C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
corn_cmap

plot_grid(corn_map, corn_cmap)
