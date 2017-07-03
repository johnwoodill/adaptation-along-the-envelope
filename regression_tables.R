names(cs.mod1$coefficients)
names(cs.mod1$coefficients) <- c("Intercept", "Degree Days (8-32C)", "Degree Days (8-32C) Squared", "Square Root Degree Days (34C)",
                                 "Precipitation", "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                 "Population Density Squared", "Percent Clay", "Minimum Permeability", 
                                 "K-factor of Top Soil", "Best Soil Class")
names(p.mod1$coefficients)
names(p.mod1$coefficients) <- c("Degree Days (8-32C)", "Degree Days (8-32C) Squared", "Square Root Degree Days (34C)", 
                                "Trend", "Trend Squared", "Precipitation", "Precipitation Squared")

stargazer(cs.mod1, p.mod1, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit = "fips", omit.stat = c("ser", "f"), 
          title = "Regression Models explaining Crop Revenue (weighted by crop acreage)", 
          dep.var.labels = c("Log(Corn Rev)", "Log(Corn Rev)")
          )

