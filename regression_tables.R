library(stargazer)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

source("cross_section_regression.R")
source("panel_regression.R")
source("logit_regression.R")

# Cross Section Model
names(cs.corn.mod1$coefficients)
names(cs.corn.mod1$coefficients) <- c("Intercept", "Avg. Temperature", "Avg. Temperature Squared", "Precipitation",
                                 "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                 "Population Density Squared", "Percent Clay", "Minimum Permeability", 
                                 "K-factor of Top Soil", "Best Soil Class")


names(cs.corn.mod2$coefficients)
names(cs.corn.mod2$coefficients) <- c("Intercept", "Degree Days (8-32C)", "Degree Days (8-32C) Squared", "Square Root Degree Days (34C)",
                                 "Precipitation", "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                 "Population Density Squared", "Percent Clay", "Minimum Permeability", 
                                 "K-factor of Top Soil", "Best Soil Class")

# Panel Model
names(p.corn.mod1$coefficients)
names(p.corn.mod1$coefficients) <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared", "Trend", "Trend Squared")

names(p.corn.mod2$coefficients)
names(p.corn.mod2$coefficients) <- c("Degree Days (8-32C)", "Degree Days (8-32C) Squared", "Square Root Degree Days (34C)", 
                                "Precipitation", "Precipitation Squared", "Trend", "Trend Squared")

# Logit Model
names(l.corn.mod1$coefficients)
names(l.corn.mod1$coefficients) <- c("Intercept", "Avg. Temperature", "Avg. Temperature Squared", "Precipitation", 
                                     "Precipitation Squared")

names(l.corn.mod2$coefficients)
names(l.corn.mod2$coefficients) <- c("Intercept", "Degree Days (8-32C)", "Degree Days (8-32C) Squared", "Square Root Degree Days (34C)",
                                     "Precipitation", "Precipitation Squared")

names(l.corn.mod3$coefficients)
names(l.corn.mod3$coefficients) <- c("Intercept", "Avg. Temperature", "Avg. Temperature Squared", "Precipitation", 
                                     "Precipitation Squared")

names(l.corn.mod4$coefficients)
names(l.corn.mod4$coefficients) <- c("Intercept", "Degree Days (8-32C)", "Degree Days (8-32C) Squared", "Square Root Degree Days (34C)",
                                     "Precipitation", "Precipitation Squared")

# Long difference Model

#Cross-Section and Panel Table
star1 <- stargazer(cs.corn.mod1, cs.corn.mod2, p.corn.mod1, p.corn.mod2, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit = "fips", omit.stat = c("ser", "f"), 
          title = "Regression Models explaining Crop Revenue", 
          column.labels = c("Cross-section", "Cross-section", "Panel", "Panel"),
          dep.var.labels = c("Log(Corn Rev)", "Log(Corn Rev)"), label = "cross.section.and.panel"
          , model.names = FALSE
          )

# Logit Model Table
star2 <- stargazer(l.corn.mod1, l.corn.mod2, l.corn.mod3, l.corn.mod4, align = TRUE, no.space = TRUE, digits = 2,  report = "vc*", 
          omit = "fips", omit.stat = c("ser", "f"), 
          title = "Regression Models explaining Crop Revenue", 
          column.labels = c("Logit", "Logit", "Diff (2000 - 1960)", "Diff (2000 - 1960)"),
          dep.var.labels = c("p(Corn Rev)", "p(Corn Rev)"), 
          model.names = FALSE
          )
{
cat("\\documentclass{article}\n\\usepackage{graphicx}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "regression_tables.tex")
cat(star1, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "regression_tables.tex", append = TRUE)
cat(star2, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex regression_tables.tex")
}
