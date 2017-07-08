library(stargazer)

rm(list=ls())

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

cs.corn.mod1 <- readRDS("models/cs.corn.mod1")
cs.corn.mod2 <- readRDS("models/cs.corn.mod2")

p.corn.mod1 <- readRDS("models/p.corn.mod1")
p.corn.mod2 <- readRDS("models/p.corn.mod2")

l.corn.mod1 <- readRDS("models/l.corn.mod1")
l.corn.mod2 <- readRDS("models/l.corn.mod2")

# Cross Section Model
names(cs.corn.mod1$coefficients)
names(cs.corn.mod1$coefficients) <- c("Intercept", "Avg. Temperature", "Avg. Temperature Squared", "Precipitation",
                                 "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                 "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                 "K-factor of Top Soil", "Best Soil Class")
# cs.corn.mod1$coefficients <- cs.corn.mod1$coefficients*100
# coef(summary(cs.corn.mod1))[c(1:10), "Std. Error"] 
# 
# coef(summary(cs.corn.mod1))[, "Std. Error"]*100


names(cs.corn.mod2$coefficients)
names(cs.corn.mod2$coefficients) <- c("Intercept", "Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Square Root Degree Days (34C)",
                                 "Precipitation", "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                 "Population Density Squared",  "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                 "K-factor of Top Soil", "Best Soil Class")
# cs.corn.mod2$coefficients <- cs.corn.mod2$coefficients*100

# Panel Model
names(p.corn.mod1$coefficients)
#p.corn.mod1$coefficients <- p.corn.mod1$coefficients[2344:2347]
names(p.corn.mod1$coefficients)[36:39] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")
# p.corn.mod1$coefficients <- p.corn.mod1$coefficients*100
# p.corn.mod1$

#p.corn.mod2$coefficients <- p.corn.mod2$coefficients[2344:2348]
names(p.corn.mod2$coefficients)
names(p.corn.mod2$coefficients)[36:40] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Square Root Degree Days (34C)", 
                                "Precipitation", "Precipitation Squared")
# p.corn.mod2$coefficients <- p.corn.mod2$coefficients*100

# Logit Model
names(l.corn.mod1$coefficients)
names(l.corn.mod1$coefficients) <- c("Intercept", "Avg. Temperature", "Avg. Temperature Squared", "Precipitation",
                                 "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                 "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                 "K-factor of Top Soil", "Best Soil Class")
# l.corn.mod1$coefficients <- l.corn.mod1$coefficients*100

names(l.corn.mod2$coefficients)
names(l.corn.mod2$coefficients) <- c("Intercept", "Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Square Root Degree Days (34C)",
                                     "Precipitation", "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                 "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                 "K-factor of Top Soil", "Best Soil Class")
# l.corn.mod2$coefficients <- l.corn.mod2$coefficients*100

# names(l.corn.mod3$coefficients)
# names(l.corn.mod3$coefficients) <- c("Intercept", "Avg. Temperature", "Avg. Temperature Squared", "Precipitation", 
#                                      "Precipitation Squared")
# 
# names(l.corn.mod4$coefficients)
# names(l.corn.mod4$coefficients) <- c("Intercept", "Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Square Root Degree Days (34C)",
#                                      "Precipitation", "Precipitation Squared")

#Average temp: Cross-Section, probit acreage, panel, difference
star1 <- stargazer(cs.corn.mod1, p.corn.mod1, l.corn.mod1, align = TRUE, no.space = TRUE, digits = 3,  report = "vct*", 
          omit = c("fips", "year"), omit.stat = c("ser", "f"), 
          title = "Regression Models explaining Crop Revenue", 
          column.labels = c("Cross-section", "Panel", "Logit"),
          dep.var.labels = c("Log(Corn Rev)", "Log(Corn Rev)", "Corn Acreage"), model.names = FALSE,
          add.lines = list(c("Fixed Effect", "none", "year, county", "none"))
          )

# Logit Model Table
star2 <- stargazer(cs.corn.mod2, p.corn.mod2, l.corn.mod2, align = TRUE, no.space = TRUE, style = "aer",
          omit = c("fips", "year"), omit.stat = c("ser", "f"), 
          title = "Regression Models explaining Crop Revenue", 
          column.labels = c("Cross section", "Panel", "Logit"),
          dep.var.labels = c("Log(Corn Rev)", "Log(Corn Rev)", "proportion(Corn Acres)"), 
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
# 
# x <- capture.output(stargazer(mtcars[1:5, 1:3], summary = FALSE, title="The main caption of the table."))
# 
# x
# x <- sub("\\caption{The main caption of the table.}", 
#          "\\caption[short caption]{The main caption of the table.}", fixed = TRUE, x)
# 
# x
