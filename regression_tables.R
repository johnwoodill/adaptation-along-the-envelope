library(stargazer)

rm(list=ls())

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

cs.corn.mod1 <- readRDS("models/cs.corn.mod1")
cs.corn.mod2 <- readRDS("models/cs.corn.mod2")

p.corn.mod1 <- readRDS("models/p.corn.mod1")
p.corn.mod2 <- readRDS("models/p.corn.mod2")

p.corn.mod3 <- readRDS("models/p.corn.mod3")
p.corn.mod4 <- readRDS("models/p.corn.mod4")

cc.corn.mod1 <- readRDS("models/cc.corn.mod1")
cc.corn.mod2 <- readRDS("models/cc.corn.mod2")

diff.corn.mod1 <- readRDS("models/diff.corn.mod1")
diff.corn.mod2 <- readRDS("models/diff.corn.mod2")
diff.corn.mod3 <- readRDS("models/diff.corn.mod3")
diff.corn.mod4 <- readRDS("models/diff.corn.mod4")

# Cross Section Model
names(cs.corn.mod1$coefficients)
names(cs.corn.mod1$coefficients)[32:44] <- c("Avg. Temperature", "Avg. Temperature Squared", "Precipitation",
                                 "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                  "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                  "K-factor of Top Soil", "Best Soil Class")

names(cs.corn.mod2$coefficients)
names(cs.corn.mod2$coefficients)[32:45] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)",
                                 "Precipitation", "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                  "Population Density Squared",  "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                  "K-factor of Top Soil", "Best Soil Class")

# Panel Model
names(p.corn.mod1$coefficients)
names(p.corn.mod1$coefficients)[61:64] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(p.corn.mod2$coefficients)
names(p.corn.mod2$coefficients)[61:65] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")

names(p.corn.mod3$coefficients)
names(p.corn.mod3$coefficients)[61:64] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(p.corn.mod4$coefficients)
names(p.corn.mod4$coefficients)[61:65] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")

# Crop Choice Model
names(cc.corn.mod1$coefficients)
names(cc.corn.mod1$coefficients)[2:14] <- c("Avg. Temperature", "Avg. Temperature Squared", "Precipitation",
                                 "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                  "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                  "K-factor of Top Soil", "Best Soil Class")

names(cc.corn.mod2$coefficients)
names(cc.corn.mod2$coefficients)[2:15] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)",
                                     "Precipitation", "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                  "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                  "K-factor of Top Soil", "Best Soil Class")
# l.corn.mod2$coefficients <- l.corn.mod2$coefficients*100

# Difference Model
names(diff.corn.mod1$coefficients)
names(diff.corn.mod1$coefficients)[2207:2210] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(diff.corn.mod2$coefficients)
names(diff.corn.mod2$coefficients)[2207:2211] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")

names(diff.corn.mod3$coefficients)
names(diff.corn.mod3$coefficients)[2:5] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(diff.corn.mod4$coefficients)
names(diff.corn.mod4$coefficients)[2:6] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")


multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

#Average temp: Cross-Section, probit acreage, panel, difference
star1 <- stargazer(list(cs.corn.mod1, p.corn.mod1, cc.corn.mod1, p.corn.mod3), align = FALSE, no.space = FALSE, style = "aer", digits = 4,
          omit = c("fips", "year","state"), omit.stat = c("ser", "f"), 
          title = "Regression Models explaining Crop Revenue and Acres", 
          column.labels = c("Cross-section", "Panel", "Cross-section", "Panel"),
          dep.var.labels = c("Log(Corn Rev)", "Corn Acres"), 
          model.names = FALSE,
          notes = "All coefficients multiplied by 100",
          notes.align = "l",
          notes.append = FALSE,
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dc#-t-as=n",
          add.lines = list(c("Weights", "Acres", "Acres", "None", "None"), 
                           c("Fixed-effect", "State", "County/Year", "State", "County/Year")))

# Model Table
star2 <- stargazer(cs.corn.mod2, p.corn.mod2, cc.corn.mod2, p.corn.mod4, align = FALSE, no.space = FALSE, style = "aer", digits = 4,
          omit = c("fips", "year", "state"), omit.stat = c("ser", "f"), 
          title = "Regression Models explaining Crop Revenue and Acres", 
          column.labels = c("Cross-section", "Panel", "Cross-section", "Panel"),
          dep.var.labels = c("Log(Corn Rev)", "Corn Acres"), 
          model.names = FALSE,
          apply.coef = multiply.100, apply.se = multiply.100,
          notes = "All coefficients multiplied by 100",
          notes.align = "l",
          notes.append = FALSE,
          table.layout ="=dc#-t-as=n",
          add.lines = list(c("Weights", "Acres", "Acres", "None", "None"),
                           c("Fixed-effect", "State", "County/Year", "State", "County/Year")))

# Model Table
star3 <- stargazer(diff.corn.mod1, diff.corn.mod2, diff.corn.mod3, diff.corn.mod4, 
                   align = FALSE, no.space = FALSE, style = "aer", digits = 4,
           omit = c("fips", "year"), omit.stat = c("ser", "f"), 
           title = "Regression Models explaining Difference in Crop Revenue and Acres", 
           column.labels = c("Diff (1960-2000)", "(1960-2000)", "(1960-2000)", "(1960-2000)"),
           dep.var.labels = c("Log(Corn Rev)", "Log(Corn Rev)", "Corn Acres", "Corn Acres"), 
           model.names = FALSE,
           apply.coef = multiply.100, apply.se = multiply.100,
           notes = "All coefficients multiplied by 100",
           notes.align = "l",
           notes.append = FALSE,
           table.layout ="=dc#-t-as=n",
           add.lines = list(c("Weights", "Acres", "Acres", "None", "None"), 
                            c("Fixed-effect", "County/Year", "County/Year", "County/Year", "County/Year")))
          
{
cat("\\documentclass{article}\n\\usepackage{graphicx}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "regression_tables.tex")
cat(star1, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "regression_tables.tex", append = TRUE)
cat(star2, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "regression_tables.tex", append = TRUE)
cat(star3, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex regression_tables.tex")
}

