library(stargazer)

rm(list=ls())

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

cs.temp.ln_corn_rrev <- readRDS("models/cs.temp.ln_corn_rrev")
cs.dd.ln_corn_rrev <- readRDS("models/cs.dd.ln_corn_rrev")

p.temp.ln_corn_rrev <- readRDS("models/p.temp.ln_corn_rrev")
p.dd.ln_corn_rrev <- readRDS("models/p.dd.ln_corn_rrev")

p.temp.p_corn_share <- readRDS("models/p.temp.p_corn_share")
p.dd.p_corn_share <- readRDS("models/p.dd.p_corn_share")

cs.temp.p_corn_share <- readRDS("models/cs.temp.p_corn_share")
cs.dd.p_corn_share <- readRDS("models/cs.dd.p_corn_share")

diff.temp.ln_corn_rrev <- readRDS("models/diff.temp.ln_corn_rrev")
diff.dd.ln_corn_rrev <- readRDS("models/diff.dd.ln_corn_rrev")
diff.temp.p_corn_share <- readRDS("models/diff.temp.p_corn_share")
diff.dd.p_corn_share <- readRDS("models/diff.dd.p_corn_share")

# Calculate robust standard errors to place in tables

# Cross Section Model
attr(p.temp.ln_corn_rrev$coefficients, "dimnames")[[1]] <- 
  c("Average Temperature", "Average Temperature Squared", "Precipitation", "Precipitation Squared")

attr(p.dd.ln_corn_rrev$coefficients, "dimnames")[[1]] <- 
  c("Degree Days (10-30C)", "Degree Days (30C)", "Precipitation", "Precipitation Squared")



# Panel Model
names(p.temp.ln_corn_rrev$coefficients)
names(p.temp.ln_corn_rrev$coefficients)[61:64] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(p.dd.ln_corn_rrev$coefficients)
names(p.dd.ln_corn_rrev$coefficients)[61:65] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")

names(p.temp.p_corn_share$coefficients)
names(p.temp.p_corn_share$coefficients)[61:64] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(p.dd.p_corn_share$coefficients)
names(p.dd.p_corn_share$coefficients)[61:65] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")

# Crop Choice Model
names(cs.temp.p_corn_share$coefficients)
names(cs.temp.p_corn_share$coefficients)[2:14] <- c("Avg. Temperature", "Avg. Temperature Squared", "Precipitation",
                                 "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                  "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                  "K-factor of Top Soil", "Best Soil Class")

names(cs.dd.p_corn_share$coefficients)
names(cs.dd.p_corn_share$coefficients)[2:15] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)",
                                     "Precipitation", "Precipitation Squared", "Latitude", "Income per Capita", "Population Density",
                                  "Population Density Squared", "Water Capacity", "Percent Clay", "Minimum Permeability", 
                                  "K-factor of Top Soil", "Best Soil Class")
# l.corn.mod2$coefficients <- l.corn.mod2$coefficients*100

# Difference Model
names(diff.temp.ln_corn_rrev$coefficients)
names(diff.temp.ln_corn_rrev$coefficients)[2207:2210] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(diff.dd.ln_corn_rrev$coefficients)
names(diff.dd.ln_corn_rrev$coefficients)[2207:2211] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")

names(diff.temp.p_corn_share$coefficients)
names(diff.temp.p_corn_share$coefficients)[2:5] <- c("Avg. Temperature", "Avg. Temperature Squared", 
                                "Precipitation", "Precipitation Squared")

names(diff.dd.p_corn_share$coefficients)
names(diff.dd.p_corn_share$coefficients)[2:6] <- c("Degree Days (10-30C)", "Degree Days (10-30C) Squared", "Degree Days (30C)", 
                                "Precipitation", "Precipitation Squared")


multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

#Average temp: Cross-Section, probit acreage, panel, difference
star1 <- stargazer(cs.temp.ln_corn_rrev, p.temp.ln_corn_rrev, cs.temp.p_corn_share, p.temp.p_corn_share, align = FALSE, no.space = FALSE, 
                   style = "aer", digits = 2,
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
          font.size = "footnotesize",
          add.lines = list(c("Weights", "Acres", "Acres", "None", "None"), 
                           c("Fixed-effect", "State", "County/Year", "State", "County/Year")))

# Model Table
star2 <- stargazer(cs.dd.ln_corn_rrev , p.dd.ln_corn_rrev, cs.dd.p_corn_share, p.dd.p_corn_share, align = FALSE, no.space = FALSE, 
                   style = "aer", digits = 2,
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
          font.size = "footnotesize",
          star.cutoffs = NA,
          add.lines = list(c("Weights", "Acres", "Acres", "None", "None"),
                           c("Fixed-effect", "State", "County/Year", "State", "County/Year")))

# Model Table
star3 <- stargazer(diff.temp.ln_corn_rrev, diff.dd.ln_corn_rrev, diff.temp.p_corn_share, diff.dd.p_corn_share, 
                   align = FALSE, no.space = FALSE, style = "aer", digits = 2,
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
           font.size = "footnotesize",
           add.lines = list(c("Weights", "Acres", "Acres", "None", "None"), 
                            c("Fixed-effect", "County/Year", "County/Year", "County/Year", "County/Year")))
          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "regression_tables.tex")
cat(star1, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "regression_tables.tex", append = TRUE)
cat(star2, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "regression_tables.tex", append = TRUE)
cat(star3, file = "regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex regression_tables.tex")
}

