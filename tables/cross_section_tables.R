library(stargazer)

rm(list=ls())

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

# Revenue Per Acre
cs.temp.ln_corn_rrev <- readRDS("models/cs.temp.ln_corn_rrev")
cs.dd.ln_corn_rrev <- readRDS("models/cs.dd.ln_corn_rrev")

cs.temp.ln_cotton_rrev <- readRDS("models/cs.temp.ln_cotton_rrev")
cs.dd.ln_cotton_rrev <- readRDS("models/cs.dd.ln_cotton_rrev")

cs.temp.ln_hay_rrev <- readRDS("models/cs.temp.ln_hay_rrev")
cs.dd.ln_hay_rrev <- readRDS("models/cs.dd.ln_hay_rrev")

cs.temp.ln_wheat_rrev <- readRDS("models/cs.temp.ln_wheat_rrev")
cs.dd.ln_wheat_rrev <- readRDS("models/cs.dd.ln_wheat_rrev")

cs.temp.ln_soybean_rrev <- readRDS("models/cs.temp.ln_soybean_rrev")
cs.dd.ln_soybean_rrev <- readRDS("models/cs.dd.ln_soybean_rrev")

# Crop Share
cs.temp.p_corn_share <- readRDS("models/cs.temp.p_corn_share")
cs.dd.p_corn_share <- readRDS("models/cs.dd.p_corn_share")

cs.temp.p_cotton_share <- readRDS("models/cs.temp.p_cotton_share")
cs.dd.p_cotton_share <- readRDS("models/cs.dd.p_cotton_share")

cs.temp.p_hay_share <- readRDS("models/cs.temp.p_hay_share")
cs.dd.p_hay_share <- readRDS("models/cs.dd.p_hay_share")

cs.temp.p_wheat_share <- readRDS("models/cs.temp.p_wheat_share")
cs.dd.p_wheat_share <- readRDS("models/cs.dd.p_wheat_share")

cs.temp.p_soybean_share <- readRDS("models/cs.temp.p_soybean_share")
cs.dd.p_soybean_share <- readRDS("models/cs.dd.p_soybean_share")


multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)
multiply.10000 <- function(x) (x * 10000)

#Average temp: Cross-Section, probit acreage, panel, difference
star1 <- stargazer(cs.temp.ln_corn_rrev, 
                   cs.temp.ln_cotton_rrev,
                   cs.temp.ln_hay_rrev, 
                   cs.temp.ln_wheat_rrev, 
                   cs.temp.ln_soybean_rrev, 
                   align = FALSE, no.space = FALSE, 
                   style = "aer", digits = 2,
          omit = c("fips", "year","state"), omit.stat = c("ser", "f"), 
          title = "Cross-section Regression Models explaining Crop Revenue per Acre", 
          column.labels = c("Cross-section", "Cross-section", "Cross-section", "Cross-section", "Cross-section"),
          dep.var.labels = c("Log(Corn Rev)", "Log(Cotton Rev)", "Log(Hay Rev)", "Log(Wheat Rev)", "Log(Soybean Rev)"),
          covariate.labels = c("Average Temperature", "Average Temperature Squared", "Precipitation", "Precipitation Squared"),
          model.names = FALSE,
          notes = "All coefficients multiplied by 100",
          notes.align = "l",
          notes.append = FALSE,
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dc#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Weights", "Acres", "Acres", "Acres", "Acres", "Acres"), 
                           c("Fixed-effect", "State", "State", "State", "State", "State"),
                           c("Cluster SE", "State", "State", "State", "State", "State")))

#Average temp: Cross-Section, probit acreage, panel, difference
star2 <- stargazer(cs.dd.ln_corn_rrev, 
                   cs.dd.ln_cotton_rrev,
                   cs.dd.ln_hay_rrev, 
                   cs.dd.ln_wheat_rrev, 
                   cs.dd.ln_soybean_rrev, 
                   align = FALSE, no.space = FALSE, 
                   style = "aer", digits = 2,
          omit = c("fips", "year","state"), omit.stat = c("ser", "f"), 
          title = "Cross-section Regression Models explaining Crop Revenue per Acre", 
          column.labels = c("Cross-section", "Cross-section", "Cross-section", "Cross-section", "Cross-section"),
          dep.var.labels = c("Log(Corn Rev)", "Log(Cotton Rev)", "Log(Hay Rev)", "Log(Wheat Rev)", "Log(Soybean Rev)"), 
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", "Precipitaton", "Precipitation Squared"),
          model.names = FALSE,
          notes = "All coefficients multiplied by 100",
          notes.align = "l",
          notes.append = FALSE,
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dc#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Weights", "Acres", "Acres", "Acres", "Acres", "Acres"), 
                           c("Fixed-effect", "State", "State", "State", "State", "State"),
                           c("Cluster SE", "State", "State", "State", "State", "State")))

star3 <- stargazer(cs.temp.p_corn_share, 
                   cs.temp.p_cotton_share,
                   cs.temp.p_hay_share, 
                   cs.temp.p_wheat_share, 
                   cs.temp.p_soybean_share, 
                   align = FALSE, no.space = FALSE, 
                   style = "aer", digits = 4,
          omit = c("fips", "year","state"), omit.stat = c("ser", "f"), 
          title = "Cross-section Regression Models explaining Proportion of Acres by Crop", 
          column.labels = c("Cross-section", "Cross-section", "Cross-section", "Cross-section", "Cross-section"),
          dep.var.labels = c("Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"), 
          covariate.labels = c("Average Temperature", "Average Temperature Squared", "Precipitation", "Precipitation Squared"),
          model.names = FALSE,
          notes = "All coefficients multiplied by 1000",
          notes.align = "l",
          notes.append = FALSE,
          apply.coef = multiply.1000, apply.se = multiply.1000,
          table.layout ="=dc#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Weights", "Total Acres", "Total Acres", "Total Acres", "Total Acres", "Total Acres"), 
                           c("Fixed-effect", "State", "State", "State", "State", "State"),
                           c("Cluster SE", "State", "State", "State", "State", "State")))

#Average temp: Cross-Section, probit acreage, panel, difference
star4 <- stargazer(cs.dd.p_corn_share, 
                   cs.dd.p_cotton_share,
                   cs.dd.p_hay_share, 
                   cs.dd.p_wheat_share, 
                   cs.dd.p_soybean_share, 
                   align = FALSE, no.space = FALSE, 
                   style = "aer", digits = 4,
          omit = c("fips", "year","state"), omit.stat = c("ser", "f"), 
          title = "Cross-section Regression Models explaining Proportion of Acres by Crop", 
          column.labels = c("Cross-section", "Cross-section", "Cross-section", "Cross-section", "Cross-section"),
          dep.var.labels = c("Corn Acres", "Cotton Acres", "Hay Acres", "Wheat Acres", "Soybean Acres"), 
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", "Precipitaton", "Precipitation Squared"),
          model.names = FALSE,
          notes = "All coefficients multiplied by 1000",
          notes.align = "l",
          notes.append = FALSE,
          apply.coef = multiply.1000, apply.se = multiply.10000,
          table.layout ="=dc#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Weights", "Total Acres", "Total Acres", "Total Acres", "Total Acres", "Total Acres"), 
                           c("Fixed-effect", "State", "State", "State", "State", "State"),
                           c("Cluster SE", "State", "State", "State", "State", "State")))

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "cross_section_regression_tables.tex")
cat(star1, file = "cross_section_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "cross_section_regression_tables.tex", append = TRUE)
cat(star2, file = "cross_section_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "cross_section_regression_tables.tex", append = TRUE)
cat(star3, file = "cross_section_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "cross_section_regression_tables.tex", append = TRUE)
cat(star4, file = "cross_section_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "cross_section_regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex cross_section_regression_tables.tex")
}


