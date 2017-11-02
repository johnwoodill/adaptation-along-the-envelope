library(stargazer)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
  }

# Revenue Per Acre
  mod0 <- readRDS("models/dd_mod0.rds")
  mod1 <- readRDS("models/dd_mod1.rds")
  mod2 <- readRDS("models/dd_mod2.rds")
  mod3 <- readRDS("models/dd_mod3.rds")
  mod4 <- readRDS("models/dd_mod4.rds")
  mod5 <- readRDS("models/dd_mod5.rds")
  
modc1 <- readRDS("models/dd_modc1.rds")
modc2 <- readRDS("models/dd_modc2.rds")
modco1 <- readRDS("models/dd_modco1.rds")
modco2 <- readRDS("models/dd_modco2.rds")
modh1 <- readRDS("models/dd_modh1.rds")
modh2 <- readRDS("models/dd_modh2.rds")
mods1 <- readRDS("models/dd_mods1.rds")
mods2 <- readRDS("models/dd_mods2.rds")
modw1 <- readRDS("models/dd_modw1.rds")
modw2 <- readRDS("models/dd_modw2.rds")

  # mod3$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")] <- 
  #   multiply.100(mod3$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")])
  # 
  # mod4$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")] <- 
  # multiply.100(mod3$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")])
  
  mod0$coefficients[2:4] <- adj_ln(mod0$coefficients[2:4])
  mod1$coefficients[1:3] <- adj_ln(mod1$coefficients[1:3])
  mod2$coefficients[1:3] <- adj_ln(mod2$coefficients[1:3])
  mod3$coefficients[6:8] <- adj_ln(mod3$coefficients[6:8])
  mod4$coefficients[6:8] <- adj_ln(mod4$coefficients[6:8])
  mod5$coefficients[6:8] <- adj_ln(mod5$coefficients[6:8])
  
  modc1$coefficients[7:9] <- adj_ln(modc1$coefficients[7:9])
  modco1$coefficients[7:9] <- adj_ln(modco1$coefficients[7:9])
  modh1$coefficients[7:9] <- adj_ln(modh1$coefficients[7:9])
  mods1$coefficients[7:9] <- adj_ln(mods1$coefficients[7:9])
  modw1$coefficients[7:9] <- adj_ln(modw1$coefficients[7:9])

    # mod0$coefficients[c("tau", "omega", "did")] <- adj_ln(mod0$coefficients[c("tau", "omega", "did")])
  # mod1$coefficients[c("tau", "omega", "did")] <- adj_ln(mod1$coefficients[c("tau", "omega", "did")])
  # mod2$coefficients[c("tau", "omega", "did")] <- adj_ln(mod2$coefficients[c("tau", "omega", "did")])
  # mod3$coefficients[c("tau", "omega", "did")] <- adj_ln(mod3$coefficients[c("tau", "omega", "did")])
  # mod4$coefficients[c("tau", "omega", "did")] <- adj_ln(mod4$coefficients[c("tau", "omega", "did")])
  # mod5$coefficients[c("tau", "omega", "did")] <- adj_ln(mod5$coefficients[c("tau", "omega", "did")])



star1 <- stargazer(mod0, mod1, mod2, mod3, mod4, 
                  align = FALSE, no.space = FALSE, 
                  style = "aer", digits = 2,
                  omit = c("fips", "year"), 
                  omit.stat = c("ser", "f"),
                  title = "Difference-in-Difference Regression Model explaining Crop Revenue per Acre", 
                  column.labels = c("Basic Model", "Basic Model", "Climate Model", "Climate Model", "Climate Model"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"), 
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
                               "Precipitaton", "Precipitation Squared", "Post - 0:1950-1980/1:1980-2010", 
                               "Treat - County 0:cooled/1:warmed the most", "Treatment-effect",
                               "State-by-Year Trend"),
          model.names = FALSE, omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "", "State", "", "State", "State \\& Year"),
                           c("Clusterd SE", "", "State", "", "State", "State \\& Year")),
          notes.append = FALSE, notes.align = "l",
          notes = "asdf")
#star1

star2 <- stargazer(modc1, modco1, modh1, mods1, modw1,
                  align = FALSE, no.space = FALSE, 
                  style = "aer", digits = 2,
                  omit = c("fips", "year"), 
                  omit.stat = c("ser", "f"),
                  title = "Difference-in-Difference Regression Model explaining Crop Acres", 
                  #column.labels = c("Basic Model", "Basic Model", "Climate Model", "Climate Model", "Climate Model"),
          dep.var.labels = c("Log(Corn Acres)",  "Log(Cotton Acres)", "Log(Hay Acres)", "Log(Soybeans Acres)", "Log(Wheat Acres)"),
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
                              "Precipitaton", "Precipitation Squared", "State-by-Year Trend", "Post - 0:1950-1980/1:1980-2010", 
                               "Treat - County 0:cooled/1:warmed the most", "Treatment-effect"
                               ),
          model.names = FALSE, omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Mean of Dep Variable", "9.30", "8.05",  "9.64", "9.17", "9.18"),
                           c("Fixed-effect", "State", "State", "State", "State", "State"),
                           c("Clusterd SE", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l",
          notes = "asdf")
#star1

star3 <- stargazer(modc2, modco2, modh2, mods2, modw2, 
                  align = FALSE, no.space = FALSE, 
                  style = "aer", digits = 2,
                  omit = c("fips", "year"), 
                  omit.stat = c("ser", "f"),
                  title = "Difference-in-Difference Regression Model explaining Crop Acres", 
                  #column.labels = c("Basic Model", "Basic Model", "Climate Model", "Climate Model", "Climate Model"),
          dep.var.labels = c("Corn Share", "Cotton Share", "Hay Share", "Soybeans Share", "Wheat Share"), 
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
                              "Precipitaton", "Precipitation Squared", "State-by-Year Trend", "Post - 0:1950-1980/1:1980-2010", 
                               "Treat - County 0:cooled/1:warmed the most", "Treatment-effect"
                               ),
          model.names = FALSE, omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Mean of Dep Variable", "0.21", "0.11",  "0.22", "0.20", "0.11"),
                             c("Fixed-effect", "State", "State", "State", "State", "State"),
                           c("Clusterd SE", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l",
          notes = "asdf")
#star1

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "dd_crop_revenue_tables.tex")
cat(star1, file = "dd_crop_revenue_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "dd_crop_revenue_tables.tex", append = TRUE)
#cat("\\begin{landscape}\n", file = "dd_crop_revenue_tables.tex", append = TRUE)
cat(star2, file = "dd_crop_revenue_tables.tex", sep = "\n", append = TRUE)
cat(star3, file = "dd_crop_revenue_tables.tex", sep = "\n", append = TRUE)
#cat("\\end{landscape}\n", file = "dd_crop_revenue_tables.tex", append = TRUE)
cat("\\end{document}", file = "dd_crop_revenue_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex dd_crop_revenue_tables.tex")
}
 
                  