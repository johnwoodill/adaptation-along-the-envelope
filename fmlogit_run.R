library(tidyverse)
library(devtools)
#source("/run/media/john/1TB/SpiderOak/Projects/fmlogit/R/fmlogit_main.R")
source("cross_section_regression.R")
load_all("/run/media/john/1TB/SpiderOak/Projects/fmlogit/")
#library("fmlogit", lib.loc = "/run/media/john/1TB/SpiderOak/Projects/fmlogit/")

cropdat$latlong <- cropdat$lat*cropdat$long

# Fractional dep variables
y <- select(cropdat, p_corn_share, p_cotton_share, p_hay_share, p_wheat_share, p_soybean_share )

X <- select(cropdat, dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)

fm <- fmlogit(y, X)
fm
eff <- effects(fm, effect = "marginal")
eff
saveRDS(fm, "models/cs.fmlogit.rds")
saveRDS(eff, "models/cs.fmlogit_effects.rds")



# 
# # data = spending
# # X = data[,2:5]
# # y = data[,6:11]
# # results1 = fmlogit(y,X)
# # results1
# # 
# # effects.fmlogit(results1, effect = "marginal")
# 
# # Variables
# $effects
#                      dday0_10     dday10_30       dday30C          prec       prec_sq
# p_corn_share     9.287073e-05 -1.314853e-04 -9.478394e-05  3.573152e-06  2.157748e-04
# p_cotton_share  -1.267887e-04  9.350218e-05  9.919583e-06 -1.549092e-06 -1.710668e-06
# p_hay_share      3.108540e-04 -2.397696e-04  4.123919e-04  2.064742e-06 -1.025665e-04
# p_wheat_share   -3.128009e-05  9.862408e-05  9.180731e-05 -1.085618e-05 -3.025145e-04
# p_soybean_share -2.456560e-04  1.791286e-04 -4.193349e-04  6.767378e-06  1.910169e-04
# 
# 
# # Centered Variables                     
# dday0_10     dday10_30       dday30C          prec       prec_sq
# p_corn_share     9.950811e-05  0.0001030711 -0.0032220796  0.0004049176  2.145908e-03
# p_cotton_share  -1.983539e-03  0.0006977859 -0.0008671989  0.0001648384 -3.144904e-03
# p_hay_share      3.181227e-03 -0.0014545179  0.0064473862  0.0006496542  1.101035e-03
# p_wheat_share    4.442576e-04 -0.0003613701  0.0046546578 -0.0018575203 -1.964374e-04
# p_soybean_share -1.741454e-03  0.0010150310 -0.0070127655  0.0006381101  9.439759e-05