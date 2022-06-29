install.packages("aqp")
library(aqp)
install.packages("soilDB")
library(soilDB)
install.packages("ggplot2")
library(ggplot2)
data("loafercreek")
View(loafercreek)
# Construct generalized horizon designations
n <- c("A","BAt", "Bt1","Bt2","Cr", "R")
p <- c("A", "BA|AB", "bt|Bw", "Bt3|Bt4|2B|C" ,"Cr","R")

 loafercreek$genhz <- generalize.hz(
  loafercreek$hzname,
  n,p)
h <- horizons(loafercreek)

table(h$genhz, h$hzname)
vars <- c("genhz", "clay","total_frags_pct",
          "phfield", "effclass")
summary(h[, vars])

sort(unique(h$hzname))
h$hzname <- ifelse(h$hzname == "BT",
                   "Bt", h$hzname)

