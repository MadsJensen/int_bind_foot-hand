

#### SETUP ####
# working dir
setwd("~/Projects/int_binding_foot/Data/libet_data")

# libraries
library(car)
library(pastecs)


#### describtive statistics ####
tapply(sub_1_libet_long$time, by=c("condition","modality"), mean)


options(scipen=100)
options(digits=2)
by(data_all_long$time, 
   data_all_long[, c("subid", "modality", "condition")], 
   stat.desc, basic=F, norm=T)


options(digits=4)
all_mean <- as.data.frame(tapply(data_all_long$time, 
       data.frame(data_all_long$subid, data_all_long$condition, 
                  data_all_long$modality),
       mean,
       na.rm=TRUE
))

all_sd <- as.data.frame(tapply(data_all_long$time, 
       data.frame(data_all_long$subid, data_all_long$condition, 
                  data_all_long$modality),
       sd,
       na.rm=TRUE
))

