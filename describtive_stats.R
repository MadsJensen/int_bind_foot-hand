

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
mean_table <- as.data.frame(tapply(data_all_long$time, 
       data.frame(data_all_long$subid, data_all_long$condition, 
                  data_all_long$modality),
       mean,
       na.rm=TRUE
))

sd_table <- as.data.frame(tapply(data_all_long$time, 
       data.frame(data_all_long$subid, data_all_long$condition, 
                  data_all_long$modality),
       sd,
       na.rm=TRUE
))

all_mean_long <- aggregate(data_all_long$time, 
               by=data.frame(data_all_long$subid, data_all_long$condition, 
                          data_all_long$modality),
               mean,
               na.rm=TRUE
)
names(all_mean_long) <- c("subid", "condition", "modality", "mean")


#### REMOVE OUTLIERS ####
data_all_long_no_outlier <- subset(data_all_long, 
         data_all_long$time > -500 & data_all_long$time < 500)


all_mean_long_no_outlier <- aggregate(data_all_long_no_outlier$time, 
                           by=data.frame(data_all_long_no_outlier$subid, 
                                         data_all_long_no_outlier$condition, 
                                         data_all_long_no_outlier$modality),
                           mean,
                           na.rm=TRUE
)
names(all_mean_long_no_outlier) <- c("subid", "condition", "modality", "mean")


by(data_all_long_no_outlier$time, 
   data_all_long_no_outlier[, c("subid", "modality", "condition")], 
   stat.desc, basic=F, norm=T)


options(digits=4)
mean_table_no_outlier <- as.data.frame(tapply(data_all_long_no_outlier$time, 
                                 data.frame(data_all_long_no_outlier$subid, 
                                            data_all_long_no_outlier$condition, 
                                            data_all_long_no_outlier$modality),
                                 mean,
                                 na.rm=TRUE
))

sd_table_no_outlier  <- as.data.frame(tapply(data_all_long_no_outlier$time, 
                                             data.frame(data_all_long_no_outlier$subid, 
                                                        data_all_long_no_outlier$condition, 
                                                        data_all_long_no_outlier$modality),
                                             sd,
                                             na.rm=TRUE
))

mean_diff_table <- mean_table - mean_table_no_outlier
sd_diff_table <- sd_table - sd_table_no_outlier



