

#### SETUP ####
# working dir
setwd("~/Projects/int_binding_foot/Data/libet_data")

# libraries
library(car)
library(pastecs)


#### describtive statistics Libet data ###
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



#### describtive statistics IB data ####

setwd("~/Projects/int_binding_foot/Data/libet_data")

subnumbers <- c(1,10,11,12,13,14,15,16,2,3,4,5,6,7,8,9)

options(digits=2)
by(IB_data_all_long$time, 
   IB_data_all_long[, c("subid", "orig_cond")], 
   stat.desc, basic=F, norm=T)

options(digits=4)
IB_mean_table <- as.data.frame(tapply(IB_data_all_long$time, 
                                   data.frame(IB_data_all_long$subid, 
                                              IB_data_all_long$orig_cond),
                                   mean,
                                   na.rm=TRUE
))

IB_sd_table <- as.data.frame(tapply(IB_data_all_long$time, 
                                    data.frame(IB_data_all_long$subid, 
                                               IB_data_all_long$orig_cond),
                                    sd,
                                    na.rm=TRUE
))


### make mean table ###
IB_mean_table$subid <- subnumbers
IB_mean_table$actionHandShift <-  IB_mean_table$actionPressHand -IB_mean_table$singlePressHand
IB_mean_table$actionFootShift <-  IB_mean_table$actionPressFoot -IB_mean_table$singlePressFoot
IB_mean_table$toneHandShift <-  IB_mean_table$actionToneHand -IB_mean_table$singleTone
IB_mean_table$toneFootShift <-  IB_mean_table$actionToneFoot -IB_mean_table$singleTone


#### make sd table ####
IB_sd_table$subid <- subnumbers
IB_sd_table$actionHandShift <-  IB_sd_table$actionPressHand -IB_sd_table$singlePressHand
IB_sd_table$actionFootShift <-  IB_sd_table$actionPressFoot -IB_sd_table$singlePressFoot
IB_sd_table$toneHandShift <-  IB_sd_table$actionToneHand -IB_sd_table$singleTone
IB_sd_table$toneFootShift <-  IB_sd_table$actionToneFoot -IB_sd_table$singleTone



#### remove outliers ####
IB_data_all_long_no_outlier <- subset(IB_data_all_long, 
                                   IB_data_all_long$time > -500 & IB_data_all_long$time < 500)
names(IB_data_all_long_no_outlier) <- c("subid", "condition", "time")

IB_mean_all_long_no_outlier <- aggregate(IB_data_all_long_no_outlier$time, 
                                      by=data.frame(IB_data_all_long_no_outlier$subid, 
                                                    IB_data_all_long_no_outlier$orig_cond),
                                      mean,
                                      na.rm=TRUE
)
names(IB_mean_all_long_no_outlier) <- c("subid", "condition", "mean")

sink("IB_desc_no_outliers_output.txt")
by(IB_data_all_long_no_outlier$time, 
   IB_data_all_long_no_outlier[, c("subid", "condition")], 
   stat.desc, basic=F, norm=T)
sink()
