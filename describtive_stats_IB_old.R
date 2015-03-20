
#source('/home/mje/Projects/int_binding_foot/Scripts/load_files.R')

library(pastecs)
library(reshape)

#### describtive statistics IB data ####
setwd("~/Projects/int_binding_foot/Data/IB_data/")

subnumbers <- c(1,10,11,12,13,14,15,16,2,3,4,5,6,7,8,9)

options(digits=2)
# by(IB_data_all_long$time,
#    IB_data_all_long[, c("subid", "orig_cond")],
#    stat.desc, basic=F, norm=T)

options(digits=4)
IB_mean_table <- as.data.frame(tapply(IB_data_all_long$value,
                                      data.frame(IB_data_all_long$subid,
                                                 IB_data_all_long$variable),
                                      mean,
                                      na.rm=TRUE
))

IB_sd_table <- as.data.frame(tapply(IB_data_all_long$value,
                                    data.frame(IB_data_all_long$subid,
                                               IB_data_all_long$variable),
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



#### shift data long format ####

IB_long_vars <- c("actionHandShift", "actionFootShift", "toneHandShift", "toneFootShift")
condition <-rep(c("action", "action", "tone", "tone"), each=16)
modality <- rep(c("hand", "foot"), each=16)
foobar <- IB_mean_table[,8:12]

IB_mean_long = melt(foobar, id="subid", 
                    measured=c("actionHandShift", "actionFootShift", "toneHandShift", "toneFootShift"))
IB_mean_long$condition <- condition
IB_mean_long$modality <- modality
names(IB_mean_long) <- c("subid", "orig_cond", "meanShift", "condition","modality")

IB_mean_long$modality <- as.factor(IB_mean_long$modality)
IB_mean_long$condition <- as.factor(IB_mean_long$condition)

IB_mean_grp <- groupedData(meanShift ~ 1 | subid,  data = IB_mean_long )

