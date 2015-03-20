
library(reshape)

subnumbers <- c(1:16)

setwd("/home/mje/Dropbox/Working_projects/int_binding_foot/IB_data")
# setwd("~/Projects/int_binding_foot/Data/IB_data/")

var_names <- c("actionPressHand", "actionPressFoot", "actionToneHand", 
            "actionToneFoot", "singlePressHand", "singlePressFoot", "singleTone");

for (i in subnumbers) {
  foo = data.frame()
  sub_name <- paste("IB_sub_", i, sep="")
  sub_name_long <- paste("IB_sub_", i, "_long", sep="")
  csv_name <-paste("IB_sub_", i, ".csv", sep="")
  foo <- read.csv(file=csv_name, header=FALSE)
  
  names(foo) <- var_names 
  foo$subid <- i
  foo$aPH_sd <- sd(foo$actionPressHand, na.rm=T)
  foo$aPF_sd <- sd(foo$actionPressFoot, na.rm=T)
  foo$aTH_sd <- sd(foo$actionToneHand, na.rm=T)
  foo$aTF_sd <- sd(foo$actionToneFoot, na.rm=T)
  foo$sPH_sd <- sd(foo$singlePressHand, na.rm=T)
  foo$sPF_sd <- sd(foo$singlePressFoot, na.rm=T)
  foo$sT_sd <- sd(foo$singleTone, na.rm=T)
  
  foo$aPH_m <- mean(foo$actionPressHand, na.rm=T)
  foo$aPF_m <- mean(foo$actionPressFoot, na.rm=T)
  foo$aTH_m <- mean(foo$actionToneHand, na.rm=T)
  foo$aTF_m <- mean(foo$actionToneFoot, na.rm=T)
  foo$sPH_m <- mean(foo$singlePressHand, na.rm=T)
  foo$sPF_m <- mean(foo$singlePressFoot, na.rm=T)
  foo$sT_m <- mean(foo$singleTone, na.rm=T)
  
  foo$actionPressHand[foo$actionPressHand > ((2*foo$aPh_sd) + foo$aPH_m) | 
                       foo$actionPressHand < ((-2*foo$aPh_sd) + foo$aPH_m) ] <- NA  
  foo$actionPressFoot[foo$actionPressFoot > ((2*foo$aPF_sd)  + foo$aPF_m)| 
                       foo$actionPressFoot < ((-2*foo$aPF_sd)  + foo$aPF_m)] <- NA
  foo$actionToneHand[foo$actionToneHand > ((2*foo$aTH_sd) + foo$aTH_m) |
                       foo$actionToneHand < ((-2*foo$aTH_sd) + foo$aTH_m)] <- NA
  foo$actionToneFoot[foo$actionToneFoot > ((2*foo$aTF_sd) + foo$aTF_m)| 
                       foo$actionToneFoot < ((-2*foo$aTF_sd) + foo$aTF_m)] <- NA
  foo$actionToneHand[foo$singlePressHand > ((2*foo$sPH_sd) + foo$sPH_m) |
                       foo$singlePressHand < ((-2*foo$sPH_sd) + foo$sPH_m)] <- NA
  foo$actionToneFoot[foo$singlePressFoot > ((2*foo$sPF_sd)  + foo$sPF_m)|
                       foo$singlePressFoot < ((-2*foo$sPF_sd)  + foo$sPF_m)] <- NA
  foo$singleTone[foo$singleTone > ((2*foo$sT_sd) + foo$sT_m) | foo$singleTone < ((-2*foo$sT_sd) + foo$sT_m)] <- NA  
  foo$trialNr <- c(1:40)
  
  
  assign(sub_name, foo)
  
  bar <- foo[,1:8]
  bar = melt(bar, id="subid", measured=var_names)
#   foo$condition <- condition
#   foo$modality <- modality
#   names(foo) <- c("subid", "orig_cond", "time", "condition","modality")
  assign(sub_name_long, bar)
  
}




IB_data_all_long = rbind(IB_sub_1_long, IB_sub_2_long, IB_sub_3_long, IB_sub_4_long, 
                         IB_sub_5_long, IB_sub_6_long, IB_sub_7_long, IB_sub_8_long,
                         IB_sub_9_long, IB_sub_10_long, IB_sub_11_long, IB_sub_12_long,
                         IB_sub_13_long, IB_sub_14_long, IB_sub_15_long, IB_sub_16_long)

IB_data_all_long$trialNr <- rep(c(1:40), times=112)