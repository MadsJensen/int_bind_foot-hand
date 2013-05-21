library(reshape)
setwd("~/Projects/int_binding_foot/Data/libet_data")

subnumbers = paste(1:16)
condition <-rep(c("M", "W", "M", "W"), each=60)
modality <- rep(c("hand", "foot"), each=120)


for (i in subnumbers) {
  foo = data.frame()
  sub_name <- paste("sub_", i, "_libet", sep="")
  sub_name_long <- paste("sub_", i, "_libet_long", sep="")
  csv_name <-paste("libet_sub_", i, ".csv", sep="")
  foo <- read.csv(file=csv_name, header=FALSE)
  
  names(foo) <- c("m_hand", "w_hand", "m_foot", "w_foot")
  foo$subid <- i
  assign(sub_name, foo)
  
  foo = melt(foo, id="subid", measured=var_names)
  foo$condition <- condition
  foo$modality <- modality
  names(foo) <- c("subid", "orig_cond", "time", "condition","modality")
  assign(sub_name_long, foo)
  
  
}

data_all = rbind(sub_1_libet, sub_2_libet, sub_3_libet, sub_4_libet, 
                 sub_5_libet, sub_6_libet, sub_7_libet, sub_8_libet,
                 sub_9_libet, sub_10_libet, sub_11_libet, sub_12_libet,
                 sub_13_libet, sub_14_libet, sub_15_libet, sub_16_libet)

data_all_long = rbind(sub_1_libet_long, sub_2_libet_long, sub_3_libet_long, 
                      sub_4_libet_long, sub_5_libet_long, sub_6_libet_long,
                      sub_7_libet_long, sub_8_libet_long, sub_9_libet_long,
                      sub_10_libet_long, sub_11_libet_long, sub_12_libet_long,
                      sub_13_libet_long, sub_14_libet_long, sub_15_libet_long,
                      sub_16_libet_long)

