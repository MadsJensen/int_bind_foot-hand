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



  