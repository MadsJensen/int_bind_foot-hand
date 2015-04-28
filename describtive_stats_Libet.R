setwd("~/Projects/int_bind_foot-hand/Libet-part/data")
# source("/home/mje/Projects/int_bind_foot-hand/importFun.R")
source("/Users/mads/Projects/int_bind_foot-hand/importFun.R")

library(dplyr)


subjects = c(101:109, 111:117)
conditions = c("M-press-hand", "M-press-hand1")

for (ii in subjects){
    nam <- paste("sub_",ii,"_M", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "hand"
    sub.data$condition <- "M"
    assign(nam, sub.data)
}

conditions = c("W-press-hand", "W-press-hand1")

for (ii in subjects){
    nam <- paste("sub_",ii,"_W", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "hand"
    sub.data$condition <- "W"
    assign(nam, sub.data)
}


hand_list <- ls(pattern = "sub_1")
all_hand = get(hand_list[1])
    
for (j in 1:length(hand_list)) {
    if (j == 1){all_hand <- get(hand_list[j])}
    else
    {all_hand <- bind_rows(all_hand, get(hand_list[j]))}
    }


conditions = c("M-press-foot", "M-press-foot1")

for (ii in subjects){
    nam <- paste("sub_",ii,"_M", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "foot"
    sub.data$condition <- "M"
    assign(nam, sub.data)
}

conditions = c("W-press-foot", "W-press-foot1")

for (ii in subjects){
    nam <- paste("sub_",ii,"_W", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "foot"
    sub.data$condition <- "W"
    assign(nam, sub.data)
}

foot_list <- ls(pattern = "sub_1")
all_foot = get(foot_list[1])
    
for (j in 1:length(foot_list)) {
    if (j == 1) {all_foot <- get(foot_list[j])}
    else
    {all_foot <- bind_rows(all_foot, get(foot_list[j]))}
    } 

# combine data into data_frame
all_data = bind_rows(all_hand, all_foot)

all_data$condition <- as.factor(all_data$condition)
all_data$modality <- as.factor(all_data$modality)


data = as.data.frame(filter(all_data, errorTime > -400 & errorTime < 400) %>% 
    select(id, errorTime, modality, condition))

data_grp <- as.data.frame(data %>% group_by(id, modality, condition) %>%
    summarise(mean = mean(errorTime), sd = sd(errorTime)))
