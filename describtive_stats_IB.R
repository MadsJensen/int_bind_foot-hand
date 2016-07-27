setwd("~/Projects/int_bind_foot-hand/IB-part/data")
# source("/home/mje/Projects/int_bind_foot-hand/importFun.R")
source("~/Projects/int_bind_foot-hand/importFun.R")

library(dplyr)
library(tidyr)


subjects = c(201, 203:205, 207:212, 214,215,217:220)
conditions = c("actionPressHand")

for (ii in subjects){
    nam <- paste("sub_",ii,"_IB", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "hand"
    sub.data$condition <- "actionPress"
    assign(nam, sub.data)
}

conditions = c("singlePressHand")

for (ii in subjects){
    nam <- paste("sub_",ii,"_single", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "hand"
    sub.data$condition <- "baseline"
    assign(nam, sub.data)
}

hand_list <- ls(pattern = "sub_2")
all_hand = get(hand_list[1])

for (j in 1:length(hand_list)) {
    if (j == 1){all_hand <- get(hand_list[j])}
    else
    {all_hand <- bind_rows(all_hand, get(hand_list[j]))}
}

conditions = c("actionPressFoot")

for (ii in subjects){
    nam <- paste("sub_",ii,"_IB", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "foot"
    sub.data$condition <- "actionPress"
    assign(nam, sub.data)
}

conditions = c("singlePressFoot")

for (ii in subjects){
    nam <- paste("sub_",ii,"_single", sep="")
    sub.data <- importData(ii,conditions)
    sub.data <- tbl_df(sub.data)
    sub.data$modality <- "foot"
    sub.data$condition <- "baseline"
    assign(nam, sub.data)
}

foot_list <- ls(pattern = "sub_2")
all_foot = get(foot_list[1])

for (j in 1:length(foot_list)) {
    if (j == 1){all_foot <- get(foot_list[j])}
    else
    {all_foot <- bind_rows(all_foot, get(foot_list[j]))}
}

all_data <- bind_rows(all_hand, all_foot)
all_data$condition <- as.factor(all_data$condition)
all_data$modality <- as.factor(all_data$modality)
all_data$id <- as.factor(all_data$id)

data = as.data.frame(filter(all_data, errorTime > -400 & errorTime < 400) %>% 
    select(id, errorTime, modality, condition))

data_grp <- as.data.frame(data %>% group_by(id, modality, condition) %>%
    summarise(mean = mean(errorTime), sd = sd(errorTime)))

data_wide_baseline <- filter(data, condition == "baseline") %>% 
    group_by(id, modality) %>%
    summarise(mean = mean(errorTime)) %>% spread(modality, mean)
data_wide_baseline <- data_wide_baseline %>% mutate(diff = foot - hand)


data_wide_action <- filter(data, condition == "actionPress") %>% 
    group_by(id, modality) %>%
    summarise(mean = mean(errorTime)) %>% spread(modality, mean)
data_wide_action <- data_wide_action %>% mutate(diff = foot - hand)

data_wide_foot <- filter(data, modality == "foot") %>% 
    group_by(id, condition) %>%
    summarise(mean = mean(errorTime)) %>% spread(condition, mean)
data_wide_foot <- data_wide_foot %>% mutate(diff = actionPress - baseline,
                                            modality = "foot")


data_wide_hand <- filter(data, modality == "hand") %>% 
    group_by(id, condition) %>%
    summarise(mean = mean(errorTime)) %>% spread(condition, mean)
data_wide_hand <- data_wide_hand %>% mutate(diff = actionPress - baseline,
                                            modality = "hand")

data_wide = bind_rows(data_wide_hand, data_wide_foot)
