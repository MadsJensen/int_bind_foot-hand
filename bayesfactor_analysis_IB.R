library(dplyr)
library(tidyr)
library(BayesFactor)

setwd("/home/mje/Projects/int_bind_foot-hand")
source("/home/mje/Projects/int_bind_foot-hand/describtive_stats_IB.R")

data = as.data.frame(filter(all_data, errorTime > -400 & errorTime < 400) %>% 
    select(id, errorTime, modality, condition))

bf <- lmBF(errorTime ~ .,
           data = data,
           whichRandom = "id")


full <- lmBF(errorTime ~ modality + condition + modality:condition,
             data=data,
             whichRandom = "id")

noInteraction <- lmBF(errorTime ~ modality + condition,
                      data=data,
                      whichRandom = "id")

onlyModality <- lmBF(errorTime ~ modality,
                 data=data,
                 whichRandom = "id")

onlyCondition <- lmBF(errorTime ~ condition,
                 data=data,
                 whichRandom = "id")

allBFs <- c(full, noInteraction, onlyModality, onlyCondition)
allBFs

fullAnova <- anovaBF(errorTime ~ modality + condition + modality:condition,
             data=data,
             whichRandom = "id")

#### T-test ####
hand_data <- filter(all_data, modality == "foot") %>%
    select(id, errorTime) %>% 

W_data_hand <- filter(all_data, condition == "W" & modality == "hand") %>%
    select(id, errorTime) %>% spread(W_data, modality, mean)
    filter(errorTime > -400 & errorTime < 400)

W_data <- filter(all_data, condition == "W") %>% 
    group_by(id, modality) %>%
    summarise(mean = mean(errorTime))


W_wide <- mutate(W_wide, diff = foot - hand)

bf_W <- ttestBF(x = W_wide$diff)
bf_W
W_chains <- posterior(bf_W, iterations = 1000000)

M_data_foot <- filter(all_data, condition == "M" & modality == "foot") %>%
    select(id, errorTime) %>% 
    filter(errorTime > -400 & errorTime < 400)

M_data_hand <- filter(all_data, condition == "M" & modality == "hand") %>%
    select(id, errorTime) %>%
    
M_data <- filter(all_data, condition == "M") %>% 
    group_by(id, modality) %>%
    summarise(mean = mean(errorTime))

M_wide <- spread(M_data, modality, mean)
M_wide <- mutate(M_wide, diff = foot - hand)


bf_M <- ttestBF(x = M_wide$diff)
bf_M

M_chains <- posterior(bf_M, iterations = 1000000)
