library(BayesFactor)
library(dplyr)
library(tidyr)
library(ggplot2)

# setwd("/home/mje/Projects/int_bind_foot-hand")
# source("/home/mje/Projects/int_bind_foot-hand/describtive_stats_Libet.R")
setwd("/Users/mads/Projects/int_bind_foot-hand")
source("/Users/mads/Projects/int_bind_foot-hand/describtive_stats_Libet.R")

data$id <- as.factor(data$id)



# BayesFactor analyses ----------------------------------------------------
bf <- lmBF(errorTime ~ .,
           data = data,
           whichRandom = "id")


full <- lmBF(
  errorTime ~ modality + condition + modality:condition,
  data = data,
  whichRandom = "id"
)

noInteraction <- lmBF(errorTime ~ modality + condition,
                      data = data,
                      whichRandom = "id")

onlyModality <- lmBF(errorTime ~ modality,
                     data = data,
                     whichRandom = "id")

onlyCondition <- lmBF(errorTime ~ condition,
                      data = data,
                      whichRandom = "id")

allBFs <- c(full, noInteraction, onlyModality, onlyCondition)
allBFs


# Anova -------------------------------------------------------------------

fullAnova_id <-
  anovaBF(
    mean ~ modality + condition + modality:condition + id,
    data = data_grp,
    whichRandom = "id"
  )

fullAnova_all <-
  anovaBF(
    errorTime ~ modality + condition + modality:condition + id,
    data = data,
    whichRandom = "id"
  )

# T-tests -----------------------------------------------------------------
W_data_foot <-
  filter(all_data, condition == "W" & modality == "foot") %>%
  select(id, errorTime) %>%
  filter(errorTime > -400 & errorTime < 400)

W_data_hand <-
  filter(all_data, condition == "W" & modality == "hand") %>%
  select(id, errorTime) %>%
  filter(errorTime > -400 & errorTime < 400)

W_data <- filter(all_data, condition == "W") %>%
  group_by(id, modality) %>%
  summarise(mean = mean(errorTime))

W_wide <- spread(W_data, modality, mean)
W_wide <- mutate(W_wide, diff = foot - hand)

bf_W <- ttestBF(x = W_wide$diff)
bf_W
W_chains <- posterior(bf_W, iterations = 1000000)

M_data_foot <-
  filter(all_data, condition == "M" & modality == "foot") %>%
  select(id, errorTime) %>%
  filter(errorTime > -400 & errorTime < 400)

M_data_hand <-
  filter(all_data, condition == "M" & modality == "hand") %>%
  select(id, errorTime) %>%
  
  M_data <- filter(all_data, condition == "M") %>%
  group_by(id, modality) %>%
  summarise(mean = mean(errorTime))

M_wide <- spread(M_data, modality, mean)
M_wide <- mutate(M_wide, diff = foot - hand)


bf_M <- ttestBF(x = M_wide$diff)
bf_M

M_chains <- posterior(bf_M, iterations = 1000000)


# plots -------------------------------------------------------------------
plot_data <- filter(all_data, errorTime > -402 & errorTime < 400)



ggplot(all_data, aes(x = condition, y = errorTime, colour = modality)) +
  geom_line(data = all_data, aes(y = errorTime, group = modality)) +
  theme_bw()
