library(dplyr)
library(lme4)

results <- read.csv("results_all.csv")
results$pas_factor <- as.factor(results$PAS)
results$subject <- as.factor(results$subject)


m1 <- glmer(correct ~ 1 + (1|subject), family = "binomial", data = results)
m2 <- update(m1, .~. + condition_type)
m3 <- update(m2, .~. + congruent)
m4 <- update(m3, .~. + in_phase)
m5 <- update(m4, .~. + pas_factor)
m6 <- update(m5, .~. + condition_type:congruent)
m7 <- update(m6, .~. + condition_type:in_phase)
m8 <- update(m7, .~. + congruent:in_phase)
m9 <- update(m8, .~. + condition_type:pas_factor)
m10 <- update(m9, .~. + congruent:pas_factor)
m11 <- update(m10, .~. + in_phase:pas_factor)
m12 <- update(m11, .~. + condition_type:congruent:pas_factor)
m13 <- update(m12, .~. + condition_type:congruent:in_phase)
m14 <- update(m13, .~. + congruent:in_phase:pas_factor)
m15 <- update(m14, .~. + condition_type:congruent:in_phase:pas_factor)

anova(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)


res <- tbl_df(results)
res_grp <- res %>% group_by(condition_type, in_phase, subject, congruent,
                            pas_factor) %>%
          summarise(avg=mean(correct), n=sum(correct))

m1 <- lmer(avg ~ 1 + (1|subject),  data = as.data.frame(res_grp), REML = FALSE)
m2 <- update(m1, .~. + condition_type)
m3 <- update(m2, .~. + congruent)
m4 <- update(m3, .~. + in_phase)
m5 <- update(m4, .~. + pas_factor + (1 | subject:pas_factor))
m6 <- update(m5, .~. + condition_type:congruent)
m7 <- update(m6, .~. + condition_type:in_phase)
m8 <- update(m7, .~. + congruent:in_phase)
m9 <- update(m8, .~. + condition_type:pas_factor)
m10 <- update(m9, .~. + congruent:pas_factor)
m11 <- update(m10, .~. + in_phase:pas_factor)
m12 <- update(m11, .~. + condition_type:congruent:pas_factor)
m13 <- update(m12, .~. + condition_type:congruent:in_phase)
m14 <- update(m13, .~. + congruent:in_phase:pas_factor)
m15 <- update(m14, .~. + condition_type:congruent:in_phase:pas_factor)

anova(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)


library(BayesFactor)

test_data <- res_grp %>% filter(pas_factor == 2) %>%
  group_by(subject, condition_type, in_phase, congruent) %>%
  summarise(correct = mean(avg))

bf_pas2 <- anovaBF(correct ~ condition_type * in_phase * congruent,
              whichRandom = "subject", 
              data = as.data.frame(test_data), iterations = 200000)

test_data <- res_grp %>% filter(pas_factor == 3) %>%
  group_by(subject, condition_type, in_phase, congruent) %>%
  summarise(correct = mean(avg))

bf_pas3 <- anovaBF(correct ~ condition_type * in_phase * congruent,
              whichRandom = "subject", 
              data = as.data.frame(test_data), iterations = 200000)



test_data <- res_grp %>% filter(pas_factor == 4) %>%
  group_by(subject, condition_type, in_phase, congruent) %>%
  summarise(correct = mean(avg))

bf_pas4 <- anovaBF(correct ~ condition_type * in_phase * congruent,
                   whichRandom = "subject", 
                   data = as.data.frame(test_data), iterations = 200000)


library(multcomp)
# linear testing
postHocs.cond <- glht(m4, linfct = mcp(condition = "Tukey"))
summary(postHocs.cond)
confint(postHocs.cond)

postHocs.mod <- glht(m4, linfct = mcp(modality = "Tukey"))
summary(postHocs.mod)
confint(postHocs.mod)

#### EZ ####

library(ez)

fooAnova <- ezANOVA(
  data = as.data.frame(res_grp),
  dv = .(avg),
  wid = .(subject),
  within = .(condition_type, in_phase),
  type = 3,
  detailed = TRUE
)
print(fooAnova)


fooTablefoo <- fooAnova$ANOVA
fooTablefoo$p <- round(fooTablefoo$p, 4)
fooTablefoo$F <- round(fooTablefoo$F, 4)
fooTablefoo$SSn <- round(fooTablefoo$SSn, 4)
fooTablefoo$SSd <- round(fooTablefoo$SSd, 4)
fooTablefoo$ges <- round(fooTablefoo$ges, 4)
print(fooTablefoo)
