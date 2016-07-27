# IB data -----------------------------------------------------------------

library(lme4)

m1 <-
  lmer(
    mean ~ 1 + (1 | id) + (1 | id:modality) + (1 | id:condition),
    data = data_grp,  REML = FALSE
  )

m2 <- update(m1, . ~ . + modality)
m3 <- update(m2, . ~ . + condition)
m4 <- update(m3, . ~ . + condition:modality)

anova(m1,m2,m3,m4)


library(multcomp)
# linear testing
postHocs.cond <- glht(m4, linfct = mcp(condition = "Tukey"))
summary(postHocs.cond)
confint(postHocs.cond)

postHocs.mod <- glht(m4, linfct = mcp(modality = "Tukey"))
summary(postHocs.mod)
confint(postHocs.mod)

# folluw up t-test

mod_foot = subset(data, data$modality == "foot")
mod_hand = subset(data, data$modality == "hand")
t.test(mod_foot$errorTime, mod_hand$errorTime, paired = TRUE)

cond_action = subset(IB_mean_long, IB_mean_long$condition == "action")
cond_tone = subset(IB_mean_long, IB_mean_long$condition == "tone")
t.test(cond_action$meanShift, cond_tone$meanShift, paired = TRUE)


#### EZ ####

library(ez)

fooAnova <- ezANOVA(
  data = data_grp,
  dv = .(mean),
  wid = .(id),
  within = .(modality, condition),
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
