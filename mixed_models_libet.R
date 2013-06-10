
source('/home/mje/Projects/int_binding_foot/Scripts/load_files_IB.R')
source('/home/mje/Projects/int_binding_foot/Scripts/describtive_stats_IB.R')


library(lme4)
# 
# 
# m1 <- lmer(mean ~ 1 + (1|subid) + (1|modality) + (1 |condition), data=all_mean_long_no_outlier, REML=FALSE)
# m2 <- update(m1, .~. + modality)
# m3 <- update(m2, .~. + condition)
# m4 <- update(m3, .~. + condition:modality)
# 
# anova(m1,m2,m3,m4)
# 
# 
# # test random effect
# mR1 <- lmer(mean ~ modality*condition + (1|subid) + (1|modality) + (1 |condition), data=all_mean_long_no_outlier, REML=FALSE)
# mR2 <- lmer(mean ~ modality*condition + (1|subid), data=all_mean_long_no_outlier, REML=FALSE)
# anova(mR1, mR2)
# 
# 
# library(nlme)
# 
# # test random effect
# mR1 <- lme(mean ~ condition*modality, random = ~1|subid/condition/modality , 
#           data=all_mean_long_no_outlier, method="ML")
# mR2 <- lme(mean ~condition*modality, random = ~1|subid , 
#            data=IB_mean_long, method="ML")
# anova(mR1, mR2)
# 
# # build models
# m1 <- lme(mean ~ 1, random = ~1|subid/condition/modality , 
#           data=all_mean_long_no_outlier, method="ML")
# m2 <- update(m1, .~. + modality)
# m3 <- update(m2, .~. + condition)
# m4 <- update(m3, .~. + condition:modality)
# 
# anova(m1,m2,m3,m4)
# 
# m4.reml <- update(m4, method="REML")
# m5 <- lme(mean ~condition*modality -1, random = ~1|subid , 
#           data=all_mean_long_no_outlier, method="REML")
# 
# # linear testing
# postHocs.cond<-glht(m4.reml, linfct = mcp(condition = "Tukey"))
# summary(postHocs.cond)
# confint(postHocs.cond)
# 
# postHocs.mod<-glht(m4.reml, linfct = mcp(modality = "Tukey"))
# summary(postHocs.mod)
# confint(postHocs.mod)
# 
# 
# 

#### EZ ####
# 
# library(ez)
# 
# fooAnova <- ezANOVA(
#   data = IB_mean_long,
#   dv = .(mean),
#   wid = .(subid),
#   within = .(modality, condition),
#   type = 3,
#   detailed = TRUE
# )
# print(fooAnova)
# 
# 
# fooTablefoo <- fooAnova$ANOVA
# fooTablefoo$p <- round(fooTablefoo$p, 4)
# fooTablefoo$F <- round(fooTablefoo$F, 4)
# fooTablefoo$SSn <- round(fooTablefoo$SSn, 4)
# fooTablefoo$SSd <- round(fooTablefoo$SSd, 4)
# fooTablefoo$ges <- round(fooTablefoo$ges, 4)
# print(fooTablefoo)
# 

#### IB data ####

library(lme4)
# test random effect
mR1 <- lmer(meanShift ~ modality*condition + (1|subid) + (1|modality) + (1 |condition), data=IB_mean_long, REML=FALSE)
mR2 <- lmer(meanShift ~ modality*condition + (1|subid), data=IB_mean_long, REML=FALSE)
anova(mR1, mR2)

# build and test models
m1 <- lmer(meanShift ~ 1 + (1|subid) , data=IB_mean_long, REML=FALSE)
m2 <- update(m1, .~. + condition)
m3 <- update(m2, .~. + modality)
m4 <- update(m3, .~. + condition:modality)

anova(m1,m2,m3,m4)

m_test <- lmer(meanShift ~ condition*modality + (1|subid) , data=IB_mean_long, REML=FALSE)


library(nlme)

# test random effect
mR1 <- lme(meanShift ~ condition*modality, random = ~1|subid/condition/modality , 
           data=IB_mean_long, method="ML")
mR2 <- lme(meanShift ~condition*modality, random = ~1|subid , 
           data=IB_mean_long, method="ML")
anova(mR1, mR2)

# build models
m1 <- lme(meanShift ~ 1, random = ~1|subid/condition/modality , 
          data=IB_mean_long, method="ML")
m2 <- update(m1, .~. + condition)
m3 <- update(m2, .~. + modality )
m4 <- update(m3, .~. + condition:modality)

anova(m1,m2,m3,m4)

m4.reml <- update(m4, method="REML")
m5 <- lme(meanShift ~ condition*modality -1, random = ~1|subid , 
          data=IB_mean_long, method="REML")

library(multcomp)
# linear testing
postHocs.cond<-glht(m4, linfct = mcp(condition = "Tukey"))
summary(postHocs.cond)
confint(postHocs.cond)

postHocs.mod<-glht(m4, linfct = mcp(modality = "Tukey"))
summary(postHocs.mod)
confint(postHocs.mod)

# folluw up t-test

mod_foot = subset(IB_mean_long, IB_mean_long$modality == "foot")
mod_hand = subset(IB_mean_long, IB_mean_long$modality == "hand")
t.test(mod_foot$meanShift, mod_hand$meanShift, paired=TRUE)

cond_action = subset(IB_mean_long, IB_mean_long$condition == "action")
cond_tone = subset(IB_mean_long, IB_mean_long$condition == "tone")
t.test(cond_action$meanShift, cond_tone$meanShift, paired=TRUE)


x#### EZ ####

library(ez)

fooAnova <- ezANOVA(
  data = IB_mean_long,
  dv = .(meanShift),
  wid = .(subid),
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





