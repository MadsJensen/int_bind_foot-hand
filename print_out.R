source('/home/mje/Projects/int_binding_foot/Scripts/load_files_IB.R')
source('/home/mje/Projects/int_binding_foot/Scripts/describtive_stats_IB.R')

library(multcomp)
library(nlme)
library(ggplot2)


IB_mean_table[, 8:12]
IB_sd_table[, 8:12]

# mean across subjects
sapply(IB_mean_table[,9:12], mean)

#### Anova model ####
m1 <- lme(meanShift ~ 1, random = ~1|subid/condition/modality, 
          data=IB_mean_long, method="ML")
m2 <- update(m1, .~. + condition)
m3 <- update(m2, .~. + modality )
m4 <- update(m3, .~. + condition:modality)

anova(m1,m2,m3,m4)


# linear testing
postHocs.cond<-glht(m4, linfct = mcp(condition = "Tukey"))
summary(postHocs.cond)
confint(postHocs.cond)

postHocs.mod<-glht(m4, linfct = mcp(modality = "Tukey"))
summary(postHocs.mod)
confint(postHocs.mod)

# predicted v fitted residuals
plot(m4)

# no interecpt
m5 <- lme(meanShift ~ condition*modality -1, random = ~1|subid , 
          data=IB_mean_long, method="REML")
plot(m5)

# calc effect size
rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
 print(paste("r = ", r))
}

rcontrast(-2.7074, 15) # condition
rcontrast(1.8706, 30) # modality


# Using EZ to make rep-anova
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


#### interaction plot ####
line <- ggplot(IB_mean_long, aes(condition,meanShift, colour=modality))
line + stat_summary(fun.y = mean, geom = "line", aes(group=modality)) + 
  stat_summary(fun.y = mean, geom = "point", aes(group=modality)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2 )


line <- ggplot(IB_mean_long, aes(modality, meanShift, colour=condition))
line + stat_summary(fun.y = mean, geom = "line", aes(group=condition)) + 
  stat_summary(fun.y = mean, geom = "point", aes(group=condition)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2 )



#### single sub plots ####
foo <- na.omit(IB_data_all_long)


ggplot(subset(foo, foo$variable=="actionPressHand"), aes(trialNr, value)) +
  geom_point() + geom_smooth(method = "lm", alpha = 0.5) + facet_wrap(~subid)

ggplot(subset(foo, foo$variable=="singlePressHand"), aes(trialNr, value)) +
  geom_point() + geom_smooth(method = "lm", alpha = 0.5) + facet_wrap(~subid)


ggplot(subset(foo, foo$variable=="actionPressHand" | 
                foo$variable=="singlePressHand"), aes(trialNr, value)) +
  geom_point(aes(pch=variable)) + 
  geom_smooth(method = "lm", alpha = 0.5, aes(linetype=variable)) + 
  facet_wrap(~subid)
