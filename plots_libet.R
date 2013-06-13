library(ggplot2)

ggplot(data_all_long_no_outlier, aes(subid, time)) + 
  geom_boxplot() + 
   coord_flip() +
  facet_wrap(~modality+condition)


ggplot(data_all_long_no_outlier, aes(x=time)) +
  geom_histogram() +
  facet_wrap(condition~modality)


qplot(time, data=data_all_long_no_outlier, geom="histogram") + facet_wrap(subid~condition*modality)

qplot(time, data=data_all_long_no_outlier, geom="histogram") + facet_wrap(~condition*modality)

qplot(time, data=data_all_long_no_outlier, geom="histogram") + facet_wrap(subid~condition)
qplot(time, data=data_all_long_no_outlier, geom="histogram") + facet_wrap(subid~modality)

qplot(time, data=data_all_long_no_outlier, geom="histogram") + facet_wrap(~condition)
qplot(time, data=data_all_long_no_outlier, geom="histogram") + facet_wrap(~modality)





ggplot(IB_data_all_long_no_outlier, aes(condition, time)) + 
  geom_boxplot() + 
  coord_flip() +
  facet_wrap(~subid)

ggplot(IB_data_all_long_no_outlier, aes(subid, time)) + 
  geom_boxplot() + 
  coord_flip() +
  facet_wrap(~condition)

ggplot(IB_data_all_long_no_outlier, aes(condition, time)) + 
  geom_boxplot() + 
  coord_flip() 


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



