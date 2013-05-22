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

