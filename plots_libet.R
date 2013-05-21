library(ggplot2)


ggplot(data_all_long_no_outlier, aes(subid, time, colour=condition)) + 
  geom_boxplot() + 
   coord_flip() +
  facet_wrap(~modality)
  
