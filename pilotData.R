## Experience of press time (M-time)... pilot
source('~/Dropbox/Dist-prox expeience/R-scripts/importFun.R')
setwd('~/Dropbox/Dist-prox expeience/Pilot1/Data')
library(lattice)

subjects = c(1,2,3,4)
conditions = c('Libet','Libet2','selfChoice','selfChoice2','preChoice','preChoice2')

for (ii in subjects){
  nam <- paste("data.sub",ii, sep="")
  sub.data <- importData(ii,conditions)
  assign(nam, sub.data)
}

x.data <- rbind(data.sub1,data.sub2,data.sub3,data.sub4)
x.data$condition <- replace(x.data$condition,
                            x.data$condition=="Libet2","Libet")
x.data$condition <- replace(x.data$condition,
                            x.data$condition=="selfChoice2","selfChoice")
x.data$condition <- replace(x.data$condition,
                            x.data$condition=="preChoice2","preChoice")
x.data$condition <- factor(x.data$condition)

sub.means <- aggregate(x.data$errorTime, 
                       by=list(id=x.data$id, con=x.data$condition),
                       mean)
aggregate(x.data$errorTime, by=list(id=x.data$id, con=x.data$condition), sd)

barchart(x~id,group=con,data=sub.means,auto.key=T,type="a")
xyplot(errorTime~condition|id,data=x.data,groups=id,auto.key=T,type="a")
xyplot(errorTime~condition,data=x.data,groups=id,auto.key=T,type="a")
xyplot(errorTime~id|condition,data=x.data,auto.key=T,type="a")



x.mod <- lmer(errorTime~condition+I(no-mean(no))+(1|id:condition),data=x.data,REML=F)
display(x.mod)
coef(x.mod)