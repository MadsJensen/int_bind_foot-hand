### Function for importing and pre-cleaning Libet data
# created 21.09.14 @MCV

importData <- function(subject.name, conditions){  
  for (con in 1:length(conditions)){
    print(conditions[con])
    temp.dat <- read.csv(paste(c("subject_",subject.name,"_",conditions[con],".csv")
                               ,collapse=''), sep=";",header=T)
    if (match(conditions[con],conditions)==1){
      sub.data <- temp.dat
    }
    else{
      sub.data <- rbind(sub.data,temp.dat)
    }
  }
  sub.data$userError <- as.logical(sub.data$userError)
  sub.data$id <- as.factor(sub.data$id)
  
  sub.data <- subset(sub.data, userError==F)
  
  recalc.1 <- ifelse(sub.data$ansAngle < 90 & sub.data$pressAngle > 270,T,F)
  sub.data$ansAngle[recalc.1] <- sub.data$ansAngle[recalc.1]+360             # +1,+H
  recalc.2 <- ifelse(sub.data$ansAngle>270 & sub.data$pressAngle<90,T,F)
  sub.data$pressAngle[recalc.2] <- sub.data$pressAngle[recalc.2]+360           # +H,+1
  sub.data$recalc <- recalc.1 | recalc.2
  
  sub.data$errorAngle <- sub.data$ansAngle-sub.data$pressAngle
  sub.data$errorTime <- sub.data$errorAngle*2560/360
  
#   sub.data$condition <- replace(sub.data$condition, sub.data$condition=="Libet2","Libet")
#   sub.data$condition <- replace(sub.data$condition, sub.data$condition=="selfChoice2","selfChoice")
#   sub.data$condition <- replace(sub.data$condition, temp.dat$condition=="preChoice2","preChoice")

  return(sub.data)
}
