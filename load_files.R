

setwd("/home/mje/Dropbox/Working_projects/int_binding_foot/libet_data")

subnumbers = c("101", "102", "103", "104", "105", "106", "107", "108", "109", 
               "111", "112", "113", "114", "115", "116", "117")

for (i in subnumbers) {
  fm1 = paste("subject_", i, "_M-press-foot.csv", sep="")
  fm2 = paste("subject_", i, "_M-press-foot1.csv", sep="")
  foot_m1 = read.csv(file=fm1, sep=";")
  foot_m2 = read.csv(file=fm2, sep=";")

  fw1 = paste("subject_", i, "_W-press-foot.csv", sep="")
  fw2 = paste("subject_", i, "_W-press-foot1.csv", sep="")
  foot_w1 = read.csv(file=fw1, sep=";")
  foot_w2 = read.csv(file=fw2, sep=";")
  

#   
  foot_m1$orig_condition = foot_m1$condition
  foot_m1$condition = "M"
  foot_m1$modality = "foot"
  foot_m2$orig_condition = foot_m2$condition
  foot_m2$condition = "M"
  foot_m2$modality = "foot"
  foot_w1$orig_condition = foot_w1$condition
  foot_w1$condition = "W"
  foot_w1$modality = "foot"
  foot_w2$orig_condition = foot_w2$condition
  foot_w2$condition = "W"
  foot_w2$modality = "foot"
  
  foo = rbind(foot_m1, foot_m2, foot_w1, foot_w2)
  foo$ansCalcAngle = foo$ansAngle - foo$pressAngle
  foo$calcTime = foo$ansCalcAngle*2550/360
  foo$test <- 2.56*((foo$ansAngle - foo$pressAngle)/(360)) 

  oname = paste("sub_", i, "_foot", sep="")
  assign(oname, foo)  
  
  
}