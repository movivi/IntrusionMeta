rm(list = ls())
library(metafor)
library(weightr)
library(dplyr)

######################
#     User Defined   #
######################
work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)

##FOR INTRUSION_ALL
IntrusionFile <- "IM_ES_Step10_INTALL_INTRUSION.csv"
EmotionFile <- "IM_ES_Step10_INTALL_EMOTION.csv"
SymptomsFile <- "IM_ES_Step10_INTALL_SYMPTOMS.csv"
DiaryFile <- "IM_ES_Step11_INTALL_INTRUSION_DIARY.csv"
QuestionnaireFile <- "IM_ES_Step11_INTALL_INTRUSION_QUESTIONNAIRE.csv"
TaskFile <- "IM_ES_Step11_INTALL_INTRUSION_TASK.csv"
AllFile <- "IM_ES_Step5a_MISMATCHROWS_REMOVED.csv"


#Do you want to calculate the outliers/influential case on which data set 
#(1 = INTRUSION,2 = EMOTION, 3 = INTRUSION SYMPTOMS, 4 = INTRUSION DIARY
# 5 = INTRUSION QUESTIONNAIRE, 6 = INTRUSION TASK, 7 = COMPLETE DATASET,
useAll = 7

if (useAll==1){
  df = read.csv(IntrusionFile)
} else if (useAll == 2) {
  df = read.csv(EmotionFile)
} else if (useAll == 3) {
  df = read.csv(SymptomsFile)
} else if (useAll == 4) {
  df = read.csv(DiaryFile)
} else if (useAll == 5) {
  df = read.csv(QuestionnaireFile)
} else if (useAll == 6) {
  df = read.csv(TaskFile)
} else if (useAll == 7) {
  df = read.csv(AllFile)
}

dv_type = c("INTRUSION","EMOTION","SYMPTOMS","IDIARY","IQUESTIONNAIRE","ITASK","ALL")

#remove NA in yi column for outlier computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#######################
#       Outliers      #
#######################

#Compute studentized residuals
meta <- rma.mv(yi = yi, V = vi, data=df, random = ~ 1 | StudyID/EffectSizeID, control=list(rel.tol=1e-8))
df$StudentizedResiduals = rstudent(meta)$z

#Get a subset of the dataset with only outliers
#Studentized Residuals +-3
df_Studentized_Outliers <- subset(df, StudentizedResiduals < -1.96 | StudentizedResiduals > 1.96)

#Export the outliers
if (useAll == 1){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTALL_INTRUSION.csv",row.names = F)
} else if (useAll == 2){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTALL_EMOTION.csv",row.names = F)
} else if (useAll == 3){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTALL_SYMPTOMS.csv",row.names = F)
} else if (useAll == 4){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTALL_IDIARY.csv",row.names = F)
} else if (useAll == 5){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTALL_IQUESTIONNAIRE.csv",row.names = F)
} else if (useAll == 6){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTALL_ITASK.csv",row.names = F)
} else if (useAll == 7){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTALL_ALL.csv",row.names = F)
}

#######################
#   Influential Case  #
#######################

#Multilevel meta-analysis for SELECTED DF with higher level variable StudyID and lower level variable effect size id
meta <- rma.mv(yi = yi, V = vi, data=df, random = ~ 1 | StudyID/EffectSizeID, control=list(rel.tol=1e-8))


#Cook's distances for each observed outcome
df$cooks <- cooks.distance(meta)
#Plot Cook's distance
png(paste("OutlierInfluential/plots/",dv_type[useAll],"_InfluentialCases_Cooks_distance_INTALL.png",sep=""), width = 8, height = 5, units = 'in', res = 300)
plot(df$cooks, type="o", pch=19, xlab="Index of Observed Outcome", ylab="Cook's Distance")
dev.off()

#DFBETAS
a <- dfbetas(meta)
df$DFBETAS <- a$intrcpt
#PLOT
png(paste("OutlierInfluential/plots/",dv_type[useAll],"_InfluentialCases_DFBETAS_INTALL.png",sep=""), width = 8, height = 5, units = 'in', res = 300)
plot(df$DFBETAS, type="o", pch=19, xlab="Index of Observed Outcome", ylab="DFBETAS")
dev.off()



if (useAll == 1){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTALL_INTRUSION.csv")
} else if(useAll==2){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTALL_EMOTION.csv")
} else if (useAll==3){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTALL_SYMPTOMS.csv")
} else if (useAll==4){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTALL_IDIARY.csv")
} else if (useAll==5){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTALL_IQUESTIONNAIRE.csv")
} else if (useAll==6){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTALL_ITASK.csv")
} else if (useAll==7){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTALL_ALL.csv")
}

####################################################################################################

