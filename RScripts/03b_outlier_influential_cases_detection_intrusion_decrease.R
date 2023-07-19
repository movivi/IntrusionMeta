rm(list = ls())
library(metafor)
library(weightr)
library(dplyr)

######################
#     User Defined   #
######################
work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)

##FOR INTRUSION_INCREASE
IntrusionFile <- "IM_ES_Step7_INTDEC_INTRUSION.csv"
EmotionFile <- "IM_ES_Step7_INTDEC_EMOTION.csv"
SymptomsFile <- "IM_ES_Step7_INTDEC_SYMPTOMS.csv"
DiaryFile <- "IM_ES_Step8_INTDEC_INTRUSION_DIARY.csv"
QuestionnaireFile <- "IM_ES_Step8_INTDEC_INTRUSION_QUESTIONNAIRE.csv"
TaskFile <- "IM_ES_Step8_INTDEC_INTRUSION_TASK.csv"
AllFile <- "IM_ES_step6_INTDEC.csv"
DPostFile = "IM_ES_Step9_INTDEC_DPOST.csv"
IDPostFile = "IM_ES_Step9_INTDEC_I+DPOST.csv"
IPostFile = "IM_ES_Step9_INTDEC_IPOST.csv"
PeriFile = "IM_ES_Step9_INTDEC_PERI.csv"
PeriIPostFile = "IM_ES_Step9_INTDEC_PERI+IPOST.csv"
PreFile = "IM_ES_Step9_INTDEC_PRE.csv"
PreIPostFile = "IM_ES_Step9_INTDEC_PRE+IPOST.csv"

#Do you want to calculate the outliers/influential case on which data set 
#(1 = INTRUSION,2 = EMOTION, 3 = INTRUSION SYMPTOMS, 4 = INTRUSION DIARY
# 5 = INTRUSION QUESTIONNAIRE, 6 = INTRUSION TASK, 7 = COMPLETE DATASET,
# 8 = D POST, 9 = I + D POST, 10 = I POST, 11 = PERI, 12 = PERI + I POST, 13 = PRE, 14 = PRE + I POST)
useAll = 14

##IMPORTANT: 9,14 HAVE EMPTY DATASET SO NO NEED TO RUN THEM. 12  DOESN'T GENERATE INFLUENTIAL CASES PERHAPS BECAUSE ONLY 1 ARTICLES IN THE DATASET)

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
} else if (useAll == 8) {
  df = read.csv(DPostFile)
} else if (useAll == 9) {
  df = read.csv(IDPostFile)
} else if (useAll == 10) {
  df = read.csv(IPostFile)
} else if (useAll == 11) {
  df = read.csv(PeriFile)
} else if (useAll == 12) {
  df = read.csv(PeriIPostFile)
} else if (useAll == 13) {
  df = read.csv(PreFile)
} else if (useAll == 14) {
  df = read.csv(PreIPostFile)
}

dv_type = c("INTRUSION","EMOTION","SYMPTOMS","IDIARY","IQUESTIONNAIRE","ITASK","ALL","DPOST","IDPOST","IPOST","PERI","PERIIPOST","PRE","PREIPOST")

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
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_INTRUSION.csv",row.names = F)
} else if (useAll == 2){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_EMOTION.csv",row.names = F)
} else if (useAll == 3){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_SYMPTOMS.csv",row.names = F)
} else if (useAll == 4){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_IDIARY.csv",row.names = F)
} else if (useAll == 5){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_IQUESTIONNAIRE.csv",row.names = F)
} else if (useAll == 6){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_ITASK.csv",row.names = F)
} else if (useAll == 7){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_ALL.csv",row.names = F)
} else if (useAll == 8){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_DPOST.csv",row.names = F)
} else if (useAll == 9){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_I+DPOST.csv",row.names = F)
} else if (useAll == 10){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_IPOST.csv",row.names = F)
} else if (useAll == 11){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PERI.csv",row.names = F)
} else if (useAll == 12){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PERI+IPOST.csv",row.names = F)
} else if (useAll == 13){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PRE.csv",row.names = F)
} else if (useAll == 14){
  write.csv(df_Studentized_Outliers,"OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PRE+IPOST.csv",row.names = F)
  
}

#######################
#   Influential Case  #
#######################

#Multilevel meta-analysis for SELECTED DF with higher level variable StudyID and lower level variable effect size id
meta <- rma.mv(yi = yi, V = vi, data=df, random = ~ 1 | StudyID/EffectSizeID, control=list(rel.tol=1e-8))


#Cook's distances for each observed outcome
df$cooks <- cooks.distance(meta)
#Plot Cook's distance
png(paste("OutlierInfluential/plots/",dv_type[useAll],"_InfluentialCases_Cooks_distance_INTDEC.png",sep=""), width = 8, height = 5, units = 'in', res = 300)
plot(df$cooks, type="o", pch=19, xlab="Index of Observed Outcome", ylab="Cook's Distance")
dev.off()

#DFBETAS
a <- dfbetas(meta)
df$DFBETAS <- a$intrcpt
#PLOT
png(paste("OutlierInfluential/plots/",dv_type[useAll],"_InfluentialCases_DFBETAS_INTDEC.png",sep=""), width = 8, height = 5, units = 'in', res = 300)
plot(df$DFBETAS, type="o", pch=19, xlab="Index of Observed Outcome", ylab="DFBETAS")
dev.off()



if (useAll == 1){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_INTRUSION.csv")
} else if(useAll==2){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_EMOTION.csv")
} else if (useAll==3){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_SYMPTOMS.csv")
} else if (useAll==4){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_IDIARY.csv")
} else if (useAll==5){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_IQUESTIONNAIRE.csv")
} else if (useAll==6){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_ITASK.csv")
} else if (useAll==7){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_ALL.csv")
} else if (useAll == 8){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_DPOST.csv")
} else if (useAll == 9){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_I+DPOST.csv")
} else if (useAll == 10){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_IPOST.csv")
} else if (useAll == 11){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_PERI.csv")
} else if (useAll == 12){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_PERI+IPOST.csv")
} else if (useAll == 13){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_PRE.csv")
} else if (useAll == 14){
  write.csv(df,"OutlierInfluential/Results_Influential_Diagnostics_in_INTDEC_PRE+IPOST.csv")
}  

####################################################################################################

