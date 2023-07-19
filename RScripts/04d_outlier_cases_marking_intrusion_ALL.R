rm(list = ls())
# Based on the outlier results of DV and measurement mode outlier results respectively from script 03,
# mark down these outliers to the data file

work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)
options(scipen=999)

##FOR INTRUSION_ALL
IntrusionFile <- "IM_ES_Step10_INTALL_INTRUSION.csv"
EmotionFile <- "IM_ES_Step10_INTALL_EMOTION.csv"
SymptomsFile <- "IM_ES_Step10_INTALL_SYMPTOMS.csv"
DiaryFile <- "IM_ES_Step11_INTALL_INTRUSION_DIARY.csv"
QuestionnaireFile <- "IM_ES_Step11_INTALL_INTRUSION_QUESTIONNAIRE.csv"
TaskFile <- "IM_ES_Step11_INTALL_INTRUSION_TASK.csv"
AllFile <- "IM_ES_Step5a_MISMATCHROWS_REMOVED.csv"


# The file path that contains the outlier results
IntrusionOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTALL_INTRUSION.csv"
EmotionOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTALL_EMOTION.csv"
SymptomsOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTALL_SYMPTOMS.csv"
DiaryOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTALL_IDIARY.csv"
QuestionnaireOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTALL_IQUESTIONNAIRE.csv"
TaskOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTALL_ITASK.csv"




#Get the EffectSizeID in the outlier files
IntrusionOutDf <- read.csv(IntrusionOutFile)
IntrusionOutID <- IntrusionOutDf$EffectSizeID

EmotionOutDf <- read.csv(EmotionOutFile)
EmotionOutID <- EmotionOutDf$EffectSizeID

SymptomsOutDf <- read.csv(SymptomsOutFile)
SymptomsOutID <- SymptomsOutDf$EffectSizeID

DiaryOutDf <- read.csv(DiaryOutFile)
DiaryOutID <- DiaryOutDf$EffectSizeID

QuestionnaireOutDf <- read.csv(QuestionnaireOutFile)
QuestionnaireOutID <- QuestionnaireOutDf$EffectSizeID

TaskOutDf <- read.csv(TaskOutFile)
TaskOutID <- TaskOutDf$EffectSizeID

#Mark down which outcomes are outliers into the datafile
alldf <- read.csv(AllFile)
alldf$outliers <- ifelse(alldf$EffectSizeID %in% IntrusionOutID | alldf$EffectSizeID %in% EmotionOutID | alldf$EffectSizeID %in% SymptomsOutID | alldf$EffectSizeID %in% DiaryOutID | alldf$EffectSizeID %in% QuestionnaireOutID | alldf$EffectSizeID %in% TaskOutID, 1,0)
write.csv(alldf, AllFile,row.names = F)


Intrusiondf <- read.csv(IntrusionFile)
Intrusiondf$outliers <- ifelse(Intrusiondf$EffectSizeID %in% IntrusionOutID, 1,0)
write.csv(Intrusiondf, IntrusionFile,row.names = F)

Emotiondf <- read.csv(EmotionFile)
Emotiondf$outliers <- ifelse(Emotiondf$EffectSizeID %in% EmotionOutID, 1,0)
write.csv(Emotiondf, EmotionFile,row.names = F)

Symptomsdf <- read.csv(SymptomsFile)
Symptomsdf$outliers <- ifelse(Symptomsdf$EffectSizeID %in% SymptomsOutID, 1,0)
write.csv(Symptomsdf, SymptomsFile,row.names = F)

Diarydf <- read.csv(DiaryFile)
Diarydf$outliers <- ifelse(Diarydf$EffectSizeID %in% DiaryOutID, 1,0)
write.csv(Diarydf, DiaryFile,row.names = F)

Questionnairedf <- read.csv(QuestionnaireFile)
Questionnairedf$outliers <- ifelse(Questionnairedf$EffectSizeID %in% QuestionnaireOutID, 1,0)
write.csv(Questionnairedf, QuestionnaireFile,row.names = F)

Taskdf <- read.csv(TaskFile)
Taskdf$outliers <- ifelse(Taskdf$EffectSizeID %in% TaskOutID, 1,0)
write.csv(Taskdf, TaskFile,row.names = F)