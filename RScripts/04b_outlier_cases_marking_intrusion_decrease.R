rm(list = ls())
# Based on the outlier results of DV and measurement mode outlier results respectively from script 03,
# mark down these outliers to the data file

work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)
options(scipen=999)

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


# The file path that contains the outlier results
IntrusionOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_INTRUSION.csv"
EmotionOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_EMOTION.csv"
SymptomsOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_SYMPTOMS.csv"
DiaryOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_IDIARY.csv"
QuestionnaireOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_IQUESTIONNAIRE.csv"
TaskOutFile <- "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_ITASK.csv"
DPostOutFile = "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_DPOST.csv"
IDPostOutFile = "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_I+DPOST.csv"
IPostOutFile = "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_IPOST.csv"
PeriOutFile = "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PERI.csv"
PeriIPostOutFile = "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PERI+IPOST.csv"
PreOutFile = "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PRE.csv"
PreIPostOutFile = "OutlierInfluential/Results_Studentized_Outliers_in_INTDEC_PRE+IPOST.csv"

##IMPORTANT: PeriIPostOutFile, PreIPostOutFile HAVE EMPTY DATASET SO NO NEED TO RUN THEM.


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

DPostOutDf <- read.csv(DPostOutFile)
DPostOutID <- DPostOutDf$EffectSizeID

#IDPostOutDf <- read.csv(IDPostOutFile)
#IDPostOutID <- IDPostOutDf$EffectSizeID

IPostOutDf <- read.csv(IPostOutFile)
IPostOutID <- IPostOutDf$EffectSizeID

PeriOutDf <- read.csv(PeriOutFile)
PeriOutID <- PeriOutDf$EffectSizeID

PeriIPostOutDf <- read.csv(PeriIPostOutFile)
PeriIPostOutID <- PeriIPostOutDf$EffectSizeID

PreOutDf <- read.csv(PreOutFile)
PreOutID <- PreOutDf$EffectSizeID

#PreIPostOutDf <- read.csv(PreIPostOutFile)
#PreIPostOutID <- PreIPostOutDf$EffectSizeID

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

DPostdf <- read.csv(DPostFile)
DPostdf$outliers <- ifelse(DPostdf$EffectSizeID %in% DPostOutID, 1,0)
write.csv(DPostdf, DPostFile,row.names = F)

#IDPostdf <- read.csv(IDPostFile)
#IDPostdf$outliers <- ifelse(IDPostdf$EffectSizeID %in% IDPostOutID, 1,0)
#write.csv(IDPostdf, IDPostFile,row.names = F)

IPostdf <- read.csv(IPostFile)
IPostdf$outliers <- ifelse(IPostdf$EffectSizeID %in% IPostOutID, 1,0)
write.csv(IPostdf, IPostFile,row.names = F)

Peridf <- read.csv(PeriFile)
Peridf$outliers <- ifelse(Peridf$EffectSizeID %in% PeriOutID, 1,0)
write.csv(Peridf, PeriFile,row.names = F)

PeriIPostdf <- read.csv(PeriIPostFile)
PeriIPostdf$outliers <- ifelse(PeriIPostdf$EffectSizeID %in% PeriIPostOutID, 1,0)
write.csv(PeriIPostdf, PeriIPostFile,row.names = F)

Predf <- read.csv(PreFile)
Predf$outliers <- ifelse(Predf$EffectSizeID %in% PreOutID, 1,0)
write.csv(Predf, PreFile,row.names = F)

#PreIPostdf <- read.csv(PreIPostFile)
#PreIPostdf$outliers <- ifelse(PreIPostdf$EffectSizeID %in% PreIPostOutID, 1,0)
#write.csv(PreIPostdf, PreIPostFile,row.names = F)
