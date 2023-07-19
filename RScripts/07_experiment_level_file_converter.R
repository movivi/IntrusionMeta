rm(list = ls())

library(metafor)
library(dmetar)


######################
#     User Defined   #
######################
##File Names
IntrusionFile_INC <- "IM_ES_Step7_INTINC_INTRUSION.csv"
EmotionFile_INC <- "IM_ES_Step7_INTINC_EMOTION.csv"
SymptomsFile_INC <- "IM_ES_Step7_INTINC_SYMPTOMS.csv"
DiaryFile_INC <- "IM_ES_Step8_INTINC_INTRUSION_DIARY.csv"
QuestionnaireFile_INC <- "IM_ES_Step8_INTINC_INTRUSION_QUESTIONNAIRE.csv"
TaskFile_INC <- "IM_ES_Step8_INTINC_INTRUSION_TASK.csv"

IntrusionFile_DEC <- "IM_ES_Step7_INTDEC_INTRUSION.csv"
EmotionFile_DEC <- "IM_ES_Step7_INTDEC_EMOTION.csv"
SymptomsFile_DEC <- "IM_ES_Step7_INTDEC_SYMPTOMS.csv"
DiaryFile_DEC <- "IM_ES_Step8_INTDEC_INTRUSION_DIARY.csv"
QuestionnaireFile_DEC <- "IM_ES_Step8_INTDEC_INTRUSION_QUESTIONNAIRE.csv"
TaskFile_DEC <- "IM_ES_Step8_INTDEC_INTRUSION_TASK.csv"

IntrusionFile_UNSP <- "IM_ES_Step7_INTUNSP_INTRUSION.csv"
EmotionFile_UNSP <- "IM_ES_Step7_INTUNSP_EMOTION.csv"
SymptomsFile_UNSP <- "IM_ES_Step7_INTUNSP_SYMPTOMS.csv"
DiaryFile_UNSP <- "IM_ES_Step8_INTUNSP_INTRUSION_DIARY.csv"
QuestionnaireFile_UNSP <- "IM_ES_Step8_INTUNSP_INTRUSION_QUESTIONNAIRE.csv"
TaskFile_UNSP <- "IM_ES_Step8_INTUNSP_INTRUSION_TASK.csv"


###Read file 
df = read.csv(TaskFile_UNSP) ##CHANGE FILE NAME DEPENDING ON WHICH FILE YOU WANT TO CONVERT

ExpConverter <- function(df,file_extension){
  #remove NA in yi column
  df$yi = as.numeric(df$yi)
  df$vi = as.numeric(df$vi)
  df$SE = as.numeric(df$SE)
  df = df[!is.na(df$yi), ]
  # Convert 'df' data to 'escalc' object
  df1 <- escalc(yi = yi, vi = vi, data = df)
  # Aggregate effect sizes on study level
  # We assume a correlation of rho=0.6
  df.agg <- aggregate(df1, cluster =StudyID, rho = 0.6)
  #merge title based on experiment no.
  r = aggregate(Title ~ StudyID, data = df, c)
  #merge author name based on experiment no.
  r1 = aggregate(Author ~ StudyID, data = df, c)
  #merge level2 based on experiment no.
  r2 = aggregate(Level2_final ~ StudyID, data = df, c)
  #merge level3 type based on experiment no.
  r3 = aggregate(Level3_Type_final ~ StudyID, data = df, c)
  #merge level3 task/instruction based on experiment no.
  r4 = aggregate(Level3_Task_Instruction_final ~ StudyID, data = df, c)
  #merge DV TYPE based on experiment no.
  r5 = aggregate(DependentVariablesType ~ StudyID, data = df, c)
  #merge ModeOfMeasurement based on experiment no.
  r6 = aggregate(ModeOfMeasurement ~ StudyID, data = df, c)
  #mean yi based on experiment no.
  #r7 = aggregate(yi ~ StudyID, data = df, mean)
  #mean vi based on experiment no.
  #r8 = aggregate(vi ~ StudyID, data = df, mean)
  #mean se based on experiment no.
  #r9 = aggregate(SE ~ StudyID, data = df, mean)
  
  #yi extracted from df.agg
  yi = df.agg$yi
  #vi extracted from df.agg
  vi = df.agg$vi
  #se extracted from df.agg
  SE = sqrt(df.agg$vi)
  #cbind all columns
  p = cbind(r,r1,r2,r3,r4,r5,r6,yi,vi,SE)
  # Find Unique Column Names
  unique_names <- unique(colnames(p))
  # Keep Only Unique Column Names
  p = p[unique_names]
  #convert list columns to character
  p <- apply(p,2,as.character)
  #write csv
  write.csv(p, paste0("trimfill_", file_extension, ".csv"))
}

ExpConverter(df,"TaskFile_UNSP") ##CHANGE FILE EXTENSION DEPENDING ON WHICH FILE YOU CONVERT
