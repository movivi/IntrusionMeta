rm(list = ls())

library(metafor)
library(papaja)

######################
#     User Defined   #
######################
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

#returns a dataframe which containing variables of a single moderator analysis - Q, tau2, I2
#modMain: 1 = main analysis, 2 = moderator analysis
getTable_Mod1 <- function(analysisName,modFactor,modMain) {
  if (modMain == 1){
    metaObject = rma.mv(yi=yi, V = vi, data=df, random = ~ 1 |  StudyID/EffectSizeID)
  } else{
    #metaObject = rma.mv(yi=Hedge.g, V = Variances, data=df, mods = ~factor(modFactor), random = ~ 1 | StudyID/Outcome)
    metaObject = rma.mv(yi=yi, V = vi, data=df, mods = ~factor(modFactor), random = ~ 1 |  StudyID/EffectSizeID, control = list(optimizer="optim"))
  }
  
  currentdf=getTemplate()
  currentdf[1,"Analysis"] = analysisName
  currentdf[1,"QM"] = formatC(metaObject$QM,3,format="f")
  currentdf[1,"p"] = formatC(metaObject$QMp,3,format="f")
  
  #without reference level
  meta_mod = rma.mv(yi=yi, V = vi, data=df, mods = ~factor(modFactor)-1, random = ~ 1 |  StudyID/EffectSizeID, control = list(optimizer="optim"))
  
  print(meta_mod)
  for (i in 1:nlevels(modFactor)){
    currentdf[1+i,"Analysis"] = levels(modFactor)[i]
    #create a subset with only the current level of modFactor
    tempdf = subset(df,modFactor == levels(modFactor)[i])
    #Calculate sample size by summing the sample size of each StudyID
    sampleSize = 0
    for (thisID in unique(tempdf$StudyID)){
      thisSampleSize = subset(tempdf,StudyID == thisID)$SampleSize[1]
      sampleSize = sampleSize + thisSampleSize
    }
    
    currentdf[1+i,"N"] = sampleSize
    currentdf[1+i,"n"] = length(unique(tempdf$StudyID))
    currentdf[1+i,"k"] = as.character(nrow(tempdf))
    currentdf[1+i,"modES"] = formatC(meta_mod$beta[i],2,format="f")
    cilb = formatC(meta_mod$ci.lb[i],2,format="f")
    ciub = formatC(meta_mod$ci.ub[i],2,format="f")
    currentdf[1+i,"CI"] = paste(" [",cilb,", ",ciub,"]",sep="")
    currentdf[1+i,"Z"] = formatC(meta_mod$zval[i],6,format="f")
    currentdf[1+i,"p"] = formatC(meta_mod$pval[i],6,format="f")
    
  } 
  
  names(currentdf)<-c("Analysis","N","n_study","k","Hedge's g","95% CI","Q","Z","p")
  
  return(currentdf)
}

getTable_Mod <- function(analysisName,modFactor,modMain) {
  if (modMain == 1){
    metaObject = rma.mv(yi=yi, V = vi, data=df, random = ~ 1 |  StudyID/EffectSizeID)
  } else{
    #metaObject = rma.mv(yi=Hedge.g, V = Variances, data=df, mods = ~factor(modFactor), random = ~ 1 | StudyID/Outcome)
    metaObject = rma.mv(yi=yi, V = vi, data=df, mods = ~factor(modFactor), random = ~ 1 |  StudyID/EffectSizeID)
  }
  
  currentdf=getTemplate()
  currentdf[1,"Analysis"] = analysisName
  currentdf[1,"QM"] = formatC(metaObject$QM,3,format="f")
  currentdf[1,"p"] = formatC(metaObject$QMp,3,format="f")
  
  #without reference level
  meta_mod = rma.mv(yi=yi, V = vi, data=df, mods = ~factor(modFactor)-1, random = ~ 1 |  StudyID/EffectSizeID)
  
  print(meta_mod)
  for (i in 1:nlevels(modFactor)){
    currentdf[1+i,"Analysis"] = levels(modFactor)[i]
    #create a subset with only the current level of modFactor
    tempdf = subset(df,modFactor == levels(modFactor)[i])
    #Calculate sample size by summing the sample size of each StudyID
    sampleSize = 0
    for (thisID in unique(tempdf$StudyID)){
      thisSampleSize = subset(tempdf,StudyID == thisID)$SampleSize[1]
      sampleSize = sampleSize + thisSampleSize
    }
    
    currentdf[1+i,"N"] = sampleSize
    currentdf[1+i,"n"] = length(unique(tempdf$StudyID))
    currentdf[1+i,"k"] = as.character(nrow(tempdf))
    currentdf[1+i,"modES"] = formatC(meta_mod$beta[i],2,format="f")
    cilb = formatC(meta_mod$ci.lb[i],2,format="f")
    ciub = formatC(meta_mod$ci.ub[i],2,format="f")
    currentdf[1+i,"CI"] = paste(" [",cilb,", ",ciub,"]",sep="")
    currentdf[1+i,"Z"] = formatC(meta_mod$zval[i],6,format="f")
    currentdf[1+i,"p"] = formatC(meta_mod$pval[i],6,format="f")
    
  } 
  
  names(currentdf)<-c("Analysis","N","n_study","k","Hedge's g","95% CI","Q","Z","p")
  
  return(currentdf)
}

#returns a template of df to store the results of all moderator analyses
getTemplate <- function(){
  df_test = data.frame(
    Analysis = character(),
    N = character(),
    n = character(),
    k = character(),
    modES = character(),
    CI = character(),
    QM = character(),
    Z = character(),
    p = character(),
    
    stringsAsFactors = FALSE
  )
  
  return(df_test)
}

allTable = getTemplate()


##############################
# INTRUSION DECREASE SUBSET      #
##############################
allTable = getTemplate()

###Analysis for Intrusion All 
df = read.csv(IntrusionFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#Level 2 category
df$Level2_final = factor(df$Level2_final)
a = getTable_Mod("Moderation: INTDEC LEVEL 2 CATEGORY",df$Level2_final,2)
allTable = rbind(allTable,a)

#Level 3 beh/neuro/pharma
df$Level3_Beh_Pharma_Neuro = factor(df$Level3_Beh_Pharma_Neuro)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 BEH/NEURO/PHARMA",df$Level3_Beh_Pharma_Neuro,2)
allTable = rbind(allTable,a)

#Level 3 category
df$Level3_Imagery_Verbal_Emotion_Other = factor(df$Level3_Imagery_Verbal_Emotion_Other)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 CATEGORY",df$Level3_Imagery_Verbal_Emotion_Other,2)
allTable = rbind(allTable,a)

#Level 3 DIRECT/INDIRECT
df$Level3_Direct_Indirect = factor(df$Level3_Direct_Indirect)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 DIRECT/INDIRECT",df$Level3_Direct_Indirect,2)
allTable = rbind(allTable,a)

#Level 3 Task/Instruction
df$Level3_Task_Instruction = factor(df$Level3_Task_Instruction)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 task/instruction",df$Level3_Task_Instruction,2)
allTable = rbind(allTable,a)

#TraumaInductionStimuli
df$TraumaStimuliType = factor(df$TraumaStimuliType)
a = getTable_Mod("Moderation: INTDEC Trauma induction stimuli",df$TraumaStimuliType,2)
allTable = rbind(allTable,a)

#Control condition nature (active/experimental vs. passive/no-task)
df$ComparisonType = as.factor(df$ComparisonType)
a = getTable_Mod("Moderation: INTDEC Comparison Type Active VS. Passive",df$ComparisonType,2)
allTable = rbind(allTable,a)

#study design (between vs. within)
df$StudyDesign = as.factor(df$StudyDesign)
a = getTable_Mod("Moderation: INTDEC Study design Between vs. Within",df$StudyDesign,2)
allTable = rbind(allTable,a)

#intrusion measurement mode
df$ModeOfMeasurement = as.factor(df$ModeOfMeasurement)
a = getTable_Mod("Moderation: INTDEC ModeOfMeasurement",df$ModeOfMeasurement,2)
allTable = rbind(allTable,a)

#neuroimaging/psychophysiological measurement yes vs. no
df$Physio_binary = as.factor(df$Physio_binary)
a = getTable_Mod("Moderation: INTDEC PhysioMeasures",df$Physio_binary,2)
allTable = rbind(allTable,a)

#publication status
df$PublicationStatus = as.factor(df$PublicationStatus)
a = getTable_Mod("Moderation: INTDEC PublicationStatus",df$PublicationStatus,2)
allTable = rbind(allTable,a)

#age
df$MeanAge[df$MeanAge == "Not Reported"] <- NA
df$MeanAge[df$MeanAge == "Not reported"] <- NA
df$MeanAge[df$MeanAge == "Not Collected"] <- NA
df$MeanAge[df$MeanAge == "Not provided"] <- NA
df1 = df[!is.na(df$MeanAge), ]
df1$MeanAge = as.numeric(df1$MeanAge)
b = rma.mv(yi=yi, V = vi, data=df1, slab=StudyID,mods=df1$MeanAge,random = ~ 1 | StudyID/EffectSizeID)
r = c("AGE","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#female ratio
df$FemaleRatio[df$FemaleRatio == "Not Reported"] <- NA
df2 = df[!is.na(df$FemaleRatio), ]
df2$FemaleRatio = as.numeric(df2$FemaleRatio)
b = rma.mv(yi=yi, V = vi, data=df2, slab=StudyID,mods=df2$FemaleRatio,random = ~ 1 | StudyID/EffectSizeID)
r = c("FEMALE RATIO","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#country
df$CountryofOrigin = as.factor(df$CountryofOrigin)
a = getTable_Mod("Moderation: INTDEC Country",df$CountryofOrigin,2)
allTable = rbind(allTable,a)

#continent
df$Continent = as.factor(df$Continent)
a = getTable_Mod("Moderation: INTDEC Continent",df$Continent,2)
allTable = rbind(allTable,a)

#total IBA score
df$TotalIBAScore = as.numeric(df$TotalIBAScore)
b = rma.mv(yi=yi, V = vi, data=df, slab=StudyID,mods=df$TotalIBAScore,random = ~ 1 | StudyID/EffectSizeID)
r = c("IBA","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#time of manipulation
df$TimeOfManipulation = as.factor(df$TimeOfManipulation)
a = getTable_Mod("Moderation: INTDEC Time of Manipulation",df$TimeOfManipulation,2)
allTable = rbind(allTable,a)

#write moderation results
allTable <- sapply(allTable, as.character)
allTable[is.na(allTable)] <- ""
write.csv(allTable,"Results_ModeratorAnalysis_INTDEC_INTRUSIONS_ALL_WithOutliers.csv",row.names = F)


################### NON-PREREGISTERED SOM ANALYSES ON INTRUSION FREQUENCY 
################### MEASURED ON DIFFERENT MODES (DIARY VS. TASK VS. QUESTIONNAIRES)
################### EMOTION AND INTRUSION SYMPTOMS OUTCOME

###Analysis for Intrusion Diary
df = read.csv(DiaryFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

rm(allTable) #delete existing table to create a new one for another analysis
allTable = getTemplate()

#Level 2 category
df$Level2_final = factor(df$Level2_final)
a = getTable_Mod("Moderation: INTDEC LEVEL 2 CATEGORY",df$Level2_final,2)
allTable = rbind(allTable,a)

#Level 3 beh/neuro/pharma
df$Level3_Beh_Pharma_Neuro = factor(df$Level3_Beh_Pharma_Neuro)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 BEH/NEURO/PHARMA",df$Level3_Beh_Pharma_Neuro,2)
allTable = rbind(allTable,a)

#Level 3 category
df$Level3_Imagery_Verbal_Emotion_Other = factor(df$Level3_Imagery_Verbal_Emotion_Other)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 CATEGORY",df$Level3_Imagery_Verbal_Emotion_Other,2)
allTable = rbind(allTable,a)

#Level 3 DIRECT/INDIRECT
df$Level3_Direct_Indirect = factor(df$Level3_Direct_Indirect)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 DIRECT/INDIRECT",df$Level3_Direct_Indirect,2)
allTable = rbind(allTable,a)

#Level 3 Task/Instruction
df$Level3_Task_Instruction = factor(df$Level3_Task_Instruction)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 task/instruction",df$Level3_Task_Instruction,2)
allTable = rbind(allTable,a)

#TraumaInductionStimuli
df$TraumaStimuliType = factor(df$TraumaStimuliType)
a = getTable_Mod("Moderation: INTDEC Trauma induction stimuli",df$TraumaStimuliType,2)
allTable = rbind(allTable,a)

#Control condition nature (active/experimental vs. passive/no-task)
df$ComparisonType = as.factor(df$ComparisonType)
a = getTable_Mod("Moderation: INTDEC Comparison Type Active VS. Passive",df$ComparisonType,2)
allTable = rbind(allTable,a)

#study design (between vs. within)
df$StudyDesign = as.factor(df$StudyDesign)
a = getTable_Mod("Moderation: INTDEC Study design Between vs. Within",df$StudyDesign,2)
allTable = rbind(allTable,a)

#intrusion measurement mode THIS MODERATOR NOT VALID FOR THIS ANALYSIS
#df$ModeOfMeasurement = as.factor(df$ModeOfMeasurement)
#a = getTable_Mod("Moderation: INTDEC ModeOfMeasurement",df$ModeOfMeasurement,2)
#allTable = rbind(allTable,a)

#neuroimaging/psychophysiological measurement yes vs. no
df$Physio_binary = as.factor(df$Physio_binary)
a = getTable_Mod("Moderation: INTDEC PhysioMeasures",df$Physio_binary,2)
allTable = rbind(allTable,a)

#publication status
df$PublicationStatus = as.factor(df$PublicationStatus)
a = getTable_Mod("Moderation: INTDEC PublicationStatus",df$PublicationStatus,2)
allTable = rbind(allTable,a)

#age
df$MeanAge[df$MeanAge == "Not Reported"] <- NA
df$MeanAge[df$MeanAge == "Not reported"] <- NA
df$MeanAge[df$MeanAge == "Not Collected"] <- NA
df$MeanAge[df$MeanAge == "Not provided"] <- NA
df1 = df[!is.na(df$MeanAge), ]
df1$MeanAge = as.numeric(df1$MeanAge)
b = rma.mv(yi=yi, V = vi, data=df1, slab=StudyID,mods=df1$MeanAge,random = ~ 1 | StudyID/EffectSizeID)
r = c("AGE","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#female ratio
df$FemaleRatio[df$FemaleRatio == "Not Reported"] <- NA
df2 = df[!is.na(df$FemaleRatio), ]
df2$FemaleRatio = as.numeric(df2$FemaleRatio)
b = rma.mv(yi=yi, V = vi, data=df2, slab=StudyID,mods=df2$FemaleRatio,random = ~ 1 | StudyID/EffectSizeID)
r = c("FEMALE RATIO","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#country
df$CountryofOrigin = as.factor(df$CountryofOrigin)
a = getTable_Mod("Moderation: INTDEC Country",df$CountryofOrigin,2)
allTable = rbind(allTable,a)

#continent
df$Continent = as.factor(df$Continent)
a = getTable_Mod("Moderation: INTDEC Continent",df$Continent,2)
allTable = rbind(allTable,a)

#total IBA score
df$TotalIBAScore = as.numeric(df$TotalIBAScore)
b= rma.mv(yi=yi, V = vi, data=df, slab=StudyID,mods=df$TotalIBAScore,random = ~ 1 | StudyID/EffectSizeID)
r = c("IBA","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#time of manipulation
df$TimeOfManipulation = as.factor(df$TimeOfManipulation)
a = getTable_Mod("Moderation: INTDEC Time of Manipulation",df$TimeOfManipulation,2)
allTable = rbind(allTable,a)

#write moderation results
allTable <- sapply(allTable, as.character)
allTable[is.na(allTable)] <- ""
write.csv(allTable,"Results_ModeratorAnalysis_INTDEC_INTRUSIONS_DIARY_WithOutliers.csv",row.names = F)

######################################################################

###Analysis for Intrusion Task
df = read.csv(TaskFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

rm(allTable) #delete existing table to create a new one for another analysis
allTable = getTemplate()

#Level 2 category
df$Level2_final = factor(df$Level2_final)
a = getTable_Mod("Moderation: INTDEC LEVEL 2 CATEGORY",df$Level2_final,2)
allTable = rbind(allTable,a)

#Level 3 beh/neuro/pharma
df$Level3_Beh_Pharma_Neuro = factor(df$Level3_Beh_Pharma_Neuro)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 BEH/NEURO/PHARMA",df$Level3_Beh_Pharma_Neuro,2)
allTable = rbind(allTable,a)

#Level 3 category
df$Level3_Imagery_Verbal_Emotion_Other = factor(df$Level3_Imagery_Verbal_Emotion_Other)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 CATEGORY",df$Level3_Imagery_Verbal_Emotion_Other,2)
allTable = rbind(allTable,a)

#Level 3 DIRECT/INDIRECT
df$Level3_Direct_Indirect = factor(df$Level3_Direct_Indirect)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 DIRECT/INDIRECT",df$Level3_Direct_Indirect,2)
allTable = rbind(allTable,a)

#Level 3 Task/Instruction
df$Level3_Task_Instruction = factor(df$Level3_Task_Instruction)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 task/instruction",df$Level3_Task_Instruction,2)
allTable = rbind(allTable,a)

#TraumaInductionStimuli
df$TraumaStimuliType = factor(df$TraumaStimuliType)
a = getTable_Mod("Moderation: INTDEC Trauma induction stimuli",df$TraumaStimuliType,2)
allTable = rbind(allTable,a)

#Control condition nature (active/experimental vs. passive/no-task)
df$ComparisonType = as.factor(df$ComparisonType)
a = getTable_Mod("Moderation: INTDEC Comparison Type Active VS. Passive",df$ComparisonType,2)
allTable = rbind(allTable,a)

#study design (between vs. within) THIS MODERATOR NOT VALID FOR THIS ANALYSIS
#df$StudyDesign = as.factor(df$StudyDesign)
#a = getTable_Mod("Moderation: INTDEC Study design Between vs. Within",df$StudyDesign,2)
#allTable = rbind(allTable,a)

#intrusion measurement mode THIS MODERATOR NOT VALID FOR THIS ANALYSIS
#df$ModeOfMeasurement = as.factor(df$ModeOfMeasurement)
#a = getTable_Mod("Moderation: INTDEC ModeOfMeasurement",df$ModeOfMeasurement,2)
#allTable = rbind(allTable,a)

#neuroimaging/psychophysiological measurement yes vs. no
df$Physio_binary = as.factor(df$Physio_binary)
a = getTable_Mod("Moderation: INTDEC PhysioMeasures",df$Physio_binary,2)
allTable = rbind(allTable,a)

#publication status
df$PublicationStatus = as.factor(df$PublicationStatus)
a = getTable_Mod("Moderation: INTDEC PublicationStatus",df$PublicationStatus,2)
allTable = rbind(allTable,a)

#age
df$MeanAge[df$MeanAge == "Not Reported"] <- NA
df$MeanAge[df$MeanAge == "Not reported"] <- NA
df$MeanAge[df$MeanAge == "Not Collected"] <- NA
df$MeanAge[df$MeanAge == "Not provided"] <- NA
df1 = df[!is.na(df$MeanAge), ]
df1$MeanAge = as.numeric(df1$MeanAge)
b = rma.mv(yi=yi, V = vi, data=df1, slab=StudyID,mods=df1$MeanAge,random = ~ 1 | StudyID/EffectSizeID)
r = c("AGE","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#female ratio
df$FemaleRatio[df$FemaleRatio == "Not Reported"] <- NA
df2 = df[!is.na(df$FemaleRatio), ]
df2$FemaleRatio = as.numeric(df2$FemaleRatio)
b = rma.mv(yi=yi, V = vi, data=df2, slab=StudyID,mods=df2$FemaleRatio,random = ~ 1 | StudyID/EffectSizeID)
r = c("FEMALE RATIO","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#country
df$CountryofOrigin = as.factor(df$CountryofOrigin)
a = getTable_Mod("Moderation: INTDEC Country",df$CountryofOrigin,2)
allTable = rbind(allTable,a)

#continent
df$Continent = as.factor(df$Continent)
a = getTable_Mod("Moderation: INTDEC Continent",df$Continent,2)
allTable = rbind(allTable,a)

#total IBA score
df$TotalIBAScore = as.numeric(df$TotalIBAScore)
b = rma.mv(yi=yi, V = vi, data=df, slab=StudyID,mods=df$TotalIBAScore,random = ~ 1 | StudyID/EffectSizeID)
r = c("IBA","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#time of manipulation
df$TimeOfManipulation = as.factor(df$TimeOfManipulation)
a = getTable_Mod("Moderation: INTDEC Time of Manipulation",df$TimeOfManipulation,2)
allTable = rbind(allTable,a)

#write moderation results
allTable <- sapply(allTable, as.character)
allTable[is.na(allTable)] <- ""
write.csv(allTable,"Results_ModeratorAnalysis_INTDEC_INTRUSIONS_TASK_WithOutliers.csv",row.names = F)

#####################################################################

###Analysis for Intrusion Questionnaire
df = read.csv(QuestionnaireFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

rm(allTable) #delete existing table to create a new one for another analysis
allTable = getTemplate()

#Level 2 category
df$Level2_final = factor(df$Level2_final)
a = getTable_Mod("Moderation: INTDEC LEVEL 2 CATEGORY",df$Level2_final,2)
allTable = rbind(allTable,a)

#Level 3 beh/neuro/pharma
df$Level3_Beh_Pharma_Neuro = factor(df$Level3_Beh_Pharma_Neuro)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 BEH/NEURO/PHARMA",df$Level3_Beh_Pharma_Neuro,2)
allTable = rbind(allTable,a)

#Level 3 category
df$Level3_Imagery_Verbal_Emotion_Other = factor(df$Level3_Imagery_Verbal_Emotion_Other)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 CATEGORY",df$Level3_Imagery_Verbal_Emotion_Other,2)
allTable = rbind(allTable,a)

#Level 3 DIRECT/INDIRECT
df$Level3_Direct_Indirect = factor(df$Level3_Direct_Indirect)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 DIRECT/INDIRECT",df$Level3_Direct_Indirect,2)
allTable = rbind(allTable,a)

#Level 3 Task/Instruction
df$Level3_Task_Instruction = factor(df$Level3_Task_Instruction)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 task/instruction",df$Level3_Task_Instruction,2)
allTable = rbind(allTable,a)

#TraumaInductionStimuli
df$TraumaStimuliType = factor(df$TraumaStimuliType)
a = getTable_Mod("Moderation: INTDEC Trauma induction stimuli",df$TraumaStimuliType,2)
allTable = rbind(allTable,a)

#Control condition nature (active/experimental vs. passive/no-task)
df$ComparisonType = as.factor(df$ComparisonType)
a = getTable_Mod("Moderation: INTDEC Comparison Type Active VS. Passive",df$ComparisonType,2)
allTable = rbind(allTable,a)

#study design (between vs. within) THIS MODERATOR NOT VALID FOR THIS ANALYSIS
#df$StudyDesign = as.factor(df$StudyDesign)
#a = getTable_Mod("Moderation: INTDEC Study design Between vs. Within",df$StudyDesign,2)
#allTable = rbind(allTable,a)

#intrusion measurement mode THIS MODERATOR NOT VALID FOR THIS ANALYSIS
#df$ModeOfMeasurement = as.factor(df$ModeOfMeasurement)
#a = getTable_Mod("Moderation: INTDEC ModeOfMeasurement",df$ModeOfMeasurement,2)
#allTable = rbind(allTable,a)

#neuroimaging/psychophysiological measurement yes vs. no
df$Physio_binary = as.factor(df$Physio_binary)
a = getTable_Mod("Moderation: INTDEC PhysioMeasures",df$Physio_binary,2)
allTable = rbind(allTable,a)

#publication status
df$PublicationStatus = as.factor(df$PublicationStatus)
a = getTable_Mod("Moderation: INTDEC PublicationStatus",df$PublicationStatus,2)
allTable = rbind(allTable,a)

#age
df$MeanAge[df$MeanAge == "Not Reported"] <- NA
df$MeanAge[df$MeanAge == "Not reported"] <- NA
df$MeanAge[df$MeanAge == "Not Collected"] <- NA
df$MeanAge[df$MeanAge == "Not provided"] <- NA
df1 = df[!is.na(df$MeanAge), ]
df1$MeanAge = as.numeric(df1$MeanAge)
b = rma.mv(yi=yi, V = vi, data=df1, slab=StudyID,mods=df1$MeanAge,random = ~ 1 | StudyID/EffectSizeID)
r = c("AGE","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#female ratio
df$FemaleRatio[df$FemaleRatio == "Not Reported"] <- NA
df2 = df[!is.na(df$FemaleRatio), ]
df2$FemaleRatio = as.numeric(df2$FemaleRatio)
b = rma.mv(yi=yi, V = vi, data=df2, slab=StudyID,mods=df2$FemaleRatio,random = ~ 1 | StudyID/EffectSizeID)
r = c("FEMALE RATIO","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#country
df$CountryofOrigin = as.factor(df$CountryofOrigin)
a = getTable_Mod1("Moderation: INTDEC Country",df$CountryofOrigin,2)
allTable = rbind(allTable,a)

#continent
df$Continent = as.factor(df$Continent)
a = getTable_Mod("Moderation: INTDEC Continent",df$Continent,2)
allTable = rbind(allTable,a)

#total IBA score #MANUAL CHANGES MADE TO CONVERGENCE OPTIMAZATION: https://www.metafor-project.org/doku.php/tips:convergence_problems_rma 
df$TotalIBAScore = as.numeric(df$TotalIBAScore)
b = rma.mv(yi=yi, V = vi, data=df, slab=StudyID,mods=df$TotalIBAScore,random = ~ 1 | StudyID/EffectSizeID, control = list(optimizer="optim"))
r = c("IBA","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#time of manipulation
df$TimeOfManipulation = as.factor(df$TimeOfManipulation)
a = getTable_Mod("Moderation: INTDEC Time of Manipulation",df$TimeOfManipulation,2)
allTable = rbind(allTable,a)

#write moderation results
allTable <- sapply(allTable, as.character)
allTable[is.na(allTable)] <- ""
write.csv(allTable,"Results_ModeratorAnalysis_INTDEC_INTRUSIONS_QUESTIONNAIRE_WithOutliers.csv",row.names = F)

####################################################################

###Analysis for Emotion
df = read.csv(EmotionFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

rm(allTable) #delete existing table to create a new one for another analysis
allTable = getTemplate()

#Level 2 category
df$Level2_final = factor(df$Level2_final)
a = getTable_Mod("Moderation: INTDEC LEVEL 2 CATEGORY",df$Level2_final,2)
allTable = rbind(allTable,a)

#Level 3 beh/neuro/pharma
df$Level3_Beh_Pharma_Neuro = factor(df$Level3_Beh_Pharma_Neuro)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 BEH/NEURO/PHARMA",df$Level3_Beh_Pharma_Neuro,2)
allTable = rbind(allTable,a)

#Level 3 category
df$Level3_Imagery_Verbal_Emotion_Other = factor(df$Level3_Imagery_Verbal_Emotion_Other)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 CATEGORY",df$Level3_Imagery_Verbal_Emotion_Other,2)
allTable = rbind(allTable,a)

#Level 3 DIRECT/INDIRECT
df$Level3_Direct_Indirect = factor(df$Level3_Direct_Indirect)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 DIRECT/INDIRECT",df$Level3_Direct_Indirect,2)
allTable = rbind(allTable,a)

#Level 3 Task/Instruction
df$Level3_Task_Instruction = factor(df$Level3_Task_Instruction)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 task/instruction",df$Level3_Task_Instruction,2)
allTable = rbind(allTable,a)

#TraumaInductionStimuli
df$TraumaStimuliType = factor(df$TraumaStimuliType)
a = getTable_Mod("Moderation: INTDEC Trauma induction stimuli",df$TraumaStimuliType,2)
allTable = rbind(allTable,a)

#Control condition nature (active/experimental vs. passive/no-task)
df$ComparisonType = as.factor(df$ComparisonType)
a = getTable_Mod("Moderation: INTDEC Comparison Type Active VS. Passive",df$ComparisonType,2)
allTable = rbind(allTable,a)

#study design (between vs. within)
df$StudyDesign = as.factor(df$StudyDesign)
a = getTable_Mod("Moderation: INTDEC Study design Between vs. Within",df$StudyDesign,2)
allTable = rbind(allTable,a)

#emotion measurement mode
df$ModeOfMeasurement = as.factor(df$ModeOfMeasurement)
a = getTable_Mod("Moderation: INTDEC ModeOfMeasurement",df$ModeOfMeasurement,2)
allTable = rbind(allTable,a)

#neuroimaging/psychophysiological measurement yes vs. no
df$Physio_binary = as.factor(df$Physio_binary)
a = getTable_Mod("Moderation: INTDEC PhysioMeasures",df$Physio_binary,2)
allTable = rbind(allTable,a)

#publication status
df$PublicationStatus = as.factor(df$PublicationStatus)
a = getTable_Mod("Moderation: INTDEC PublicationStatus",df$PublicationStatus,2)
allTable = rbind(allTable,a)

#age
df$MeanAge[df$MeanAge == "Not Reported"] <- NA
df$MeanAge[df$MeanAge == "Not reported"] <- NA
df$MeanAge[df$MeanAge == "Not Collected"] <- NA
df$MeanAge[df$MeanAge == "Not provided"] <- NA
df1 = df[!is.na(df$MeanAge), ]
df1$MeanAge = as.numeric(df1$MeanAge)
b = rma.mv(yi=yi, V = vi, data=df1, slab=StudyID,mods=df1$MeanAge,random = ~ 1 | StudyID/EffectSizeID)
r = c("AGE","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#female ratio
df$FemaleRatio[df$FemaleRatio == "Not Reported"] <- NA
df2 = df[!is.na(df$FemaleRatio), ]
df2$FemaleRatio = as.numeric(df2$FemaleRatio)
b = rma.mv(yi=yi, V = vi, data=df2, slab=StudyID,mods=df2$FemaleRatio,random = ~ 1 | StudyID/EffectSizeID)
r = c("FEMALE RATIO","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#country
df$CountryofOrigin = as.factor(df$CountryofOrigin)
a = getTable_Mod("Moderation: INTDEC Country",df$CountryofOrigin,2)
allTable = rbind(allTable,a)

#continent
df$Continent = as.factor(df$Continent)
a = getTable_Mod("Moderation: INTDEC Continent",df$Continent,2)
allTable = rbind(allTable,a)

#total IBA score
df$TotalIBAScore = as.numeric(df$TotalIBAScore)
b = rma.mv(yi=yi, V = vi, data=df, slab=StudyID,mods=df$TotalIBAScore,random = ~ 1 | StudyID/EffectSizeID)
r = c("IBA","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#time of manipulation
df$TimeOfManipulation = as.factor(df$TimeOfManipulation)
a = getTable_Mod("Moderation: INTDEC Time of Manipulation",df$TimeOfManipulation,2)
allTable = rbind(allTable,a)

#write moderation results
allTable <- sapply(allTable, as.character)
allTable[is.na(allTable)] <- ""
write.csv(allTable,"Results_ModeratorAnalysis_INTDEC_EMOTION_WithOutliers.csv",row.names = F)

####################################################################

###Analysis for SYMPTOM QUESTIONNAIRE
df = read.csv(SymptomsFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

rm(allTable) #delete existing table to create a new one for another analysis
allTable = getTemplate()

#Level 2 category
df$Level2_final = factor(df$Level2_final)
a = getTable_Mod("Moderation: INTDEC LEVEL 2 CATEGORY",df$Level2_final,2)
allTable = rbind(allTable,a)

#Level 3 beh/neuro/pharma
df$Level3_Beh_Pharma_Neuro = factor(df$Level3_Beh_Pharma_Neuro)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 BEH/NEURO/PHARMA",df$Level3_Beh_Pharma_Neuro,2)
allTable = rbind(allTable,a)

#Level 3 category
df$Level3_Imagery_Verbal_Emotion_Other = factor(df$Level3_Imagery_Verbal_Emotion_Other)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 CATEGORY",df$Level3_Imagery_Verbal_Emotion_Other,2)
allTable = rbind(allTable,a)

#Level 3 DIRECT/INDIRECT
df$Level3_Direct_Indirect = factor(df$Level3_Direct_Indirect)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 DIRECT/INDIRECT",df$Level3_Direct_Indirect,2)
allTable = rbind(allTable,a)

#Level 3 Task/Instruction
df$Level3_Task_Instruction = factor(df$Level3_Task_Instruction)
a = getTable_Mod("Moderation: INTDEC LEVEL 3 task/instruction",df$Level3_Task_Instruction,2)
allTable = rbind(allTable,a)

#TraumaInductionStimuli
df$TraumaStimuliType = factor(df$TraumaStimuliType)
a = getTable_Mod("Moderation: INTDEC Trauma induction stimuli",df$TraumaStimuliType,2)
allTable = rbind(allTable,a)

#Control condition nature (active/experimental vs. passive/no-task)
df$ComparisonType = as.factor(df$ComparisonType)
a = getTable_Mod("Moderation: INTDEC Comparison Type Active VS. Passive",df$ComparisonType,2)
allTable = rbind(allTable,a)

#study design (between vs. within)
df$StudyDesign = as.factor(df$StudyDesign)
a = getTable_Mod("Moderation: INTDEC Study design Between vs. Within",df$StudyDesign,2)
allTable = rbind(allTable,a)

#symptoms measurement mode THIS MODERATOR NOT VALID FOR THIS ANALYSIS
#df$ModeOfMeasurement = as.factor(df$ModeOfMeasurement)
#a = getTable_Mod("Moderation: INTDEC ModeOfMeasurement",df$ModeOfMeasurement,2)
#allTable = rbind(allTable,a)

#neuroimaging/psychophysiological measurement yes vs. no
df$Physio_binary = as.factor(df$Physio_binary)
a = getTable_Mod("Moderation: INTDEC PhysioMeasures",df$Physio_binary,2)
allTable = rbind(allTable,a)

#publication status
df$PublicationStatus = as.factor(df$PublicationStatus)
a = getTable_Mod("Moderation: INTDEC PublicationStatus",df$PublicationStatus,2)
allTable = rbind(allTable,a)

#age
df$MeanAge[df$MeanAge == "Not Reported"] <- NA
df$MeanAge[df$MeanAge == "Not reported"] <- NA
df$MeanAge[df$MeanAge == "Not Collected"] <- NA
df$MeanAge[df$MeanAge == "Not provided"] <- NA
df1 = df[!is.na(df$MeanAge), ]
df1$MeanAge = as.numeric(df1$MeanAge)
b = rma.mv(yi=yi, V = vi, data=df1, slab=StudyID,mods=df1$MeanAge,random = ~ 1 | StudyID/EffectSizeID)
r = c("AGE","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#female ratio
df$FemaleRatio[df$FemaleRatio == "Not Reported"] <- NA
df2 = df[!is.na(df$FemaleRatio), ]
df2$FemaleRatio = as.numeric(df2$FemaleRatio)
b = rma.mv(yi=yi, V = vi, data=df2, slab=StudyID,mods=df2$FemaleRatio,random = ~ 1 | StudyID/EffectSizeID)
r = c("FEMALE RATIO","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#country
df$CountryofOrigin = as.factor(df$CountryofOrigin)
a = getTable_Mod1("Moderation: INTDEC Country",df$CountryofOrigin,2)
allTable = rbind(allTable,a)

#continent
df$Continent = as.factor(df$Continent)
a = getTable_Mod("Moderation: INTDEC Continent",df$Continent,2)
allTable = rbind(allTable,a)

#total IBA score
df$TotalIBAScore = as.numeric(df$TotalIBAScore)
b = rma.mv(yi=yi, V = vi, data=df, slab=StudyID,mods=df$TotalIBAScore,random = ~ 1 | StudyID/EffectSizeID)
r = c("IBA","NA",b$s.nlevels[1],b$s.nlevels[2],"NA","NA",b$QM,"NA",b$QMp)
allTable = rbind(allTable,r)

#time of manipulation
df$TimeOfManipulation = as.factor(df$TimeOfManipulation)
a = getTable_Mod("Moderation: INTDEC Time of Manipulation",df$TimeOfManipulation,2)
allTable = rbind(allTable,a)

#write moderation results
allTable <- sapply(allTable, as.character)
allTable[is.na(allTable)] <- ""
write.csv(allTable,"Results_ModeratorAnalysis_INTDEC_SYMPTOMS_WithOutliers.csv",row.names = F)