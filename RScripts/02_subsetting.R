work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)

options(digits=5)

# Import csv files
dat <- read.csv("IM_ES_step5_upd.csv")


##remove mismatch rows or rows with t-score direction not known and null rows at the end
dat <- dat[-c(189, 190, 406, 409, 410, 412, 413, 414, 675, 676, 677, 678), ]

##ensure values in moderator columns are consistent and to lowercase
dat$Level2_final = tolower(dat$Level2_final)
dat$Level3_Type_final = tolower(dat$Level3_Type_final)
dat$Level3_Task_Instruction_final = tolower(dat$Level3_Task_Instruction_final)
dat$TimeOfManipulation = tolower(dat$TimeOfManipulation)
dat$StudyDesign = tolower(dat$StudyDesign)
dat$DependentVariablesType = tolower(dat$DependentVariablesType)
dat$ModeOfMeasurement = tolower(dat$ModeOfMeasurement)
dat$Physio_binary = tolower(dat$Physio_binary)
dat$TraumaStimuliType = tolower(dat$TraumaStimuliType)
dat$Continent = tolower(dat$Continent)
dat$Intrusion_Predicted_Direction = tolower(dat$Intrusion_Predicted_Direction)
dat$ComparisonType = tolower(dat$ComparisonType)
dat$Intrusion_Retro_or_RealTime = tolower(dat$Intrusion_Retro_or_RealTime)
dat$VoluntaryMemoryMeasure = tolower(dat$VoluntaryMemoryMeasure)
dat$VoluntaryMemoryDirection = tolower(dat$VoluntaryMemoryDirection)
dat$StudyType = tolower(dat$StudyType)
dat$CountryofOrigin = tolower(dat$CountryofOrigin)
dat$Level3_Beh_Pharma_Neuro = tolower(dat$Level3_Beh_Pharma_Neuro)
dat$Level3_Direct_Indirect = tolower(dat$Level3_Direct_Indirect)
dat$Level3_Imagery_Verbal_Emotion_Other = tolower(dat$Level3_Imagery_Verbal_Emotion_Other)
dat$Level3_Task_Instruction = tolower(dat$Level3_Task_Instruction)

write.csv(dat, "IM_ES_Step5a_MISMATCHROWS_REMOVED.csv") #save for record

##STEP 6 SUBSETTING BASED ON INTRUSION PREDICTED DIRECTION (step counting based on previous script)
#subset based on col Intrusion_Predicted_Direction (Decrease, Increase, Unspecified)
IM_ES_Step6_INTINC <- dat[ which(dat$Intrusion_Predicted_Direction =='increase'), ]
write.csv(IM_ES_Step6_INTINC, "IM_ES_Step6_INTINC.csv")

IM_ES_Step6_INTDEC <- dat[ which(dat$Intrusion_Predicted_Direction =='decrease'), ]
write.csv(IM_ES_Step6_INTDEC, "IM_ES_Step6_INTDEC.csv")

IM_ES_Step6_INTUNSP <- dat[ which(dat$Intrusion_Predicted_Direction =='unspecified'), ]
write.csv(IM_ES_Step6_INTUNSP, "IM_ES_Step6_INTUNSP.csv")


##rename the files into short forms
dat1a = IM_ES_Step6_INTINC
dat1b = IM_ES_Step6_INTDEC
dat1c = IM_ES_Step6_INTUNSP


##STEP 7a SUBSETTING BASED ON DependentVariableType for INTRUSION_INCREASE
#subset based on col DependentVariableType (Involuntary memory, emotion, intrusion symptoms)
IM_ES_Step7_INTINC_INTRUSION <- dat1a[ which(dat1a$DependentVariablesType =='involuntary memory'), ]
write.csv(IM_ES_Step7_INTINC_INTRUSION, "IM_ES_Step7_INTINC_INTRUSION.csv")

IM_ES_Step7_INTINC_EMOTION <- dat1a[ which(dat1a$DependentVariablesType =='emotion'), ]
write.csv(IM_ES_Step7_INTINC_EMOTION, "IM_ES_Step7_INTINC_EMOTION.csv")

IM_ES_Step7_INTINC_SYMPTOMS <- dat1a[ which(dat1a$DependentVariablesType =='intrusion symptoms'), ]
write.csv(IM_ES_Step7_INTINC_SYMPTOMS, "IM_ES_Step7_INTINC_SYMPTOMS.csv")


##STEP 7b SUBSETTING BASED ON DependentVariableType for INTRUSION_DECREASE
#subset based on col DependentVariableType (Involuntary memory, emotion, intrusion symptoms)
IM_ES_Step7_INTDEC_INTRUSION <- dat1b[ which(dat1b$DependentVariablesType =='involuntary memory'), ]
write.csv(IM_ES_Step7_INTDEC_INTRUSION, "IM_ES_Step7_INTDEC_INTRUSION.csv")

IM_ES_Step7_INTDEC_EMOTION <- dat1b[ which(dat1b$DependentVariablesType =='emotion'), ]
write.csv(IM_ES_Step7_INTDEC_EMOTION, "IM_ES_Step7_INTDEC_EMOTION.csv")

IM_ES_Step7_INTDEC_SYMPTOMS <- dat1b[ which(dat1b$DependentVariablesType =='intrusion symptoms'), ]
write.csv(IM_ES_Step7_INTDEC_SYMPTOMS, "IM_ES_Step7_INTDEC_SYMPTOMS.csv")


##STEP 7c SUBSETTING BASED ON DependentVariableType for INTRUSION_UNSPECIFIED
#subset based on col DependentVariableType (Involuntary memory, emotion, intrusion symptoms)
IM_ES_Step7_INTUNSP_INTRUSION <- dat1c[ which(dat1c$DependentVariablesType =='involuntary memory'), ]
write.csv(IM_ES_Step7_INTUNSP_INTRUSION, "IM_ES_Step7_INTUNSP_INTRUSION.csv")

IM_ES_Step7_INTUNSP_EMOTION <- dat1c[ which(dat1c$DependentVariablesType =='emotion'), ]
write.csv(IM_ES_Step7_INTUNSP_EMOTION, "IM_ES_Step7_INTUNSP_EMOTION.csv")

IM_ES_Step7_INTUNSP_SYMPTOMS <- dat1c[ which(dat1c$DependentVariablesType =='intrusion symptoms'), ]
write.csv(IM_ES_Step7_INTUNSP_SYMPTOMS, "IM_ES_Step7_INTUNSP_SYMPTOMS.csv")


##rename the files into short forms
dat2a = IM_ES_Step7_INTINC_INTRUSION
dat2b = IM_ES_Step7_INTDEC_INTRUSION
dat2c = IM_ES_Step7_INTUNSP_INTRUSION


##STEP 8a SUBSETTING BASED ON INTRUSION MEASUREMENT TYPE for INTRUSION_INCREASE
#subset based on col ModeOfMeasurement (intrusion diary, lab-based task, self-report questionnaire)
IM_ES_Step8_INTINC_INTRUSION_DIARY <- dat2a[ which(dat2a$ModeOfMeasurement =='intrusion diary'), ]
write.csv(IM_ES_Step8_INTINC_INTRUSION_DIARY, "IM_ES_Step8_INTINC_INTRUSION_DIARY.csv")

IM_ES_Step8_INTINC_INTRUSION_TASK <- dat2a[ which(dat2a$ModeOfMeasurement =='lab-based intrusion monitoring task'), ]
write.csv(IM_ES_Step8_INTINC_INTRUSION_TASK, "IM_ES_Step8_INTINC_INTRUSION_TASK.csv")

IM_ES_Step8_INTINC_INTRUSION_QUESTIONNAIRE <- dat2a[ which(dat2a$ModeOfMeasurement =='self-report questionnaire'), ]
write.csv(IM_ES_Step8_INTINC_INTRUSION_QUESTIONNAIRE, "IM_ES_Step8_INTINC_INTRUSION_QUESTIONNAIRE.csv")


##STEP 8b SUBSETTING BASED ON INTRUSION MEASUREMENT TYPE for INTRUSION_DECREASE
#subset based on col ModeOfMeasurement (intrusion diary, lab-based task, self-report questionnaire)
IM_ES_Step8_INTDEC_INTRUSION_DIARY <- dat2b[ which(dat2b$ModeOfMeasurement =='intrusion diary'), ]
write.csv(IM_ES_Step8_INTDEC_INTRUSION_DIARY, "IM_ES_Step8_INTDEC_INTRUSION_DIARY.csv")

IM_ES_Step8_INTDEC_INTRUSION_TASK <- dat2b[ which(dat2b$ModeOfMeasurement =='lab-based intrusion monitoring task'), ]
write.csv(IM_ES_Step8_INTDEC_INTRUSION_TASK, "IM_ES_Step8_INTDEC_INTRUSION_TASK.csv")

IM_ES_Step8_INTDEC_INTRUSION_QUESTIONNAIRE <- dat2b[ which(dat2b$ModeOfMeasurement =='self-report questionnaire'), ]
write.csv(IM_ES_Step8_INTDEC_INTRUSION_QUESTIONNAIRE, "IM_ES_Step8_INTDEC_INTRUSION_QUESTIONNAIRE.csv")


##STEP 8c SUBSETTING BASED ON INTRUSION MEASUREMENT TYPE for INTRUSION_UNSPECIFIED
#subset based on col ModeOfMeasurement (intrusion diary, lab-based task, self-report questionnaire)
IM_ES_Step8_INTUNSP_INTRUSION_DIARY <- dat2c[ which(dat2c$ModeOfMeasurement =='intrusion diary'), ]
write.csv(IM_ES_Step8_INTUNSP_INTRUSION_DIARY, "IM_ES_Step8_INTUNSP_INTRUSION_DIARY.csv")

IM_ES_Step8_INTUNSP_INTRUSION_TASK <- dat2c[ which(dat2c$ModeOfMeasurement =='lab-based intrusion monitoring task'), ]
write.csv(IM_ES_Step8_INTUNSP_INTRUSION_TASK, "IM_ES_Step8_INTUNSP_INTRUSION_TASK.csv")

IM_ES_Step8_INTUNSP_INTRUSION_QUESTIONNAIRE <- dat2c[ which(dat2c$ModeOfMeasurement =='self-report questionnaire'), ]
write.csv(IM_ES_Step8_INTUNSP_INTRUSION_QUESTIONNAIRE, "IM_ES_Step8_INTUNSP_INTRUSION_QUESTIONNAIRE.csv")


##STEP 9a SUBSETTING BASED ON TIME OF MEASUREMENT for INTRUSION_INCREASE 
#subset based on col TimeOfManipulation (Pre, Peri, immediate post, delayed post)
IM_ES_Step9_INTINC_PRE <- dat1a[ which(dat1a$TimeOfManipulation =='pre'), ]
write.csv(IM_ES_Step9_INTINC_PRE, "IM_ES_Step9_INTINC_PRE.csv")

IM_ES_Step9_INTINC_PERI <- dat1a[ which(dat1a$TimeOfManipulation =='peri'), ]
write.csv(IM_ES_Step9_INTINC_PERI, "IM_ES_Step9_INTINC_PERI.csv")

IM_ES_Step9_INTINC_IPOST <- dat1a[ which(dat1a$TimeOfManipulation =='immediate post'), ]
write.csv(IM_ES_Step9_INTINC_IPOST, "IM_ES_Step9_INTINC_IPOST.csv")

IM_ES_Step9_INTINC_DPOST <- dat1a[ which(dat1a$TimeOfManipulation =='delayed post'), ]
write.csv(IM_ES_Step9_INTINC_DPOST, "IM_ES_Step9_INTINC_DPOST.csv")

IM_ES_Step9_INTINC_IDPOST <- dat1a[ which(dat1a$TimeOfManipulation =='immediate post + delayed post'), ]
write.csv(IM_ES_Step9_INTINC_IDPOST, "IM_ES_Step9_INTINC_I+DPOST.csv")

IM_ES_Step9_INTINC_PERI_IPOST <- dat1a[ which(dat1a$TimeOfManipulation =='peri + immediate post'), ]
write.csv(IM_ES_Step9_INTINC_PERI_IPOST, "IM_ES_Step9_INTINC_PERI+IPOST.csv")

IM_ES_Step9_INTINC_PRE_IPOST <- dat1a[ which(dat1a$TimeOfManipulation =='pre + immediate post'), ]
write.csv(IM_ES_Step9_INTINC_PRE_IPOST, "IM_ES_Step9_INTINC_PRE+IPOST.csv")


##STEP 9b SUBSETTING BASED ON TIME OF MEASUREMENT for INTRUSION_DECREASE
#subset based on col TimeOfManipulation (Pre, Peri, immediate post, delayed post)
IM_ES_Step9_INTDEC_PRE <- dat1b[ which(dat1b$TimeOfManipulation =='pre'), ]
write.csv(IM_ES_Step9_INTDEC_PRE, "IM_ES_Step9_INTDEC_PRE.csv")

IM_ES_Step9_INTDEC_PERI <- dat1b[ which(dat1b$TimeOfManipulation =='peri'), ]
write.csv(IM_ES_Step9_INTDEC_PERI, "IM_ES_Step9_INTDEC_PERI.csv")

IM_ES_Step9_INTDEC_IPOST <- dat1b[ which(dat1b$TimeOfManipulation =='immediate post'), ]
write.csv(IM_ES_Step9_INTDEC_IPOST, "IM_ES_Step9_INTDEC_IPOST.csv")

IM_ES_Step9_INTDEC_DPOST <- dat1b[ which(dat1b$TimeOfManipulation =='delayed post'), ]
write.csv(IM_ES_Step9_INTDEC_DPOST, "IM_ES_Step9_INTDEC_DPOST.csv")

IM_ES_Step9_INTDEC_IDPOST <- dat1b[ which(dat1b$TimeOfManipulation =='immediate post + delayed post'), ]
write.csv(IM_ES_Step9_INTDEC_IDPOST, "IM_ES_Step9_INTDEC_I+DPOST.csv")

IM_ES_Step9_INTDEC_PERI_IPOST <- dat1b[ which(dat1b$TimeOfManipulation =='peri + immediate post'), ]
write.csv(IM_ES_Step9_INTDEC_PERI_IPOST, "IM_ES_Step9_INTDEC_PERI+IPOST.csv")

IM_ES_Step9_INTDEC_PRE_IPOST <- dat1b[ which(dat1b$TimeOfManipulation =='pre + immediate post'), ]
write.csv(IM_ES_Step9_INTDEC_PRE_IPOST, "IM_ES_Step9_INTDEC_PRE+IPOST.csv")


##STEP 9c SUBSETTING BASED ON TIME OF MEASUREMENT for INTRUSION_UNSPECIFIED
#subset based on col TimeOfManipulation (Pre, Peri, immediate post, delayed post)
IM_ES_Step9_INTUNSP_PRE <- dat1c[ which(dat1c$TimeOfManipulation =='pre'), ]
write.csv(IM_ES_Step9_INTUNSP_PRE, "IM_ES_Step9_INTUNSP_PRE.csv")

IM_ES_Step9_INTUNSP_PERI <- dat1c[ which(dat1c$TimeOfManipulation =='peri'), ]
write.csv(IM_ES_Step9_INTUNSP_PERI, "IM_ES_Step9_INTUNSP_PERI.csv")

IM_ES_Step9_INTUNSP_IPOST <- dat1c[ which(dat1c$TimeOfManipulation =='immediate post'), ]
write.csv(IM_ES_Step9_INTUNSP_IPOST, "IM_ES_Step9_INTUNSP_IPOST.csv")

IM_ES_Step9_INTUNSP_DPOST <- dat1c[ which(dat1c$TimeOfManipulation =='delayed post'), ]
write.csv(IM_ES_Step9_INTUNSP_DPOST, "IM_ES_Step9_INTUNSP_DPOST.csv")

IM_ES_Step9_INTUNSP_IDPOST <- dat1c[ which(dat1c$TimeOfManipulation =='immediate post + delayed post'), ]
write.csv(IM_ES_Step9_INTUNSP_IDPOST, "IM_ES_Step9_INTUNSP_I+DPOST.csv")

IM_ES_Step9_INTUNSP_PERI_IPOST <- dat1c[ which(dat1c$TimeOfManipulation =='peri + immediate post'), ]
write.csv(IM_ES_Step9_INTUNSP_PERI_IPOST, "IM_ES_Step9_INTUNSP_PERI+IPOST.csv")

IM_ES_Step9_INTUNSP_PRE_IPOST <- dat1c[ which(dat1c$TimeOfManipulation =='pre + immediate post'), ]
write.csv(IM_ES_Step9_INTUNSP_PRE_IPOST, "IM_ES_Step9_INTUNSP_PRE+IPOST.csv")


#Step10 SUBSETTING FOR FOCAL ANALYSIS (ALL DIRECTIONS INCLUDED)
#subset based on col DependentVariableType (Involuntary memory, emotion, intrusion symptoms)
IM_ES_Step10_INTALL_INTRUSION <- dat[ which(dat$DependentVariablesType =='involuntary memory'), ]
write.csv(IM_ES_Step10_INTALL_INTRUSION, "IM_ES_Step10_INTALL_INTRUSION.csv")
dat3 = IM_ES_Step10_INTALL_INTRUSION

IM_ES_Step10_INTIALL_EMOTION <- dat[ which(dat$DependentVariablesType =='emotion'), ]
write.csv(IM_ES_Step10_INTIALL_EMOTION, "IM_ES_Step10_INTALL_EMOTION.csv")

IM_ES_Step10_INTALL_SYMPTOMS <- dat[ which(dat$DependentVariablesType =='intrusion symptoms'), ]
write.csv(IM_ES_Step10_INTALL_SYMPTOMS, "IM_ES_Step10_INTALL_SYMPTOMS.csv")

##STEP11 SUBSETTING FOR FOCAL ANALYSIS (ALL DIRECTIONS INCLUDED), 
#SUBSETTING BASED ON INTRUSION MEASUREMENT TYPE
#subset based on col ModeOfMeasurement (intrusion diary, lab-based task, self-report questionnaire)
IM_ES_Step11_INTALL_INTRUSION_DIARY <- dat3[ which(dat3$ModeOfMeasurement =='intrusion diary'), ]
write.csv(IM_ES_Step11_INTALL_INTRUSION_DIARY, "IM_ES_Step11_INTALL_INTRUSION_DIARY.csv")

IM_ES_Step11_INTALL_INTRUSION_TASK <- dat3[ which(dat3$ModeOfMeasurement =='lab-based intrusion monitoring task'), ]
write.csv(IM_ES_Step11_INTALL_INTRUSION_TASK, "IM_ES_Step11_INTALL_INTRUSION_TASK.csv")

IM_ES_Step11_INTALL_INTRUSION_QUESTIONNAIRE <- dat3[ which(dat3$ModeOfMeasurement =='self-report questionnaire'), ]
write.csv(IM_ES_Step11_INTALL_INTRUSION_QUESTIONNAIRE, "IM_ES_Step11_INTALL_INTRUSION_QUESTIONNAIRE.csv")

