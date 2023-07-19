rm(list = ls())
library(metafor)
library(weightr)
library(dplyr)

######################
#     User Defined   #
######################
##File Names
#Outcome-level
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

#Experiment-level
ExpIntrusionFile_INC <- "trimfill_IntrusionFile_INC.csv"
ExpEmotionFile_INC <- "trimfill_EmotionFile_INC.csv"
ExpSymptomsFile_INC <- "trimfill_SymptomsFile_INC.csv"
ExpDiaryFile_INC <- "trimfill_DiaryFile_INC.csv"
ExpQuestionnaireFile_INC <- "trimfill_QuestionnaireFile_INC.csv"
ExpTaskFile_INC <- "trimfill_TaskFile_INC.csv"

ExpIntrusionFile_DEC <- "trimfill_IntrusionFile_DEC.csv"
ExpEmotionFile_DEC <- "trimfill_EmotionFile_DEC.csv"
ExpSymptomsFile_DEC <- "trimfill_SymptomsFile_DEC.csv"
ExpDiaryFile_DEC <- "trimfill_DiaryFile_DEC.csv"
ExpQuestionnaireFile_DEC <- "trimfill_QuestionnaireFile_DEC.csv"
ExpTaskFile_DEC <- "trimfill_TaskFile_DEC.csv"

ExpIntrusionFile_UNSP <- "trimfill_IntrusionFile_UNSP.csv"
ExpEmotionFile_UNSP <- "trimfill_EmotionFile_UNSP.csv"
ExpSymptomsFile_UNSP <- "trimfill_SymptomsFile_UNSP.csv"
ExpDiaryFile_UNSP <- "trimfill_DiaryFile_UNSP.csv"
ExpQuestionnaireFile_UNSP <- "trimfill_QuestionnaireFile_UNSP.csv"
ExpTaskFile_UNSP <- "trimfill_TaskFile_UNSP.csv"



dir.create(file.path("PublicationBias"), showWarnings = FALSE)
dir.create(file.path("PublicationBias/INC"), showWarnings = FALSE)
dir.create(file.path("PublicationBias/INC/plots"), showWarnings = FALSE)
dir.create(file.path("PublicationBias/DEC"), showWarnings = FALSE)
dir.create(file.path("PublicationBias/DEC/plots"), showWarnings = FALSE)
dir.create(file.path("PublicationBias/UNSP"), showWarnings = FALSE)
dir.create(file.path("PublicationBias/UNSP/plots"), showWarnings = FALSE)

############################
#   Function of All Tests  #
############################
#Enter a df, export the results as csv of the test

#Egger's test
Egger <- function(df,file_extension,folderName){
  multilevel = TRUE
  if (multilevel){
    #Multilevel model:
    #enter the standard error term as moderator to the multilevel model
    meta <- rma.mv(yi=yi, V = vi,mods=SE, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
    res.df <- data.frame(Analysis = "Egger's test for funnel plot asymmetry",
                         z = meta$zval[2],
                         p = meta$pval[2])
    print(meta)
  } else {
    #Simple random effect model:
    meta <- rma(yi = yi, vi = vi, data = df)
    res <- regtest(meta, model = "rma", predictor="sei" )
    res.df <- data.frame(Analysis = "Egger's test for funnel plot asymmetry",
                         z = res$zval,
                         p = res$pval)
  }
  
  write.csv(res.df,paste("PublicationBias/",folderName,"/Results_EggersTest_",file_extension,".csv",sep=""),row.names = F)
}


#Trim and Fill
Trim_Fill <- function(df,file_extension,folderName){
  meta <- rma(yi = yi, vi = vi, data = df, control=list(optimizer="optim"))
  res <- trimfill(meta)
  print(res)
  res.df <- data.frame(Analysis = "Trim and Fill (Averaged Outcome)",
                       ImputedSide = res$side,
                       ImputedStudies = res$k0,
                       estimate = res$beta,
                       se = res$se,
                       z = res$zval,
                       p = res$pval,
                       ci.lb = res$ci.lb,
                       ci.ub = res$ci.ub)  
  write.csv(res.df,paste("PublicationBias/",folderName,"/Results_TrimFill_",file_extension,".csv",sep=""),row.names = F)
  
  #Export the plots
  #the legend
  a <- expression(paste("0.10 < ",italic("p")," <= 1.00",sep=""))
  b <- expression(paste("0.05 < ",italic("p")," <= 0.10",sep=""))
  c <- expression(paste("0.01< ",italic("p")," <= 0.05",sep=""))
  d <- expression(paste("0.00 < ",italic("p")," <= 0.01",sep=""))
  
  trimmedMeta <- res
  png(paste("PublicationBias/",folderName,"/plots/FunnelPlot_SizeFixed_",file_extension,".png",sep=""), width = 5, height = 5, units = 'in', res = 300)
  metafor::funnel(trimmedMeta , level=c(90, 95, 99),shade=c("white", "gray55", "gray75"), refline=0, legend=FALSE)
  dev.off()
}

#Selection model: three-parameter likelihood model (Iyengar & Greenhouse, 1988)
#one-sided testing with the cutpoint at .025
ThreePSM <- function(df,file_extension,folderName){
  sm <- weightfunct(df$yi,df$vi , steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE)
  print(sm)
  sm.df <- data.frame(Analysis = "3 Parameter Selection Model",
                      estimate = sm$adj_est[[2]], 
                      std.error = sm$adj_se[[2]],
                      z.stat = sm$z_adj[[2]],
                      p.val = sm$p_adj[[2]],
                      ci.lb = sm$ci.lb_adj[[2]],
                      ci.ub = sm$ci.ub_adj[[2]])
  
  write.csv(sm.df,paste("PublicationBias/",folderName,"/Results_3PSM_SelectionModel_",file_extension,".csv",sep=""),row.names = F)
}


#Selection model (Vevea & Woods, 2005)
Vevea_Woods_Selection <- function(df,file_extension,folderName){
  # Define the cut-points
  a <- c(0.005, 0.01, 0.05, 0.10, 0.25, 0.35, 0.50, 
         0.65, 0.75, 0.90, 0.95, 0.99, 0.995, 1)
  
  # Define the selection likelihood for each interval 
  # (moderate/severe selection)
  w.moderate.onetailed <- c(1, 0.99, 0.95, 0.90, 0.80, 0.75, 0.65, 0.60, 
                  0.55, 0.50, 0.50, 0.50, 0.50, 0.50)
  w.severe.onetailed <- c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.40, 0.35, 
                0.30, 0.25, 0.10, 0.10, 0.10, 0.10)
  w.moderate.twotailed <- c(1, 0.99, 0.95, 0.90, 0.80, 0.75, 0.60, 0.60, 
                            0.75, 0.80, 0.90, 0.95, 0.99, 1)
  w.severe.twotailed <- c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.25, 0.25, 
                          0.50, 0.60, 0.75, 0.90, 0.99, 1)
  
  sel <- weightfunct(df$yi, df$vi, steps=a, weights=w.moderate.onetailed)
  adj_est <- c(sel$adj_est[[2]])
  print(sel)
  sel <- weightfunct(df$yi, df$vi, steps=a, weights=w.severe.onetailed)
  adj_est <- c(adj_est, sel$adj_est[[2]])
  print(sel)
  sel <- weightfunct(df$yi, df$vi, steps=a, weights=w.moderate.twotailed)
  adj_est <- c(adj_est, sel$adj_est[[2]])
  print(sel)
  sel <- weightfunct(df$yi, df$vi, steps=a, weights=w.severe.twotailed)
  adj_est <- c(adj_est, sel$adj_est[[2]])
  print(sel)
  selection <- c("Moderate one-tailed selection",	"Severe one-tailed selection",
                 "Moderate two-tailed selection",	"Severe two-tailed selection")
  
  a <- data.frame(Selection = selection, Adjusted.Estimate = adj_est)
  
  write.csv(a, paste("PublicationBias/",folderName,"/Results_Vevea_Woods_SelectionModel_",file_extension,".csv",sep=""),row.names = F)
}

Vevea_Woods_Selection1 <- function(df,file_extension,folderName){
  # Define the cut-points
  a <- c(0.025, 0.005, 0.01, 0.05, 0.10, 0.25, 0.35, 0.50, 
         0.65, 0.75, 0.90, 0.95, 0.99, 0.995)
  
  # Define the selection likelihood for each interval 
  # (moderate/severe selection)
  w.moderate.onetailed <- c(1, 0.99, 0.95, 0.90, 0.80, 0.75, 0.65, 0.60, 
                            0.55, 0.50, 0.50, 0.50, 0.50, 0.50)
  w.severe.onetailed <- c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.40, 0.35, 
                          0.30, 0.25, 0.10, 0.10, 0.10, 0.10)
  w.moderate.twotailed <- c(1, 0.99, 0.95, 0.90, 0.80, 0.75, 0.60, 0.60, 
                            0.75, 0.80, 0.90, 0.95, 0.99, 1)
  w.severe.twotailed <- c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.25, 0.25, 
                          0.50, 0.60, 0.75, 0.90, 0.99, 1)
  
  sel <- weightfunct(df$yi, df$vi, steps=a)
  adj_est <- c(sel$adj_est[[2]])
  print(sel)
  sel <- weightfunct(df$yi, df$vi, steps=a)
  adj_est <- c(adj_est, sel$adj_est[[2]])
  print(sel)
  sel <- weightfunct(df$yi, df$vi, steps=a)
  adj_est <- c(adj_est, sel$adj_est[[2]])
  print(sel)
  sel <- weightfunct(df$yi, df$vi, steps=a)
  adj_est <- c(adj_est, sel$adj_est[[2]])
  print(sel)
  selection <- c("Moderate one-tailed selection",	"Severe one-tailed selection",
                 "Moderate two-tailed selection",	"Severe two-tailed selection")
  
  a <- data.frame(Selection = selection, Adjusted.Estimate = adj_est)
  
  write.csv(a, paste("PublicationBias/",folderName,"/Results_Vevea_Woods_SelectionModel_",file_extension,".csv",sep=""),row.names = F)
}

######################################################
#         Wrapper Function to Perform all tests      #
######################################################
#read in an outcome-level df and an experiment-level df
RunAllTests <- function(outcomeDF, expDF, file_extension, folderName){
  Egger(outcomeDF,file_extension,folderName)
  ThreePSM(outcomeDF,file_extension,folderName)
  Vevea_Woods_Selection(outcomeDF,file_extension,folderName)
  Trim_Fill(expDF,file_extension,folderName)
}

##########################################
#            Main Script                 #
##########################################

#INPUT FILE NAMES YOU WANT TO RUN HERE
outcomeDF = read.csv(IntrusionFile_UNSP)
expDF= read.csv(ExpIntrusionFile_UNSP)
#remove NA in yi column
expDF$yi = as.numeric(expDF$yi)
expDF$vi = as.numeric(expDF$vi)
expDF$SE = as.numeric(expDF$SE)
expDF = expDF[!is.na(expDF$yi), ]

outcomeDF$yi = as.numeric(outcomeDF$yi)
outcomeDF$vi = as.numeric(outcomeDF$vi)
outcomeDF$SE = as.numeric(outcomeDF$SE)

RunAllTests(outcomeDF,expDF,"INTRUSION","UNSP")

#IMPORTANT: 
#1. TRIMFILL: INTRUSION UNSPECIFIED DID NOT CONVERGE EVEN AFTER CHANGING OPTIMIZER PARAMETERS. Error message:Fisher scoring algorithm did not converge  (https://www.metafor-project.org/doku.php/tips:convergence_problems_rma)
#2. THREE PARAMETER SELECTION MODEL: a) QUESTIONNAIRE_INCREASE ENCOUNTERS ERROR: Error in solve.default(output_adj$hessian) : system is computationally singular: reciprocal condition number = 1.65318e-20
