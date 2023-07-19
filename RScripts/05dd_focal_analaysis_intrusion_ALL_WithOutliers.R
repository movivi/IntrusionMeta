

work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)
options(scipen=999)
require(metafor)
library(esc)


######################
#     User Defined   #
######################
#FOR INTRUSION ALL
IntrusionFile <- "IM_ES_Step10_INTALL_INTRUSION.csv"
EmotionFile <- "IM_ES_Step10_INTALL_EMOTION.csv"
SymptomsFile <- "IM_ES_Step10_INTALL_SYMPTOMS.csv"
DiaryFile <- "IM_ES_Step11_INTALL_INTRUSION_DIARY.csv"
QuestionnaireFile <- "IM_ES_Step11_INTALL_INTRUSION_QUESTIONNAIRE.csv"
TaskFile <- "IM_ES_Step11_INTALL_INTRUSION_TASK.csv"
AllFile <- "IM_ES_Step5a_MISMATCHROWS_REMOVED.csv"

########################################
# Multilevel Meta-Analysis of INTRUSION  #
########################################

df <- read.csv(IntrusionFile) #CHANGE FILENAME BASED ON THE DATASET YOU WANT TO RUN.

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#identify level 2 names with single k
single_level2 <- df %>% count(Level2_final, sort = TRUE) %>% 
  filter(n == 1) %>% 
  select(Level2_final) %>% 
  .$Level2_final
single_level2

#identify level 2 names with multiple k
nonsolo_level2 <- df[which(!df$Level2_final %in% single_level2), ]

level2_names <- unique(nonsolo_level2$Level2_final)

#define results file to store results from for loop
df1 <- data.frame(matrix(NA, ncol = 13, nrow = length(level2_names)))
df1[, 1] <- level2_names
names(df1) <- c("level2names", "n_study", "k", "Hedgesg", "ci.lb", "ci.ub", "z", "p",
                "Q", "tau2.studylevel","tau2.outcomelevel","I2","Q.pval")

#multilevel meta for level 2 names with more than 1 k
for(i in 1:length(level2_names)){
  meta <- rma.mv(yi, vi, data = nonsolo_level2, random = ~1 | StudyID/EffectSizeID, subset = (Level2_final == level2_names[i]), control=list(rel.tol=1e-8))
  print(meta)
  #study and outcome level I^2 using function var_comp (run the script on console prior to running these codes)
  i2 <- var.comp(meta)
  summary(i2)
  i2
  re_table <- data.frame(n_study = meta$s.nlevels[1],
                         k = meta$s.nlevels[2],
                         Hedgesg = meta$beta,
                         ci.lb = meta$ci.lb,
                         ci.ub = meta$ci.ub,
                         z = meta$zval,
                         p = meta$pval,
                         Q = meta$QE,
                         tau2.studylevel = meta$sigma2[1],
                         tau2.outcomelevel = meta$sigma2[2],
                         I2 = i2$totalI2,
                         Q.pval = meta$QEp)
  df1[i, -1] <- re_table
}

#define list dataset for level 2 names with single k
solo_level2 <- df[which(df$Level2_final %in% single_level2), ]
df2 = split(solo_level2, solo_level2$Level2_final) 
names(df2)

sololevel2_names <- unique(solo_level2$Level2_final)


meta.final = c()

#random effects meta for level 2 names with single k
for (p in df2) {
  print(p$Level2_final)
  print(p)
  x = as.data.frame(p)
  meta = rma(yi=x$yi, vi = x$vi, data=x)
  print(meta)
  
  meta.res <- data.frame(level2names = unique(x$Level2_final),
                         n_study = length(unique(x$StudyID)),
                         n_article = length(unique(x$ArticleID)),
                         k = meta$k,
                         Hedgesg = meta$beta,
                         ci.lb = meta$ci.lb,
                         ci.ub = meta$ci.ub,
                         z = meta$zval,
                         p = meta$pval,
                         Q = meta$QE,
                         #tau2.studylevel = meta$sigma2[1],
                         #tau2.outcomelevel = meta$sigma2[2],
                         I2 = meta$I2,
                         Q.pval = meta$QEp,
                         stringsAsFactors = F)
  meta.final <- rbind(meta.final, meta.res)
}
#merge solo and multiple k level 2 meta results into one folder
meta_level2 = bind_rows(df1,meta.final)


#returns a dataframe which containing variables of a single moderator analysis - Q, tau2, I2
#modMain: 1 = main analysis, 2 = moderator analysis
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

#Level 3 beh/neuro/pharma
df$Level3_Beh_Pharma_Neuro = factor(df$Level3_Beh_Pharma_Neuro)
a = getTable_Mod("Moderation: INTALL_INTRUSION LEVEL 3 BEH/NEURO/PHARMA",df$Level3_Beh_Pharma_Neuro,2) #CHANGE NAME BASED ON DATAFRAME USED
allTable = rbind(allTable,a)

#Level 3 category
df$Level3_Imagery_Verbal_Emotion_Other = factor(df$Level3_Imagery_Verbal_Emotion_Other)
a = getTable_Mod("Moderation: INTALL_INTRUSION LEVEL 3 CATEGORY",df$Level3_Imagery_Verbal_Emotion_Other,2) #CHANGE NAME BASED ON DATAFRAME USED
allTable = rbind(allTable,a)

#Level 3 direct/indirect
df$Level3_Direct_Indirect = factor(df$Level3_Direct_Indirect)
a = getTable_Mod("Moderation: INTALL_INTRUSION LEVEL 3 DIRECT/INDIRECT",df$Level3_Direct_Indirect,2) #CHANGE NAME BASED ON DATAFRAME USED
allTable = rbind(allTable,a)

#Level 3 Task/Instruction
df$Level3_Task_Instruction = factor(df$Level3_Task_Instruction)
a = getTable_Mod("Moderation: INTALL_INTRUSION LEVEL 3 task/instruction",df$Level3_Task_Instruction,2)
allTable = rbind(allTable,a)

#write moderation results
allTable <- sapply(allTable, as.character)
allTable[is.na(allTable)] <- ""
write.csv(allTable,"Results_ModeratorAnalysis_INTALL_INTRUSION_ALL_WithOutliers.csv",row.names = F) #cHANGE FILENAME BASED ON DATAFRAME USED


#write RM results
meta_level2$Hedgesg = format(round(meta_level2$Hedgesg,digits=5),nsmall=5)
meta_level2$ci.lb = format(round(meta_level2$ci.lb,digits=5),nsmall=5)
meta_level2$ci.ub = format(round(meta_level2$ci.ub,digits=5),nsmall=5)
meta_level2$z = format(round(meta_level2$z,digits=5),nsmall=5)
meta_level2$p = format(round(meta_level2$p,digits=5),nsmall=5)
meta_level2$Q = format(round(meta_level2$Q,digits=5),nsmall=5)
meta_level2$I2 = format(round(meta_level2$I2,digits=5),nsmall=5)
meta_level2$Q.pval = format(round(meta_level2$Q.pval,digits=5),nsmall=5)


write.csv(meta_level2, "Results_RM_Meta-Analysis_INTALL_INTRUSION_WithOutliers.csv",row.names = F) #CHANGE FILE NAME HERE BASED ON THE DATASET USED
