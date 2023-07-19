

work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)
options(scipen=999)
require(metafor)
library(esc)
library(ggplot2)


######################
#     User Defined   #
######################
##FOR INTRUSION_INCREASE
IntrusionFile <- "IM_ES_Step7_INTINC_INTRUSION.csv"
EmotionFile <- "IM_ES_Step7_INTINC_EMOTION.csv"
SymptomsFile <- "IM_ES_Step7_INTINC_SYMPTOMS.csv"
DiaryFile <- "IM_ES_Step8_INTINC_INTRUSION_DIARY.csv"
QuestionnaireFile <- "IM_ES_Step8_INTINC_INTRUSION_QUESTIONNAIRE.csv"
TaskFile <- "IM_ES_Step8_INTINC_INTRUSION_TASK.csv"
AllFile <- "IM_ES_step6_INTINC.csv"

########################################
# Multilevel Meta-Analysis of INTRUSION  #
########################################

df <- read.csv(IntrusionFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]


meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta

#study and outcome level I^2 using function var_comp (run the script on console prior to running these codes)
i2 <- var.comp(meta)
summary(i2)
i2


### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2

100 * sav[[1]]$random[1,2:3] / (sav[[1]]$random[1,2:3] + (meta$k-meta$p)/sum(diag(P))) ### CI for study level I^2
100 * sav[[2]]$random[1,2:3] / (sav[[2]]$random[1,2:3] + (meta$k-meta$p)/sum(diag(P))) ### CI for outcome level I^2

#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2
options(digits=3) #sets all output to 3 decimal places

meta.res <- data.frame(Analysis = "Intrusion Multilevel INTINC",
                       n_study = meta$s.nlevels[1],
                       k = meta$s.nlevels[2],
                       Hedgesg = meta$beta,
                       ci.lb = meta$ci.lb,
                       ci.ub = meta$ci.ub,
                       z = meta$zval,
                       p = meta$pval,
                       Q = meta$QE,
                       tau2.studylevel = meta$sigma2[1],
                       tau2.outcomelevel = meta$sigma2[2],
                       I2 = total_I2,
                       Q.pval = meta$QEp,
                       stringsAsFactors = F)


###################################
# Analysis for EMOTION effects    #
####################################
df = read.csv(EmotionFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta

#study and outcome level I^2 using function var_comp (run the script on console prior to running these codes)
i2 <- var.comp(meta)
summary(i2)
i2

### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res2 <- data.frame(Analysis = "Emotion Multilevel INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

###################################
# Analysis for SYMPTOMS effects    #
####################################
df = read.csv(SymptomsFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta

#study and outcome level I^2 using function var_comp (run the script on console prior to running these codes)
i2 <- var.comp(meta)
summary(i2)
i2

### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res3 <- data.frame(Analysis = "Symtpoms Multilevel INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

###################################
# Analysis for DIARY effects    #
####################################
df = read.csv(DiaryFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta

#study and outcome level I^2 using function var_comp (run the script on console prior to running these codes)
i2 <- var.comp(meta)
summary(i2)
i2

### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res4 <- data.frame(Analysis = "Diary Multilevel INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

###################################
# Analysis for QUESTIONNAIRE effects    #
####################################
df = read.csv(QuestionnaireFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta

#study and outcome level I^2 using function var_comp (run the script on console prior to running these codes)
i2 <- var.comp(meta)
summary(i2)
i2

### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res5 <- data.frame(Analysis = "Questionnaire Multilevel INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

###################################
# Analysis for TASK effects    #
####################################
df = read.csv(TaskFile)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta

#study and outcome level I^2 using function var_comp (run the script on console prior to running these codes)
i2 <- var.comp(meta)
summary(i2)
i2

### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res6 <- data.frame(Analysis = "Task Multilevel INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

allres = rbind(meta.res,meta.res2,meta.res3,meta.res4,meta.res5,meta.res6)

##################################
#INTRUSION WITH OUTLIER REMOVED

df = read.csv(IntrusionFile)
df = subset(df,outliers!=1)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta


### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res7 <- data.frame(Analysis = "INTRUSION Multilevel without Outliers INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

##################################
#EMOTION WITH OUTLIER REMOVED

df = read.csv(EmotionFile)
df = subset(df,outliers!=1)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta


### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res8 <- data.frame(Analysis = "EMOTION Multilevel without Outliers INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

##################################
#SYMPTOMS WITH OUTLIER REMOVED

df = read.csv(SymptomsFile)
df = subset(df,outliers!=1)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta


### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res9 <- data.frame(Analysis = "SYMPTOMS Multilevel without Outliers INTINC",
                        n_study = meta$s.nlevels[1],
                        k = meta$s.nlevels[2],
                        Hedgesg = meta$beta,
                        ci.lb = meta$ci.lb,
                        ci.ub = meta$ci.ub,
                        z = meta$zval,
                        p = meta$pval,
                        Q = meta$QE,
                        tau2.studylevel = meta$sigma2[1],
                        tau2.outcomelevel = meta$sigma2[2],
                        I2 = total_I2,
                        Q.pval = meta$QEp,
                        stringsAsFactors = F)

##################################
#DIARY WITH OUTLIER REMOVED

df = read.csv(DiaryFile)
df = subset(df,outliers!=1)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta


### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res10 <- data.frame(Analysis = "DIARY Multilevel without Outliers INTINC",
                         n_study = meta$s.nlevels[1],
                         k = meta$s.nlevels[2],
                         Hedgesg = meta$beta,
                         ci.lb = meta$ci.lb,
                         ci.ub = meta$ci.ub,
                         z = meta$zval,
                         p = meta$pval,
                         Q = meta$QE,
                         tau2.studylevel = meta$sigma2[1],
                         tau2.outcomelevel = meta$sigma2[2],
                         I2 = total_I2,
                         Q.pval = meta$QEp,
                         stringsAsFactors = F)

##################################
#QUESTIONNAIRE WITH OUTLIER REMOVED

df = read.csv(QuestionnaireFile)
df = subset(df,outliers!=1)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta


### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res11 <- data.frame(Analysis = "QUESTIONNAIRE Multilevel without Outliers INTINC",
                         n_study = meta$s.nlevels[1],
                         k = meta$s.nlevels[2],
                         Hedgesg = meta$beta,
                         ci.lb = meta$ci.lb,
                         ci.ub = meta$ci.ub,
                         z = meta$zval,
                         p = meta$pval,
                         Q = meta$QE,
                         tau2.studylevel = meta$sigma2[1],
                         tau2.outcomelevel = meta$sigma2[2],
                         I2 = total_I2,
                         Q.pval = meta$QEp,
                         stringsAsFactors = F)

##################################
#TASK WITH OUTLIER REMOVED

df = read.csv(TaskFile)
df = subset(df,outliers!=1)

#remove NA in yi column for meta computation
df$yi = as.numeric(df$yi)
df$vi = as.numeric(df$vi)
df = df[!is.na(df$yi), ]

#multilevel model
meta <- rma.mv(yi=yi, V = vi, data=df, slab=StudyID, random = ~ 1 | StudyID/EffectSizeID)
meta


### study and outcome level I^2
sav <- confint(meta)
W <- diag(1/df$vi)
X <- model.matrix(meta)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * meta$sigma2 / (meta$sigma2 + (meta$k-meta$p)/sum(diag(P))) ### study and outcome level I^2
#calculation of the total I^2
total_I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k-meta$p)/sum(diag(P))) ### total I^2

meta.res12 <- data.frame(Analysis = "TASK Multilevel without Outliers INTINC",
                         n_study = meta$s.nlevels[1],
                         k = meta$s.nlevels[2],
                         Hedgesg = meta$beta,
                         ci.lb = meta$ci.lb,
                         ci.ub = meta$ci.ub,
                         z = meta$zval,
                         p = meta$pval,
                         Q = meta$QE,
                         tau2.studylevel = meta$sigma2[1],
                         tau2.outcomelevel = meta$sigma2[2],
                         I2 = total_I2,
                         Q.pval = meta$QEp,
                         stringsAsFactors = F)


allres = rbind(allres,meta.res7,meta.res8,meta.res9,meta.res10,meta.res11,meta.res12)

write.csv(allres, "Results_Multilevel_Meta-Analysis_INTINC.csv",row.names = F)