library(metafor)
library(tidyr)
library(dplyr)
library(magrittr)
library(esc)
library(naniar)



work_dir = "~/HUlab/Analyses" # Change to the work folder, should include both the data and scripts.
setwd(work_dir)

options(digits=5)

# Import csv file
dat <- read.csv("IM_rawdata_v3.csv")


##STEP 1 BASEED ON CODED MEAN,SD,SAMPLE SIZE
#convert string texts like Not Reported to NA from specific columns used to compute effect size
dat$ExperimentalConditionSampleSize = as.numeric(dat$ExperimentalConditionSampleSize)
dat$ComparisonConditionSampleSize = as.numeric(dat$ComparisonConditionSampleSize)
dat$Experimental_Mean = as.numeric(dat$Experimental_Mean)
dat$Comparison_Mean = as.numeric(dat$Comparison_Mean)
dat$Experimental_SD = as.numeric(dat$Experimental_SD)
dat$Comparison_SD = as.numeric(dat$Comparison_SD)
dat$Experimental_SE = as.numeric(dat$Experimental_SE)
dat$Comparison_SE = as.numeric(dat$Comparison_SE)
dat$StudySampleSize = as.numeric(dat$StudySampleSize)

#add sign based on the direction of the mean difference
dat$sign <- with(dat, ifelse(dat$Experimental_Mean > dat$Comparison_Mean, -1, 1)) 

#compute effect size (SMD)
dat_st1 <- escalc(measure="SMD", m1i=dat$Comparison_Mean, m2i=dat$Experimental_Mean, sd1i=dat$Comparison_SD, sd2i=dat$Experimental_SD, n1i=dat$ComparisonConditionSampleSize, n2i=dat$ExperimentalConditionSampleSize, data=dat)

#WRITE THE DAT FILE INTO CSV (CONTAINS EFFECT SIZE SOLELY COMPUTED USING MEAN, SD AND SAMPLE SIZE)
write.csv(dat_st1, "IM_ES_step1.csv")


##STEP 2 BASED ON CODED MEAN,SAMPLE SIZE AND SD COMPUTED FROM SE
#calcuate sd based on se and sample size
dat$exp_sd = replmiss(dat$Experimental_SD, dat$Experimental_SE * sqrt(dat$ExperimentalConditionSampleSize))
dat$com_sd = replmiss(dat$Comparison_SD, dat$Comparison_SE * sqrt(dat$ComparisonConditionSampleSize))

dat$exp_sd = as.numeric(dat$exp_sd)
dat$com_sd = as.numeric(dat$com_sd)

#compute effect size (SMD)
dat_st2 <- escalc(measure="SMD", m1i=dat$Comparison_Mean, m2i=dat$Experimental_Mean, sd1i=dat$com_sd, sd2i=dat$exp_sd, n1i=dat$ComparisonConditionSampleSize, n2i=dat$ExperimentalConditionSampleSize, data=dat)

#WRITE THE DAT FILE INTO CSV (CONTAINS EFFECT SIZE COMPUTED USING MEAN, SD AND SAMPLE SIZE + SE CONVERTED TO SD)
write.csv(dat_st2, "IM_ES_step2.csv")


##STEP 3 BASED ON CODED T-SCORES
#duplicate and modify < and other strings in the effect size column
dat$eval = dat$EffectSize
dat$eval = gsub("[^0-9.-]", "", dat$eval)
dat$eval = as.numeric(dat$eval)
dat$etype = dat$EffectSizeType #effectsize type

#add original t-value column to tval column to remove text
dat_st2$tval = dat_st2$T.test

#change Not Reported and Not provided text in tval to NA
dat_st2$tval[dat_st2$tval == 'Not Reported'] <- NA
dat_st2$tval[dat_st2$tval == 'Not provided'] <- NA
dat_st2$tval[dat_st2$tval == 'Not reported'] <- NA

dat_st2$ttrue = grepl('t', dat_st2$tval) #identify rows containing T value (only include df = 1)
trows = dat_st2 %>% filter(dat_st2$ttrue== TRUE & is.na(dat_st2$yi)) #create data set with rows containing F values
trows$tval = gsub("\\(.*?\\)","",trows$tval) #remove df values and only preserve the T value
trows$tval = gsub("[^0-9.-]", "", trows$tval) #remove string values
trows$tval = as.numeric(trows$tval) #ensure all rows numeric

#compute cohen's d  based on t-values (since escalc was not computing it 
#automatically when i plug in the t-score values; so i used esc_t function)
c = esc_t(t = trows$tval, grp1n = trows$ComparisonConditionSampleSize, 
          grp2n = trows$ExperimentalConditionSampleSize, es.type = "g")
d = esc_t(t = trows$tval, totaln = trows$StudySampleSize, es.type = "g")

#add es and var info from list a and b to trows table
trows$yi = replmiss(trows$yi, c$es)
trows$yi = replmiss(trows$yi, d$es)

trows$vi = replmiss(trows$vi, c$var)
trows$vi = replmiss(trows$vi, d$var)

#merge trows and dat_st2 dataset based on EffectSizeID
dat_st2 = dat_st2[!(dat_st2$EffectSizeID %in% trows$EffectSizeID),]
dat_st2 <- rbind(trows,dat_st2)
dat_st2 = dat_st2[order(dat_st2$EffectSizeID),]
dat_st3 = dat_st2

dat_st3$tval = as.numeric(dat_st3$tval)

#WRITE THE DAT FILE INTO CSV (CONTAINS EFFECT SIZE COMPUTED USING MEAN, SD AND SAMPLE SIZE + SE CONVERTED TO SD + ALREADY CODED T SCORES)
write.csv(dat_st3, "IM_ES_step3.csv")


##STEP 4 BASED ON TOTAL SAMPLE SIZE
#calculate based on assuming equal sample size for both groups (using total sample size column)
#duplicate experimental and comparison sample size columns
dat$expsample = dat$ExperimentalConditionSampleSize
dat$comsample = dat$ComparisonConditionSampleSize

#assume equal sample size for those rows which don't have group sample size not reported (use total sample size column)
missingSampleInd_total = which(is.na(dat$expsample))
dat$SampleSize = as.numeric(dat$SampleSize)

#based on studysamplesize column
dat$expsample[missingSampleInd_total] = dat$StudySampleSize[missingSampleInd_total]/2
dat$comsample[missingSampleInd_total] = dat$StudySampleSize[missingSampleInd_total]/2

#based on samplesize column
dat$expsample[missingSampleInd_total] = replmiss(dat$expsample[missingSampleInd_total], dat$SampleSize[missingSampleInd_total]/dat$SampleNo[missingSampleInd_total])
dat$comsample[missingSampleInd_total] = replmiss(dat$comsample[missingSampleInd_total], dat$SampleSize[missingSampleInd_total]/dat$SampleNo[missingSampleInd_total])

#compute effect size (SMD) using SD computed from SE and originally coded SD
dat_st4 <- escalc(measure="SMD", m1i=dat$Comparison_Mean, m2i=dat$Experimental_Mean, sd1i=dat$com_sd, sd2i=dat$exp_sd, n1i=dat$comsample, n2i=dat$expsample, data=dat)

#add es and var info from dat_st3 to dat_st4 table
dat_st4$yi = replmiss(dat_st4$yi, dat_st3$yi)
dat_st4$vi = replmiss(dat_st4$vi, dat_st3$vi)

#add tval andttrue info from dat_st3 to dat_st4 table and base dat
dat_st4$tval = dat_st3$tval
dat_st4$ttrue = dat_st3$ttrue

dat$tval = dat_st3$tval
dat$ttrue = dat_st3$ttrue

#WRITE THE DAT FILE INTO CSV (CONTAINS EFFECT SIZE COMPUTED USING MEAN, SD AND SAMPLE SIZE + SE CONVERTED TO SD + ALREADY CODED T SCORES + ASSUMING EQUAL SAMPLE SIZE)
write.csv(dat_st4, "IM_ES_step4.csv")


##STEP 5 BASED ON P VALUE
#duplicate p-value column to modify < and other strings in this column
dat$pval = dat$pvalue
dat$pval = gsub("[^0-9.-]", "", dat$pval)
dat$pval = as.numeric(dat$pval)

#add t-sign row info from col DirectionSign to col Sign
dat$sign = replmiss(dat$sign, dat$DirectionSignMo)

#compute t-values based on p-values
dat$tvalcomputed <- dat$sign * qt(dat$pval/2, df=dat$ExperimentalConditionSampleSize+dat$ComparisonConditionSampleSize-2, lower.tail=FALSE)
dat$tvalcomputed = as.numeric(dat$tvalcomputed)

#add computed t-values info from col tvalcomputed to col tval
dat$tval = replmiss(dat$tval, dat$tvalcomputed)

#compute cohen's d  based on t-values (since escalc was not computing it automatically when i plug in the t-score values; so i used esc_t function)
a = esc_t(t = dat$tval, grp1n = dat$expsample, grp2n = dat$comsample, es.type = "g")

#add es and var info from list a to dat_st5 table
dat_st5 = dat_st4
dat_st5$yi = replmiss(dat_st5$yi, a$es)
dat_st5$vi = replmiss(dat_st5$vi, a$var)

#manually change row 265 (row 266 in excel) as it has a p value not from t-test
dat_st5$yi[265] <- NA
dat_st5$vi[265] <- NA

is.num <- sapply(dat_st5, is.numeric)
dat_st5[is.num] <- lapply(dat_st5[is.num], round, 9)

#add SE value
dat_st5$SE = sqrt(dat_st5$vi)

dat_st5$SE = format(round(dat_st5$SE,digits=5),nsmall=5)
dat_st5$yi = format(round(dat_st5$yi,digits=5),nsmall=5)
dat_st5$vi = format(round(dat_st5$vi,digits=5),nsmall=5)

#WRITE THE DAT FILE INTO CSV (CONTAINS EFFECT SIZE COMPUTED USING MEAN, SD AND SAMPLE SIZE + SE CONVERTED TO SD + ALREADY CODED T SCORES + ASSUMING EQUAL SAMPLE SIZE + P VALUES CONVERTED TO T SCORE)
write.csv(dat_st5, "IM_ES_step5_upd.csv")
