library(dplyr)
library(psych)

#mo's data
df <- read.csv("~/HUlab/Analyses/IM_ES_Step5a_MISMATCHROWS_REMOVED.csv")
df$yi = as.numeric(df$yi)
df$SampleSize = as.numeric(df$SampleSize)
df$MeanAge = as.numeric(df$MeanAge)
df$FemaleRatio = as.numeric(df$FemaleRatio)
df$Female = as.numeric(df$Female)
df$SampleSize = as.numeric(df$SampleSize)


df1 = df[!is.na(df$yi), ] #remove NA rows

str(df1) #581 rows (584 valid effect sizes for hypothesis-free analyses)

df1a = df1 %>% distinct(Level2_final , .keep_all=TRUE) #identify unique techniques

df2 = df1 %>% distinct(Title , .keep_all=TRUE) #identify unique articles

length(df2$Title) #126 articles for hypothesis-free analyses

df3 = df1 %>% distinct(StudyID , .keep_all=TRUE) #identify unique experiments

length(df3$StudyID) #145 experiments for hypothesis-free analyses

sum(df3$SampleSize) #11132 participants for hypothesis-free analyses

df3a = df3[!is.na(df3$FemaleRatio), ] #remove NA rows

mean(df3a$FemaleRatio) #67.043% female ratio

sum(df3$Female, na.rm = TRUE)/sum(df3$SampleSize) #67.643% female percentage


df4 = df1 %>% distinct(Title , StudyNo, Intrusion_Predicted_Direction, .keep_all=TRUE) #identify unique articles

length(which(df4$Intrusion_Predicted_Direction == "decrease")) #101 decrease hypothesis experiments
length(which(df4$Intrusion_Predicted_Direction == "increase")) #60 increase hypothesis experiments
length(which(df4$Intrusion_Predicted_Direction == "unspecified")) #38 unspecified hypothesis experiments
length(which(df4$Intrusion_Predicted_Direction == "null")) # 2  null hypothesis experiments (articleID = 129 had both decrease and null hypothesis testing)

write.csv(df3, "descriptive_data.csv")

write.csv(df4, "descriptive_data_direction.csv")
