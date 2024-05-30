
# ******************************************************************************************************************************************
# Physical Activity data harmonization for FHS OMNI 1 cohort
# ******************************************************************************************************************************************
# 
# Created by Chenglin Lyu
# Last updated: 5/29/2024
# 
# 
# The purpose of this R code is to abstract data from questions related to physical activity in OMNI 1, exams 1, 2, 3 and 4
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
# e_exam_ex01_7_0020.sas7bdat (OMNI 1 Exam 1)
# e_exam_ex02_7_0003.sas7bdat (OMNI 1 Exam 2)
# e_exam_ex03_7_0426.sas7bdat (OMNI 1 Exam 3)
# e_exam_ex09_1b_0844.sas7bdat (OMNI 1 Exam 4)


# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")


#R library used:
library(sas7bdat)
library(tidyverse)
library(haven)
library(dplyr)
library(stringr)


########################Input original data from Y drive
exam1<-read_sas("/restricted/projectnb/fhs-bap/phhwang/Data/Physical activity/OMNI 1/e_exam_ex01_7_0020.sas7bdat")
exam2<-read_sas("/restricted/projectnb/fhs-bap/phhwang/Data/Physical activity/OMNI 1/e_exam_ex02_7_0003.sas7bdat")
exam3<-read_sas("/restricted/projectnb/fhs-bap/phhwang/Data/Physical activity/OMNI 1/e_exam_ex03_7_0426.sas7bdat")
exam4<-read_sas("/restricted/projectnb/fhs-bap/phhwang/Data/Physical activity/OMNI 1/e_exam_ex09_1b_0844.sas7bdat")


########################Exclude the participants from Gen 2 cohort at Exam 4
exam4<-subset(exam4, exam4$idtype == 7)


########################Pull out variables related to rest and activity for each exam
restact2<-exam2[,colnames(exam2) %in% c("id", paste0("g", c(689:693) ))]
restact3<-exam3[,colnames(exam3) %in% c("id", paste0("h", c(480:484) ))]
restact4<-exam4[,colnames(exam4) %in% c("id", paste0("j", c(628:632) ))]


########################Pull out variables related to flights of stairs and walking pace for each exam
walk2<-exam2[,colnames(exam2) %in% c("id", "g687", "g688")]
walk3<-exam3[,colnames(exam3) %in% c("id", "h485","h486" )]


########################Pull out variables related to sitting
sit4<-exam4[,colnames(exam4) %in% c("id", "j633", "j634")]


########################Pull out variables related to specific activities for each exam
act3<-exam3[,colnames(exam3) %in% c("id", paste0("h", c(488:557) ))]
act4<-exam4[,colnames(exam4) %in% c("id", paste0("j", c(638:723) ))]


########################Rename sleep rest and activity
colsleep2<-c("sleep_2","seden_2","slightact_2","modact_2","heavyact_2")
colsleep3<-c("sleep_3","seden_3","slightact_3","modact_3","heavyact_3")
colsleep4<-c("sleep_4","seden_4","slightact_4","modact_4","heavyact_4")
colnames(restact2)[-1]<-colsleep2
colnames(restact3)[-1]<-colsleep3
colnames(restact4)[-1]<-colsleep4


########################Rename flights of stairs and walking pace
colnames(walk2)[-1]<-c("stairs_2","walkblock_2")
colnames(walk3)[-1]<-c("walkpace_3","stairs_3")


########################Rename sitting variable
colnames(sit4)[-1]<-c("sitfreq_4","sithr_4")


########################Rename activities in the past year for Exam 3 
colname3<-c("walk_3",
            "walkfreq_3",
            "walkhr_3",
            "walkmin_3",
            "walkmon_3",
            "calisth_3",
            "calisthfreq_3",
            "calisthhr_3",
            "calisthmin_3",
            "calisthmon_3",
            "household_3",
            "householdfreq_3",
            "householdhr_3",
            "householdmin_3",
            "householdmon_3",
            "lawn_3",
            "lawnfreq_3",
            'lawnhr_3',
            "lawnmin_3",
            "lawnmon_3",
            "garden_3",
            "gardenfreq_3",
            "gardenhr_3",
            "gardenmin_3",
            "gardenmon_3",
            "hike_3",
            "hikefreq_3",
            "hikehr_3",
            "hikemin_3",
            "hikemon_3",
            "jog_3",
            "jogfreq_3",
            "joghr_3",
            "jogmin_3",
            "jogmon_3",
            "bike_3",
            "bikefreq_3",
            "bikehr_3",
            "bikemin_3",
            "bikemon_3",
            "cycle_3",
            "cyclefreq_3",
            "cyclehr_3",
            "cyclemin_3",
            "cyclemon_3",
            "dance_3",
            "dancefreq_3",
            "dancehr_3",
            "dancemin_3",
            "dancemon_3",
            "aerobic_3",
            "aerobicfreq_3",
            "aerobichr_3",
            "aerobicmin_3",
            "aerobicmon_3",
            "golf_3",
            "golffreq_3",
            "golfhr_3",
            "golfmin_3",
            "golfmon_3",
            "swim_3",
            "swimfreq_3",
            "swimhr_3",
            "swimmin_3",
            "swimmon_3",
            "weights_3",
            "weightsfreq_3",
            "weightshr_3",
            "weightsmin_3",
            "weightsmon_3")
colnames(act3)[-1]<-colname3


########################Rename activities in the past year for Exam 4 
colname4<-c("walk_4",
            "walkfreq_4",
            "walkhr_4",
            "walkmin_4",
            "walkmon_4",
            "calisth_4",
            "calisthfreq_4",
            "calisthhr_4",
            "calisthmin_4",
            "calisthmon_4",
            "cycle_4",
            "cyclefreq_4",
            "cyclehr_4",
            "cyclemin_4",
            "cyclemon_4",
            "weights_4",
            "weightsfreq_4",
            "weightshr_4",
            "weightsmin_4",
            "weightsmon_4",
            "household_4",
            "householdfreq_4",
            "householdhr_4",
            "householdmin_4",
            "householdmon_4",
            "jog_4",
            "jogfreq_4",
            "joghr_4",
            "jogmin_4",
            "jogmon_4",
            "bike_4",
            "bikefreq_4",
            "bikehr_4",
            "bikemin_4",
            "bikemon_4",
            "dance_4",
            "dancefreq_4",
            "dancehr_4",
            "dancemin_4",
            "dancemon_4",
            "aerobic_4",
            "aerobicfreq_4",
            "aerobichr_4",
            "aerobicmin_4",
            "aerobicmon_4",
            "swim_4",
            "swimfreq_4",
            "swimhr_4",
            "swimmin_4",
            "swimmon_4",
            "tennis_4",
            "tennisfreq_4",
            "tennishr_4",
            "tennismin_4",
            "tennismon_4",
            "golf_4",
            "golffreq_4",
            "golfhr_4",
            "golfmin_4",
            "golfmon_4",
            "lawn_4",
            "lawnfreq_4",
            "lawnhr_4",
            "lawnmin_4",
            "lawnmon_4",
            "garden_4",
            "gardenfreq_4",
            "gardenhr_4",
            "gardenmin_4",
            "gardenmon_4",
            "hike_4",
            "hikefreq_4",
            "hikehr_4",
            "hikemin_4",
            "hikemon_4",
            "lightrec_4",
            "lightrecfreq_4",
            "lightrechr_4",
            "lightrecmin_4",
            "lightrecmon_4",
            "other_4",
            "othername_4",
            "otherfreq_4",
            "otherhr_4",
            "othermin_4",
            "othermon_4")

colnames(act4)[-1]<-colname4


########################Merge by exams
df_list2 <- list(restact2,walk2)
paex2<-df_list2 %>% reduce(full_join, by='id')
df_list3 <- list(restact3,walk3,act3)
paex3<-df_list3 %>% reduce(full_join, by='id')
df_list4 <- list(restact4,sit4,act4)
paex4<-df_list4 %>% reduce(full_join, by='id')


########################Calculate total time for Exam 3
hr<-colnames(paex3[grepl("hr",names(paex3), fixed=TRUE)])
min<-colnames(paex3[grepl("min",names(paex3), fixed=TRUE)])
data1<-as.data.frame(paex3[grepl("hr",names(paex3), fixed=TRUE)])
data2<-as.data.frame(paex3[grepl("min",names(paex3), fixed=TRUE)])
data<-cbind(data1,data2)


for (i in 1:14 ){                                                              
  
  data[,paste0("time",hr[i])]<-data[,min[i]]*60+data[,hr[i]]*3600
  
} 

time3<-data[,c(29:42)]
time3col<-gsub("hr", "time", colnames(time3))
time3col<-substring(time3col,5)
colnames(time3)<-time3col
timeex3<-cbind(paex3, time3)


########################Calculate total time for Exam 4
hr<-colnames(paex4[grepl("hr",names(paex4), fixed=TRUE)])[-1]
min<-colnames(paex4[grepl("min",names(paex4), fixed=TRUE)])
data1<-as.data.frame(paex4[grepl("hr",names(paex4), fixed=TRUE)])
data2<-as.data.frame(paex4[grepl("min",names(paex4), fixed=TRUE)])
data<-cbind(data1,data2)


for (i in 1:17 ){                                                              
  
  data[,paste0("time",hr[i])]<-data[,min[i]]*60+data[,hr[i]]*3600
  
} 

time4<-data[,c(36:52)]
time4col<-gsub("hr", "time", colnames(time4))
time4col<-substring(time4col,5)
colnames(time4)<-time4col
timeex4<-cbind(paex4, time4)


########################Create physical activity index (Tan et al., 2016)
paex2$pai_2<-paex2$sleep_2+1.1*paex2$seden_2+1.5*paex2$slightact_2+2.4*paex2$modact_2+5*paex2$heavyact_2
timeex3$pai_3<-paex3$sleep_3+1.1*paex3$seden_3+1.5*paex3$slightact_3+2.4*paex3$modact_3+5*paex3$heavyact_3
timeex4$pai_4<-paex4$sleep_4+1.1*paex4$seden_4+1.5*paex4$slightact_4+2.4*paex4$modact_4+5*paex4$heavyact_4


########################Merge Exam 2 - Exam 4 
df_list <- list(paex2,timeex3,timeex4)
data<-df_list %>% reduce(full_join, by='id')


########################Reorder the columns so that the same activities can be ordered by the number of exams
new_order <- sort(colnames(data))
data1 <- data[, new_order]
data1<-data[,order( colnames(data) )]
data1<- data1 %>%select(id, everything())


########################Place the rest and activity variables at the beginning of the dataset
sleep<-paste0("sleep_",c(2,3,4))
seden<-paste0("seden_",c(2,3,4))
slight<-paste0("slightact_",c(2,3,4))
mod<-paste0("modact_",c(2,3,4))
heavy<-paste0("heavyact_",c(2,3,4))
pai<-paste0("pai_",c(2,3,4))
data2<-data1 %>% relocate(c(id,sleep,seden,slight,mod,heavy,pai), .before =  everything())


########################Remove columns defined with hour, minute and second, and keep the column calculated as total time by seconds
hr_del<-colnames(data2[grepl("hr",names(data2), fixed=TRUE)])[-25]
min_del<-colnames(data2[grepl("min",names(data2), fixed=TRUE)])
data3<-data2[,!colnames(data2) %in% c(hr_del, min_del )]
data3[data3==""]<-NA


########################Convert range 1-3 to 0-2; 1-4 to 0-3; in categorical variables
data3$sithr_4[data3$sithr_4 == "1"]<-"0"
data3$sithr_4[data3$sithr_4 == "2"]<-"1"
data3$sithr_4[data3$sithr_4 == "3"]<-"2"
data3$sithr_4[data3$sithr_4 == "4"]<-"3"


########################Create Framingham ID
data3$framid<-700000+data3$id
data3<- data3 %>% relocate(framid, .before = id)


########################Switch the order for other, weight and walk variables
data3<-data3 %>% relocate(othername_4, .after = other_4)
data3<-data3 %>% relocate(113:117, .after = weightstime_4)
data3<-data3 %>% relocate(walkpace_3, .after = walkblock_2)


########################Switch the order between month and frequency
data3<-data3 %>% relocate(25:26, .before = aerobicfreq_3)
data3<-data3 %>% relocate(33:34, .before = bikefreq_3)
data3<-data3 %>% relocate(41:42, .before = calisthfreq_3)
data3<-data3 %>% relocate(49:50, .before = cyclefreq_3)
data3<-data3 %>% relocate(57:58, .before = dancefreq_3)
data3<-data3 %>% relocate(65:66, .before = gardenfreq_3)
data3<-data3 %>% relocate(73:74, .before = golffreq_3)
data3<-data3 %>% relocate(81:82, .before = hikefreq_3)
data3<-data3 %>% relocate(89:90, .before = householdfreq_3)
data3<-data3 %>% relocate(97:98, .before = jogfreq_3)
data3<-data3 %>% relocate(105:106, .before = lawnfreq_3)
data3<-data3 %>% relocate(lightrecmon_4, .before = lightrecfreq_4)
data3<-data3 %>% relocate(121:122, .before = swimfreq_3)
data3<-data3 %>% relocate(tennismon_4, .before = tennisfreq_4)
data3<-data3 %>% relocate(135:136, .before = walkfreq_3)
data3<-data3 %>% relocate(143:144, .before = weightsfreq_3)
data3<-data3 %>% relocate(othermon_4, .before = otherfreq_4)


########################Create idtype in the dataset
data3$idtype<-"7"
data3<- data3 %>% relocate(idtype, .after = framid)


########################Remove the participants who are not in the curated datasets
gen2<-read_sas("/restricted/projectnb/fhs-bap/BAPData/Curated_datasets/Gen2_Omni1/curated_bap_17_05202024.sas7bdat")
data4<-subset(data3, data3$framid %in% gen2$framid)


########################Add "core" in front of each exam number
colnames(data4)[-c(1:3)]<-gsub("_", "_core", colnames(data4)[-c(1:3)])


########################Transfer "core2" to "core7", "core3" to "core8", "core4" ro "core9"
colnames(data4)[-c(1:3)]<-gsub("_core2", "_core7", colnames(data4)[-c(1:3)])
colnames(data4)[-c(1:3)]<-gsub("_core3", "_core8", colnames(data4)[-c(1:3)])
colnames(data4)[-c(1:3)]<-gsub("_core4", "_core9", colnames(data4)[-c(1:3)])