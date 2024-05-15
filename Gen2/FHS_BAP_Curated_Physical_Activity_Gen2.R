
# ******************************************************************************************************************************************
# Physical Activity data harmonization for FHS Gen 2 cohort
# ******************************************************************************************************************************************
# 
# Created by Chenglin Lyu
# Last updated: 5/3/2024
# 
# 
# The purpose of this R code is to abstract data from questions related to physical activity in Gen 2, exams 2,4,5,6,7,8,9.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
# e_exam_ex02_1_0080_v1.sas7bdat (Gen 2 Exam 2)
# e_exam_ex04_1_0082.sas7bdat (Gen 2 Exam 4)
# q_physact_ex05_1_0166.sas7bdat (Gen 2 Exam 5)
# q_physact_ex06_1_0165.sas7bdat (Gen 2 Exam 6)
# e_exam_ex07_1_0085_v1.sas7bdat(Gen 2 Exam 7)
# e_exam_ex08_1_0005.sas7bdat (Gen 2 Exam 8)
# ee_exam_ex09_1b_0844.sas7bdat (Gen 2 Exam 9)


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
exam2<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/e_exam_ex02_1_0080_v1.sas7bdat")
exam4<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/e_exam_ex04_1_0082.sas7bdat")
exam5<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/q_physact_ex05_1_0166.sas7bdat")
exam6<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/q_physact_ex06_1_0165.sas7bdat")
exam7<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/e_exam_ex07_1_0085_v1.sas7bdat")
exam8<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/e_exam_ex08_1_0005.sas7bdat")
exam9<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/e_exam_ex09_1b_0844.sas7bdat")


########################Exclude the participants from OMNI 1 cohort at Exam 9
exam9<-subset(exam9, exam9$idtype == 1)


########################Pull out variables related to rest and activity for each exam
restact2<-exam2[,colnames(exam2) %in% c("ID", paste0("B", c(104:108) ))]
restact4<-exam4[,colnames(exam4) %in% c("ID", paste0("D", c(426:430) ))]
restact5<-exam5[,colnames(exam5) %in% c("ID", "EH_10A", "EH_10B","EH_10C","EH_10D","EH_10E" )]
restact7<-exam7[,colnames(exam7) %in% c("ID", paste0("G", c(689:693) ))]
restact8<-exam8[,colnames(exam8) %in% c("ID", paste0("H", c(480:484) ))]
restact9<-exam9[,colnames(exam9) %in% c("id", paste0("j", c(628:632) ))]


########################Pull out variables related to flights of stairs and walking pace for each exam
walk4<-exam4[,colnames(exam4) %in% c("ID", paste0("D", c(424,425) ))]
walk5<-exam5[,colnames(exam5) %in% c("ID", "EH_9A","EH_9B" )]
walk6<-exam6[,colnames(exam6) %in% c("ID", "PA_9A", "PA_9B")]
walk7<-exam7[,colnames(exam7) %in%  c("ID", paste0("G", c(687:688) ))]


########################Pull out variables related to current exercise habit for each exam
habit5<-exam5[,colnames(exam5) %in% c("ID", "EH_1","EH_2","EH_3" )]
habit6<-exam6[,colnames(exam6) %in% c("ID",  paste0("PA_", c(1:3) ))]
habit7<-exam7[,colnames(exam7) %in% c("ID", paste0("G", c(685) ))]


########################Pull out variables related to some specific activities for each exam
colac5<-colnames((exam5[grepl("EH_5",names(exam5), fixed=TRUE)]))
act5<-exam5[,colnames(exam5) %in% c("ID",colac5)]
colac5lastyr<-colnames((exam5[grepl("EH_6",names(exam5), fixed=TRUE)]))
actlastyr5<-exam5[,colnames(exam5) %in% c("ID",colac5lastyr)]
act9<-exam9[,colnames(exam9) %in% c("id", paste0("j", c(638:723) ))]
act8<-exam8[,colnames(exam8) %in% c("ID", paste0("H", c(488:557) ))]
new_order8 <- sort(colnames(act8))
act8 <- act8[, new_order8]
act8<- act8 %>%select(ID, everything())
colwlk5<-colnames((exam5[grepl("EH_7",names(exam5), fixed=TRUE)]))
wlk5<-exam5[,colnames(exam5) %in% c("ID",colwlk5)]
coljg5<-colnames((exam5[grepl("EH_8",names(exam5), fixed=TRUE)]))
jg5<-exam5[,colnames(exam5) %in% c("ID",coljg5)]
sit9<-exam9[,colnames(exam9) %in% c("id",paste0("j", c(633:634) ))]
act9[ , c("j638","j643","j648","j653","j658","j663","j668","j673","j678","j683","j688","j693","j698","j703","j708","j713","j718") ][ act9[ , c("j638","j643","j648","j653","j658","j663","j668","j673","j678","j683","j688","j693","j698","j703","j708","j713","j718")] == "8" ] <- NA
sit9[ , paste0("j",c(633,634)) ][ sit9[ , paste0("j",c(633,634)) ] == "8" ] <- NA


########################Rename sleep rest and activity
colsleep2<-c("sleep_2","seden_2","slightact_2","modact_2","heavyact_2")
colsleep4<-c("sleep_4","seden_4","slightact_4","modact_4","heavyact_4")
colsleep5<-c("sleep_5","seden_5","slightact_5","modact_5","heavyact_5")
colsleep7<-c("sleep_7","seden_7","slightact_7","modact_7","heavyact_7")
colsleep8<-c("sleep_8","seden_8","slightact_8","modact_8","heavyact_8")
colsleep9<-c("sleep_9","seden_9","slightact_9","modact_9","heavyact_9")
colnames(restact2)[-1]<-colsleep2
colnames(restact4)[-1]<-colsleep4
colnames(restact5)[-1]<-colsleep5
colnames(restact7)[-1]<-colsleep7
colnames(restact8)[-1]<-colsleep8
colnames(restact9)[-1]<-colsleep9


########################Rename flights of stairs and walking pace
colnames(walk4)[-1]<-c("stairs_4","walkblock_4")
colnames(walk5)[-1]<-c("stairs_5","walkblock_5")
colnames(walk6)[-1]<-c("stairs_6","walkblock_6")
colnames(walk7)[-1]<-c("stairs_7","walkblock_7")


########################Rename current exercise habit
colnames(habit5)[-1]<-c("intenseactweek_5","lastweekact_5","actlevel_5")
colnames(habit6)[-1]<-c("intenseactweek_6","lastweekact_6","actlevel_6")
colnames(habit7)[-1]<-c("intenseactweek_7")


########################Rename sitting activities
colnames(sit9)[2]<-"sitfreq_9"
colnames(sit9)[3]<-"sithr_9"


########################Rename last 7 days activities for Exam 5 
colname5<-c("act7day1_5",
            "act7hr1_5",
            "act7min1_5",
            "act7day2_5",
            "act7hr2_5",
            "act7min2_5",
            "act7day3_5",
            "act7hr3_5",
            "act7min3_5",
            "act7day4_5",
            "act7hr4_5",
            "act7min4_5",
            "act7day5_5",
            "act7hr5_5",
            "act7min5_5",
            "act7day6_5",
            "act7hr6_5",
            "act7min6_5")

colnames(act5)[-1]<-colname5


########################Rename last year activities for Exam 5 
colname5lastyr<-c("act1yr1_5",
                  "act1hr1_5",
                  "act1min1_5",
                  "actnumwk1_5",
                  "act1yr2_5",
                  "act1hr2_5",
                  "act1min2_5",
                  "actnumwk2_5",
                  "act1yr3_5",
                  "act1hr3_5",
                  "act1min3_5",
                  "actnumwk3_5",
                  "act1yr4_5",
                  "act1hr4_5",
                  "act1min4_5",
                  "actnumwk4_5",
                  "act1yr5_5",
                  "act1hr5_5",
                  "act1min5_5",
                  "actnumwk5_5",
                  "act1yr6_5",
                  "act1hr6_5",
                  "act1min6_5",
                  "actnumwk6_5",
                  "act1yr7_5",
                  "act1hr7_5",
                  "act1min7_5",
                  "actnumwk7_5",
                  "act1yr8_5",
                  "act1hr8_5",
                  "act1min8_5",
                  "actnumwk8_5",
                  "act1yr9_5",
                  "act1hr9_5",
                  "act1min9_5",
                  "actnumwk9_5",
                  "act1yr10_5",
                  "act1hr10_5",
                  "act1min10_5",
                  "actnumwk10_5",
                  "act1yr11_5",
                  "act1hr11_5",
                  "act1min11_5",
                  "actnumwk11_5",
                  "act1yr12_5",
                  "act1hr12_5",
                  "act1min12_5",
                  "actnumwk12_5",
                  "act1yr13_5",
                  "act1hr13_5",
                  "act1min13_5",
                  "actnumwk13_5",
                  "act1yr14_5",
                  "act1hr14_5",
                  "act1min14_5",
                  "actnumwk14_5",
                  "act1yr15_5",
                  "act1hr15_5",
                  "act1min15_5",
                  "actnumwk15_5",
                  "act1yr16_5",
                  "act1hr16_5",
                  "act1min16_5",
                  "actnumwk16_5",
                  "act1yr17_5",
                  "act1hr17_5",
                  "act1min17_5",
                  "actnumwk17_5")

colnames(actlastyr5)[-1]<-colname5lastyr


########################Rename activities in the past year for Exam 8 
colname8<-c("walk_8",
            "walkfreq2_8",
            "walkhr_8",
            "walkmin_8",
            "walkmon_8",
            "calisth_8",
            "calisthfreq_8",
            "calisthhr_8",
            "calisthmin_8",
            "calisthmon_8",
            "household_8",
            "householdfreq_8",
            "householdhr_8",
            "householdmin_8",
            "householdmon_8",
            "lawn_8",
            "lawnfreq_8",
            'lawnhr_8',
            "lawnmin_8",
            "lawnmon_8",
            "garden_8",
            "gardenfreq_8",
            "gardenhr_8",
            "gardenmin_8",
            "gardenmon_8",
            "hike_8",
            "hikefreq_8",
            "hikehr_8",
            "hikemin_8",
            "hikemon_8",
            "jog_8",
            "jogfreq2_8",
            "joghr_8",
            "jogmin_8",
            "jogmon_8",
            "bike_8",
            "bikefreq_8",
            "bikehr_8",
            "bikemin_8",
            "bikemon_8",
            "cycle_8",
            "cyclefreq_8",
            "cyclehr_8",
            "cyclemin_8",
            "cyclemon_8",
            "dance_8",
            "dancefreq_8",
            "dancehr_8",
            "dancemin_8",
            "dancemon_8",
            "aerobic_8",
            "aerobicfreq_8",
            "aerobichr_8",
            "aerobicmin_8",
            "aerobicmon_8",
            "golf_8",
            "golffreq_8",
            "golfhr_8",
            "golfmin_8",
            "golfmon_8",
            "swim_8",
            "swimfreq_8",
            "swimhr_8",
            "swimmin_8",
            "swimmon_8",
            "weights_8",
            "weightsfreq_8",
            "weightshr_8",
            "weightsmin_8",
            "weightsmon_8")

colnames(act8)[-1]<-colname8


########################Convert any activity variable value equals to "8" to "NA"
act8$hike_8[act8$hike_8 == "8"]<-NA
act8$jog_8[act8$jog_8 == "8"]<-NA
act8$bike_8[act8$bike_8 == "8"]<-NA
act8$cycle_8[act8$cycle_8 == "8"]<-NA
act8$dance_8[act8$dance_8 == "8"]<-NA
act8$aerobic_8[act8$aerobic_8 == "8"]<-NA
act8$golf_8[act8$golf_8 == "8"]<-NA
act8$swim_8[act8$swim_8 == "8"]<-NA
act8$weights_8[act8$weights_8 == "8"]<-NA


########################Rename activities in the past year for Exam 9
colname9<-c("walk_9",
            "walkfreq2_9",
            "walkhr_9",
            "walkmin_9",
            "walkmon_9",
            "calisth_9",
            "calisthfreq_9",
            "calisthhr_9",
            "calisthmin_9",
            "calisthmon_9",
            "cycle_9",
            "cyclefreq_9",
            "cyclehr_9",
            "cyclemin_9",
            "cyclemon_9",
            "weights_9",
            "weightsfreq_9",
            "weightshr_9",
            "weightsmin_9",
            "weightsmon_9",
            "household_9",
            "householdfreq_9",
            "householdhr_9",
            "householdmin_9",
            "householdmon_9",
            "jog_9",
            "jogfreq2_9",
            "joghr_9",
            "jogmin_9",
            "jogmon_9",
            "bike_9",
            "bikefreq_9",
            "bikehr_9",
            "bikemin_9",
            "bikemon_9",
            "dance_9",
            "dancefreq_9",
            "dancehr_9",
            "dancemin_9",
            "dancemon_9",
            "aerobic_9",
            "aerobicfreq_9",
            "aerobichr_9",
            "aerobicmin_9",
            "aerobicmon_9",
            "swim_9",
            "swimfreq_9",
            "swimhr_9",
            "swimmin_9",
            "swimmon_9",
            "tennis_9",
            "tennisfreq_9",
            "tennishr_9",
            "tennismin_9",
            "tennismon_9",
            "golf_9",
            "golffreq_9",
            "golfhr_9",
            "golfmin_9",
            "golfmon_9",
            "lawn_9",
            "lawnfreq_9",
            "lawnhr_9",
            "lawnmin_9",
            "lawnmon_9",
            "garden_9",
            "gardenfreq_9",
            "gardenhr_9",
            "gardenmin_9",
            "gardenmon_9",
            "hike_9",
            "hikefreq_9",
            "hikehr_9",
            "hikemin_9",
            "hikemon_9",
            "lightrec_9",
            "lightrecfreq_9",
            "lightrechr_9",
            "lightrecmin_9",
            "lightrecmon_9",
            "other_9",
            "othername_9",
            "otherfreq_9",
            "otherhr_9",
            "othermin_9",
            "othermon_9")

colnames(act9)[-1]<-colname9


########################Rename Walk & Jog activities for Exam 5 
colwlk5<-c("walk_5",
           "walkmile_5",
           "walkmin_5",
           "walksec_5",
           "walkfreq1_5")

colnames(wlk5)[-1]<-colwlk5


coljog5<-c("jog_5",
           "jogmile_5",
           "jogmin_5",
           "jogsec_5",
           "jogfreq1_5")

colnames(jg5)[-1]<-coljog5


########################Merge by exams
paex2 <-restact2
paex4<-merge(restact4, walk4, by = "ID")
ex5list <- list(restact5, walk5, wlk5, jg5, habit5, act5, actlastyr5)
paex5 <-ex5list %>% reduce(full_join, by='ID')
ex6list <- list(walk6, habit6)
paex6 <-ex6list %>% reduce(full_join, by='ID')
ex7list <- list(restact7, walk7, habit7)
paex7 <-ex7list %>% reduce(full_join, by='ID')
paex8 <-merge(restact8, act8, by = "ID")
ex9list <- list(restact9,act9, sit9)
paex9 <-ex9list %>% reduce(full_join, by='id')


########################Calculate total time for Exam 5
hr<-colnames(paex5[grepl("hr",names(paex5), fixed=TRUE)])                      #pick up variable name including hour
min<-colnames(paex5[grepl("min",names(paex5), fixed=TRUE)])[-c(1,2)]           #pick up variable name including minute
data1<-as.data.frame(paex5[grepl("hr",names(paex5), fixed=TRUE)])              #create a dataset by picking up columns named including hour 
data2<-as.data.frame(paex5[grepl("min",names(paex5), fixed=TRUE)])[-c(1,2)]    #create a dataset by picking up columns named including minute 
data<-cbind(data1,data2)                                                       #create a dataset with only variables with hour and minutes

for (i in 1:23 ){                                                              #transfer hour and minute to second and create anew variable as total time by adding up hour and minute
  
  data[,paste0("time",hr[i])]<-data[,min[i]]*60+data[,hr[i]]*3600
  
} 

time5<-data[,c(47:69)]
time5col<-gsub("hr", "time", colnames(time5))
time5col<-substring(time5col,5)
colnames(time5)<-time5col
timeex5<-cbind(paex5, time5)
timeex5[,"jogtime_5"]<-timeex5$jogmin_5*60+timeex5$jogsec_5                     #transfer hour and minute to second and create anew variable as total time by adding up hour and minute for jog
timeex5[,"walktime_5"]<-timeex5$walkmin_5*60+timeex5$walksec_5                  #transfer hour and minute to second and create anew variable as total time by adding up hour and minute for walk


########################Calculate total time for Exam 8
hr<-colnames(paex8[grepl("hr",names(paex8), fixed=TRUE)])
min<-colnames(paex8[grepl("min",names(paex8), fixed=TRUE)])
data1<-as.data.frame(paex8[grepl("hr",names(paex8), fixed=TRUE)])
data2<-as.data.frame(paex8[grepl("min",names(paex8), fixed=TRUE)])
data<-cbind(data1,data2)

for (i in 1:14 ){                                                              
  
  data[,paste0("time",hr[i])]<-data[,min[i]]*60+data[,hr[i]]*3600
  
} 

time8<-data[,c(29:42)]
time8col<-gsub("hr", "time", colnames(time8))
time8col<-substring(time8col,5)
colnames(time8)<-time8col
timeex8<-cbind(paex8, time8)


########################Calculate total time for Exam 9
hr<-colnames(paex9[grepl("hr",names(paex9), fixed=TRUE)])[-18]
min<-colnames(paex9[grepl("min",names(paex9), fixed=TRUE)])
data1<-as.data.frame(paex9[grepl("hr",names(paex9), fixed=TRUE)])[-18]
data2<-as.data.frame(paex9[grepl("min",names(paex9), fixed=TRUE)])
data<-cbind(data1,data2)

for (i in 1:17 ){                                                              
  
  data[,paste0("time",hr[i])]<-data[,min[i]]*60+data[,hr[i]]*3600
  
} 

time9<-data[,c(35:51)]
time9col<-gsub("hr", "time", colnames(time9))
time9col<-substring(time9col,5)
colnames(time9)<-time9col
timeex9<-cbind(paex9, time9)


########################Create physical activity index (Tan et al., 2016)
paex2$pai_2<-paex2$sleep_2+1.1*paex2$seden_2+1.5*paex2$slightact_2+2.4*paex2$modact_2+5*paex2$heavyact_2
paex4$pai_4<-paex4$sleep_4+1.1*paex4$seden_4+1.5*paex4$slightact_4+2.4*paex4$modact_4+5*paex4$heavyact_4
timeex5$pai_5<-paex5$sleep_5+1.1*paex5$seden_5+1.5*paex5$slightact_5+2.4*paex5$modact_5+5*paex5$heavyact_5
paex7$pai_7<-paex7$sleep_7+1.1*paex7$seden_7+1.5*paex7$slightact_7+2.4*paex7$modact_7+5*paex7$heavyact_7
timeex8$pai_8<-paex8$sleep_8+1.1*paex8$seden_8+1.5*paex8$slightact_8+2.4*paex8$modact_8+5*paex8$heavyact_8
timeex9$pai_9<-paex9$sleep_9+1.1*paex9$seden_9+1.5*paex9$slightact_9+2.4*paex9$modact_9+5*paex9$heavyact_9
colnames(timeex9)[colnames(timeex9) == "id"]<-"ID"


########################Merge Exam 2 - Exam 9 by ID and remove duplicated rows
df_list <- list(paex2,paex4,timeex5,paex6,paex7,timeex8,timeex9)
data<-df_list %>% reduce(full_join, by='ID')
data<-subset(data, !duplicated(data$ID ))


########################Reorder the columns so that the same activities can be ordered by the number of exams
new_order <- sort(colnames(data))
data1 <- data[, new_order]
data1<-data[,order( colnames(data) )]
data1<- data1 %>%select(ID, everything())


########################Make the rest and activity variables as the most front of the dataset
sleep<-paste0("sleep_",c(2,4,5,7,8,9))
seden<-paste0("seden_",c(2,4,5,7,8,9))
slight<-paste0("slightact_",c(2,4,5,7,8,9))
mod<-paste0("modact_",c(2,4,5,7,8,9))
heavy<-paste0("heavyact_",c(2,4,5,7,8,9))
pai<-paste0("pai_",c(2,4,5,7,8,9))
data2<-data1 %>% relocate(c(ID,sleep,seden,slight,mod,heavy,pai), .before =  everything())


########################Create Framingham ID
data2$framid<-80000+data2$ID
data2<- data2 %>%select(framid, everything())


########################Remove columns defined with hour, min and second, keep the column calculated as total time by second
hr_del<-colnames(data2[grepl("hr",names(data2), fixed=TRUE)])[-48]
min_del<-colnames(data2[grepl("min",names(data2), fixed=TRUE)])
data3<-data2[,!colnames(data2) %in% c(hr_del, min_del, "jogsec_5"  ,  "walksec_5"  )]
data3[data3==""]<-NA


########################Create supplemental dataset
othercol<-colnames(data3[grepl("other",names(data3), fixed=TRUE)])
act1timecol<-colnames(data3[grepl("act1time",names(data3), fixed=TRUE)])
act7timecol<-colnames(data3[grepl("act7time",names(data3), fixed=TRUE)])
datasup<-data3[,colnames(data3) %in% c("framid","ID", colname5, colname5lastyr,act1timecol,act7timecol, othercol)]
datasup<-datasup[,sort(colnames(datasup))] 
datasup<- datasup %>%select(framid,ID, everything())
colsup<-colnames(datasup)[-c(1,2)]
data4<-data3[,!colnames(data3) %in% colsup]


########################Get rid of the ID variable, may add them and type in the future
#data5<-data4[,!colnames(data4) %in% "ID"]
#datasup<-datasup[,!colnames(datasup) %in% "ID"]


########################Reorder some positions of variables supplemental dataset
datasup<-datasup %>% relocate(4:11, .after = act1time9_5)
datasup<-datasup %>% relocate(21:28, .after = act1yr9_5)
datasup<-datasup %>% relocate(50:57, .after = actnumwk9_5)
datasup<-datasup %>% relocate(20:42, .after = ID)
datasup<-datasup %>% relocate(49:65, .after = act7day6_5)
datasup<-datasup %>% relocate(othername_9, .after = other_9)
datasup<-datasup %>% relocate(20:25, .after = act1time17_5)
datasup<-datasup %>% relocate(otherfreq_9, .after = othermon_9)


########################Move lastweekact_5 and lastweekact_6 come after actlevel_6
data5<-data4 %>% relocate(lastweekact_5,lastweekact_6, .after = actlevel_6)
data5<-data5 %>% relocate(intenseactweek_5,intenseactweek_6, intenseactweek_7, .after = lastweekact_6)


########################Convert range 1-3 to 0-2; 1-4 to 0-3; in categorical variables
data5$actlevel_5[data5$actlevel_5 == "1"]<-"0"
data5$actlevel_5[data5$actlevel_5 == "2"]<-"1"
data5$actlevel_5[data5$actlevel_5 == "3"]<-"2"

data5$actlevel_6[data5$actlevel_6 == "1"]<-"0"
data5$actlevel_6[data5$actlevel_6 == "2"]<-"1"
data5$actlevel_6[data5$actlevel_6 == "3"]<-"2"

data5$lastweekact_5[data5$lastweekact_5 == "1"]<-"0"
data5$lastweekact_5[data5$lastweekact_5 == "2"]<-"1"
data5$lastweekact_5[data5$lastweekact_5 == "3"]<-"2"

data5$lastweekact_6[data5$lastweekact_6 == "1"]<-"0"
data5$lastweekact_6[data5$lastweekact_6 == "2"]<-"1"
data5$lastweekact_6[data5$lastweekact_6 == "3"]<-"2"

data5$sithr_9[data5$sithr_9 == "1"]<-"0"
data5$sithr_9[data5$sithr_9 == "2"]<-"1"
data5$sithr_9[data5$sithr_9 == "3"]<-"2"
data5$sithr_9[data5$sithr_9 == "4"]<-"3"


########################Rename time variables at Exam 8 and Exam 9
colnames(data5)[colnames(data5) == "jogtime_8"]<-"jogtime2_8"
colnames(data5)[colnames(data5) == "jogtime_9"]<-"jogtime2_9"
colnames(data5)[colnames(data5) == "walktime_8"]<-"walktime2_8"
colnames(data5)[colnames(data5) == "walktime_9"]<-"walktime2_9"


########################Switch the order between month and frequency
data5<-data5 %>% relocate(50:51, .before = aerobicfreq_8)
data5<-data5 %>% relocate(58:59, .before = bikefreq_8)
data5<-data5 %>% relocate(66:67, .before = calisthfreq_8)
data5<-data5 %>% relocate(74:75, .before = cyclefreq_8)
data5<-data5 %>% relocate(82:83, .before = dancefreq_8)
data5<-data5 %>% relocate(90:91, .before = gardenfreq_8)
data5<-data5 %>% relocate(98:99, .before = golffreq_8)
data5<-data5 %>% relocate(106:107, .before = hikefreq_8)
data5<-data5 %>% relocate(114:115, .before = householdfreq_8)
data5<-data5 %>% relocate(125:126, .before = jogfreq1_5)
data5<-data5 %>% relocate(134:135, .before = lawnfreq_8)
data5<-data5 %>% relocate(lightrecmon_9, .before = lightrecfreq_9)
data5<-data5 %>% relocate(152:153, .before = swimfreq_8)
data5<-data5 %>% relocate(tennismon_9, .before = tennisfreq_9)
data5<-data5 %>% relocate(171:172, .before = walkfreq1_5)
data5<-data5 %>% relocate(180:181, .before = weightsfreq_8)


########################move jogmile_5 after jog_9 and move walkmile_5 after walk_9
data5<-data5 %>% relocate(jogmile_5, .after = jog_9)
data5<-data5 %>% relocate(walkmile_5, .after = walk_9)


########################Create idtype in the dataset
data5$idtype<-"1"
data5<- data5 %>% relocate(idtype, .after = framid)
datasup$idtype<-"1"
datasup<- datasup %>% relocate(idtype, .after = framid)


########################Remove the participants who are not in the curated datasets
gen2<-read_sas("/restricted/projectnb/fhs-bap/BAPData/Curated_datasets/Gen2_Omni1/curated_bap_17_04122024.sas7bdat")
data6<-subset(data5, data5$framid %in% gen2$framid)
datasup<-subset(datasup, datasup$framid %in% gen2$framid)


########################Add "core" in front of each exam number
colnames(data6)[-c(1:3)]<-gsub("_", "_core", colnames(data6)[-c(1:3)])
colnames(datasup)[-c(1:3)]<-gsub("_", "_core", colnames(datasup)[-c(1:3)])