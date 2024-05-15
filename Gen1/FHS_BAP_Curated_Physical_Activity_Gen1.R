
# ******************************************************************************************************************************************
# Physical Activity data harmonization for FHS Gen 1 cohort
# ******************************************************************************************************************************************
# 
# Created by Chenglin Lyu
# Last updated: 5/3/2024
# 
# 
# The purpose of this R code is to abstract data from questions related to physical activity in Gen 1, exams 12, 19 and 20.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
# e_exam_ex12_0_0060_v1.sas7bdat (Gen 1 Exam 12)
# e_exam_ex19_0_0067.sas7bdat (Gen 1 Exam 19)
# e_exam_ex20_0_0068.sas7bdat (Gen 1 Exam 20)


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
exam12<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/Gen1/e_exam_ex12_0_0060_v1.sas7bdat")
exam19<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/Gen1/e_exam_ex19_0_0067.sas7bdat")
exam20<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/Gen1/e_exam_ex20_0_0068.sas7bdat")


########################Pull out variables related to rest and activity for each exam
exam12pa<-exam12[,colnames(exam12) %in% c("ID",paste0("FE", 186:190))]
exam19pa<-exam19[,colnames(exam19) %in% c("ID","FL087","FL088","FL089","FL090","FL091","FL092","FL093")]
exam20pa<-exam20[,colnames(exam20) %in% c("ID",paste0("FM", 92:98))]


########################Rename sleep and rest activity variables
cov12<-c(	"sleep_12",
          "seden_12",
          "slightact_12",
          "modact_12",
          "heavyact_12")

cov19<-c("sleep_19",
         "seden_19",
         "slightact_19",
         "modact_19",
         "heavyact_19")

cov20<-c("sleep_20",
         "seden_20",
         "slightact_20",
         "modact_20",
         "heavyact_20")

colnames(exam12pa)[-1]<-cov12
colnames(exam19pa)[-c(1:3)]<-cov19
colnames(exam20pa)[-c(1:3)]<-cov20


########################Rename flights of stairs and walking pace variables
colnames(exam19pa)[c(2:3)]<-c("stairs_19","walkblock_19")
colnames(exam20pa)[c(2:3)]<-c("stairs_20","walkblock_20")


########################Create physical activity index (Tan et al., 2016)
exam12pa$pai_12<-exam12pa$sleep_12+1.1*exam12pa$seden_12+1.5*exam12pa$slightact_12+2.4*exam12pa$modact_12+5*exam12pa$heavyact_12
exam19pa$pai_19<-exam19pa$sleep_19+1.1*exam19pa$seden_19+1.5*exam19pa$slightact_19+2.4*exam19pa$modact_19+5*exam19pa$heavyact_19
exam20pa$pai_20<-exam20pa$sleep_20+1.1*exam20pa$seden_20+1.5*exam20pa$slightact_20+2.4*exam20pa$modact_20+5*exam20pa$heavyact_20


########################Merge Exams 12, 19, 20 data
df_list <- list(exam12pa,exam19pa,exam20pa)
data<-df_list %>% reduce(full_join, by='ID')


########################Reorder the columns so that the same activities can be ordered by the number of exams
new_order <- sort(colnames(data))
data1 <- data[, new_order]
data1<-data[,order( colnames(data) )]
data1<- data1 %>%select(ID, everything())


########################Place the rest and activity variables at the beginning of the dataset
sleep<-paste0("sleep_",c(12,19,20))
seden<-paste0("seden_",c(12,19,20))
slight<-paste0("slightact_",c(12,19,20))
mod<-paste0("modact_",c(12,19,20))
heavy<-paste0("heavyact_",c(12,19,20))
pai<-paste0("pai_",c(12,19,20))
data2<-data1 %>% relocate(c(ID,sleep,seden,slight,mod,heavy,pai), .before =  everything())


########################Create framingham id and idtype in the dataset
data2$idtype<-"0"
data2<- data2 %>%select(idtype, everything())
data2$framid<-data2$ID
data2<- data2 %>%select(framid, everything())


########################Covert "ID" to "id"
colnames(data2)[3]<-"id"


########################Add "core" in front of each exam number
colnames(data2)[-c(1:3)]<-gsub("_", "_core", colnames(data2)[-c(1:3)])