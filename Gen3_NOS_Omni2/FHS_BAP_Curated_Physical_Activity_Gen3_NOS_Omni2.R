# ******************************************************************************************************************************************
# Physical Activity data harmonization for FHS Gen 3, OMNI 2 and NOS cohort
# ******************************************************************************************************************************************
# 
# Created by Chenglin Lyu
# Last updated: 5/3/2024
# 
# 
# The purpose of this R code is to abstract data from questions related to physical activity in exams 1,2 of Gen 3, NOS, OMNI 2 respectively.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
# e_exam_ex01_3_0086_v2.sas7bdat (Gen 3 Exam 1)
# e_exam_ex01_2_0813.sas7bdat (NOS Exam 1)
# e_exam_ex01_72_0652.sas7bdat (OMNI 2 Exam 1)
# e_exam_2011_m_0017_v1.sas7bdat (Gen 3, NOS, OMNI 2 Exams 2)

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
library(data.table)


########################Input original data from Y drive
gen3_ex1<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/gen3/e_exam_ex01_3_0086_v2.sas7bdat")
gen3_ex1<- gen3_ex1 %>%select(c(ID), everything())
colnames(gen3_ex1)[1]<-"id"
omni2_ex1<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/gen3/e_exam_ex01_72_0652.sas7bdat")
nos_ex1<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/gen3/e_exam_ex01_2_0813.sas7bdat")
ex2<-read_sas("/restricted/projectnb/fhs-bap/Data_core (Administrative purposes)/Data Core Analysts/Chenglin/physical_activity/data/gen3/e_exam_2011_m_0017_v1.sas7bdat")
nos_ex2<-subset(ex2, ex2$idtype == 2)
gen3_ex2<-subset(ex2, ex2$idtype == 3)
omni2_ex2<-subset(ex2, ex2$idtype == 72)


########################Pull out variables related to rest and activity for each exam and each cohort
gen3_ex1pa<-gen3_ex1[,colnames(gen3_ex1) %in% c("id",paste0("G3A", c(596:602)))]
gen3_ex2pa<-gen3_ex2[,colnames(gen3_ex2) %in% c("id",paste0("g3b", c("0703","0704","0705","0706","0707")))]

nos_ex1pa<-nos_ex1[,colnames(nos_ex1) %in% c("id",paste0("g3a", c(596:602)))]
nos_ex2pa<-nos_ex2[,colnames(nos_ex2) %in% c("id",paste0("g3b", c("0703","0704","0705","0706","0707")))]

omni2_ex1pa<-omni2_ex1[,colnames(omni2_ex1) %in% c("id",paste0("g3a", c(596:602)))]
omni2_ex2pa<-omni2_ex2[,colnames(omni2_ex2) %in% c("id",paste0("g3b", c("0703","0704","0705","0706","0707")))]


########################Rename sleep, rest, and activity variables
gen3_ex1_rest<-c(	"sleep_1",
                  "seden_1",
                  "slightact_1",
                  "modact_1",
                  "heavyact_1")

nos_ex1_rest<-c(	"sleep_1",
                 "seden_1",
                 "slightact_1",
                 "modact_1",
                 "heavyact_1")

omni2_ex1_rest<-c("sleep_1",
                  "seden_1",
                  "slightact_1",
                  "modact_1",
                  "heavyact_1")

gen3_ex2_rest<-c("sleep_2",
                 "seden_2",
                 "slightact_2",
                 "modact_2",
                 "heavyact_2")

nos_ex2_rest<-c("sleep_2",
                "seden_2",
                "slightact_2",
                "modact_2",
                "heavyact_2")

omni2_ex2_rest<-c("sleep_2",
                  "seden_2",
                  "slightact_2",
                  "modact_2",
                  "heavyact_2")

colnames(gen3_ex1pa)[c(2:6)]<-gen3_ex1_rest
colnames(gen3_ex2pa)[-1]<-gen3_ex2_rest
colnames(nos_ex1pa)[c(2:6)]<-nos_ex1_rest
colnames(nos_ex2pa)[-1]<-nos_ex2_rest
colnames(omni2_ex1pa)[c(2:6)]<-omni2_ex1_rest
colnames(omni2_ex2pa)[c(-1)]<-omni2_ex2_rest


########################Create physical activity index (Tan et al., 2016)
gen3_ex1pa$pai_1<-gen3_ex1pa$sleep_1+1.1*gen3_ex1pa$seden_1+1.5*gen3_ex1pa$slightact_1+2.4*gen3_ex1pa$modact_1+5*gen3_ex1pa$heavyact_1
gen3_ex2pa$pai_2<-gen3_ex2pa$sleep_2+1.1*gen3_ex2pa$seden_2+1.5*gen3_ex2pa$slightact_2+2.4*gen3_ex2pa$modact_2+5*gen3_ex2pa$heavyact_2
nos_ex1pa$pai_1<-nos_ex1pa$sleep_1+1.1*nos_ex1pa$seden_1+1.5*nos_ex1pa$slightact_1+2.4*nos_ex1pa$modact_1+5*nos_ex1pa$heavyact_1
nos_ex2pa$pai_2<-nos_ex2pa$sleep_2+1.1*nos_ex2pa$seden_2+1.5*nos_ex2pa$slightact_2+2.4*nos_ex2pa$modact_2+5*nos_ex2pa$heavyact_2
omni2_ex1pa$pai_1<-omni2_ex1pa$sleep_1+1.1*omni2_ex1pa$seden_1+1.5*omni2_ex1pa$slightact_1+2.4*omni2_ex1pa$modact_1+5*omni2_ex1pa$heavyact_1
omni2_ex2pa$pai_2<-omni2_ex2pa$sleep_2+1.1*omni2_ex2pa$seden_2+1.5*omni2_ex2pa$slightact_2+2.4*omni2_ex2pa$modact_2+5*omni2_ex2pa$heavyact_2


########################Rename walk & stair activities from Exam 1
colnames(gen3_ex1pa)[c(7,8)]<-c("walkpace_1","stairs_1")
colnames(nos_ex1pa)[c(7,8)]<-c("walkpace_1","stairs_1")
colnames(omni2_ex1pa)[c(7,8)]<-c("walkpace_1","stairs_1")


########################Extract and rename activities in the past year or 12 months
gen3_ex1_act<-gen3_ex1[,colnames(gen3_ex1) %in% c("id",paste0("G3A", c(603:612)))]
gen3_ex2_act<-gen3_ex2[,colnames(gen3_ex2) %in% c("id",paste0("g3b0", c(711:726,729:748,751:770,773:775,778:782)))]

nos_ex1_act<-nos_ex1[,colnames(nos_ex1) %in% c("id",paste0("g3a", c(603:612)))]
nos_ex2_act<-nos_ex2[,colnames(nos_ex2) %in% c("id",paste0("g3b0", c(711:726,729:748,751:770,773:775,778:782)))]

omni2_ex1_act<-omni2_ex1[,colnames(omni2_ex1) %in% c("id",paste0("g3a", c(603:612)))]
omni2_ex2_act<-omni2_ex2[,colnames(omni2_ex2) %in% c("id",paste0("g3b0", c(711:726,729:748,751:770,773:775,778:782)))]

colnam_gen3_ex1<-c("walk_1",
                   "jog_1",
                   "run_1",
                   "bike_1",
                   "tennis_1",
                   "swim_1",
                   "aerobic_1",
                   "lowintense_1",
                   "vigex_1",
                   "weights_1")

colnam_gen3_ex2<-c(	"jog_2",
                    "jogtime_2",
                    "jogfreq_2",
                    "jogmon_2",
                    "vigsport_2",
                    "vigsportmon_2", 
                    "vigsportfreq_2", 
                    "vigsporttime_2",   
                    "bike_2",
                    "bikemon_2",
                    "bikefreq_2",
                    "biketime_2",
                    "swim_2",
                    "swimmon_2",
                    "swimfreq_2",
                    "swimtime_2",
                    "vigclass_2",   
                    "vigclassmon_2",
                    "vigclassfreq_2",
                    "vigclasstime_2",
                    "vigjob_2",
                    "vigjobmon_2",
                    "vigjobfreq_2",
                    "vigjobtime_2",
                    "homeact_2",
                    "homeactmon_2",
                    "homeactfreq_2",
                    "homeacttime_2",
                    "strensport_2",
                    "strensportmon_2",
                    "strensportfreq_2",
                    "strensporttime_2",
                    "nonstren_2",  
                    "nonstrenmon_2",
                    "nonstrenfreq_2",
                    "nonstrentime_2",
                    "walkwork_2",  
                    "walkworkmon_2",
                    "walkworkfreq_2",
                    "walkworktime_2",
                    "bowl_2",
                    "bowlmon_2",
                    "bowlfreq_2",
                    "bowltime_2",
                    "calisth_2",
                    "calisthmon_2",
                    "calisthfreq_2",
                    "calisthtime_2",
                    "homemaintain_2",
                    "homemaintainmon_2",
                    "homemaintainfreq_2",
                    "homemaintaintime_2",
                    "weights_2",
                    "weightsmon_2",
                    "weightsfreq_2",
                    "weightstime_2",
                    "modex18_2",
                    "modex35_2",
                    "modex50_2",
                    "sitatwork_2", 
                    "standatwork_2", 
                    "walkatwork_2", 
                    "sitleisure_2",  
                    "compleisure_2")

colnam_nos_ex1<-c("walk_1",
                  "jog_1",
                  "run_1",
                  "bike_1",
                  "tennis_1",
                  "swim_1",
                  "aerobic_1",
                  "lowintense_1",
                  "vigex_1",
                  "weights_1")

colnam_nos_ex2<-c( "jog_2",
                   "jogtime_2",
                   "jogfreq_2",
                   "jogmon_2",
                   "vigsport_2",
                   "vigsportmon_2", 
                   "vigsportfreq_2", 
                   "vigsporttime_2",   
                   "bike_2",
                   "bikemon_2",
                   "bikefreq_2",
                   "biketime_2",
                   "swim_2",
                   "swimmon_2",
                   "swimfreq_2",
                   "swimtime_2",
                   "vigclass_2",   
                   "vigclassmon_2",
                   "vigclassfreq_2",
                   "vigclasstime_2",
                   "vigjob_2",
                   "vigjobmon_2",
                   "vigjobfreq_2",
                   "vigjobtime_2",
                   "homeact_2",
                   "homeactmon_2",
                   "homeactfreq_2",
                   "homeacttime_2",
                   "strensport_2",
                   "strensportmon_2",
                   "strensportfreq_2",
                   "strensporttime_2",
                   "nonstren_2",  
                   "nonstrenmon_2",
                   "nonstrenfreq_2",
                   "nonstrentime_2",
                   "walkwork_2",  
                   "walkworkmon_2",
                   "walkworkfreq_2",
                   "walkworktime_2",
                   "bowl_2",
                   "bowlmon_2",
                   "bowlfreq_2",
                   "bowltime_2",
                   "calisth_2",
                   "calisthmon_2",
                   "calisthfreq_2",
                   "calisthtime_2",
                   "homemaintain_2",
                   "homemaintainmon_2",
                   "homemaintainfreq_2",
                   "homemaintaintime_2",
                   "weights_2",
                   "weightsmon_2",
                   "weightsfreq_2",
                   "weightstime_2",
                   "modex18_2",
                   "modex35_2",
                   "modex50_2",
                   "sitatwork_2", 
                   "standatwork_2", 
                   "walkatwork_2", 
                   "sitleisure_2",  
                   "compleisure_2")

colnam_omni2_ex1<-c("walk_1",
                    "jog_1",
                    "run_1",
                    "bike_1",
                    "tennis_1",
                    "swim_1",
                    "aerobic_1",
                    "lowintense_1",
                    "vigex_1",
                    "weights_1")

colnam_omni2_ex2<-c( "jog_2",
                     "jogtime_2",
                     "jogfreq_2",
                     "jogmon_2",
                     "vigsport_2",
                     "vigsportmon_2", 
                     "vigsportfreq_2", 
                     "vigsporttime_2",   
                     "bike_2",
                     "bikemon_2",
                     "bikefreq_2",
                     "biketime_2",
                     "swim_2",
                     "swimmon_2",
                     "swimfreq_2",
                     "swimtime_2",
                     "vigclass_2",   
                     "vigclassmon_2",
                     "vigclassfreq_2",
                     "vigclasstime_2",
                     "vigjob_2",
                     "vigjobmon_2",
                     "vigjobfreq_2",
                     "vigjobtime_2",
                     "homeact_2",
                     "homeactmon_2",
                     "homeactfreq_2",
                     "homeacttime_2",
                     "strensport_2",
                     "strensportmon_2",
                     "strensportfreq_2",
                     "strensporttime_2",
                     "nonstren_2",  
                     "nonstrenmon_2",
                     "nonstrenfreq_2",
                     "nonstrentime_2",
                     "walkwork_2",  
                     "walkworkmon_2",
                     "walkworkfreq_2",
                     "walkworktime_2",
                     "bowl_2",
                     "bowlmon_2",
                     "bowlfreq_2",
                     "bowltime_2",
                     "calisth_2",
                     "calisthmon_2",
                     "calisthfreq_2",
                     "calisthtime_2",
                     "homemaintain_2",
                     "homemaintainmon_2",
                     "homemaintainfreq_2",
                     "homemaintaintime_2",
                     "weights_2",
                     "weightsmon_2",
                     "weightsfreq_2",
                     "weightstime_2",
                     "modex18_2",
                     "modex35_2",
                     "modex50_2",
                     "sitatwork_2", 
                     "standatwork_2", 
                     "walkatwork_2", 
                     "sitleisure_2",  
                     "compleisure_2")

colnames(gen3_ex1_act)[-1]<-colnam_gen3_ex1
colnames(gen3_ex2_act)[-1]<-colnam_gen3_ex2
colnames(nos_ex1_act)[-1]<-colnam_nos_ex1
colnames(nos_ex2_act)[-1]<-colnam_nos_ex2
colnames(omni2_ex1_act)[-1]<-colnam_omni2_ex1
colnames(omni2_ex2_act)[-1]<-colnam_omni2_ex2


#Convert total time from minutes to seconds
timegen3ex2<-colnames(gen3_ex2_act[grepl("time",names(gen3_ex2_act), fixed=TRUE)])
for ( i in timegen3ex2){
  gen3_ex2_act[,colnames(gen3_ex2_act) %in% i]<-gen3_ex2_act[,colnames(gen3_ex2_act) %in% i]*60
}

timenosex2<-colnames(nos_ex2_act[grepl("time",names(nos_ex2_act), fixed=TRUE)])
for ( i in timenosex2){
  nos_ex2_act[,colnames(nos_ex2_act) %in% i]<-nos_ex2_act[,colnames(nos_ex2_act) %in% i]*60
}

timeomni2ex2<-colnames(omni2_ex2_act[grepl("time",names(omni2_ex2_act), fixed=TRUE)])
for ( i in timeomni2ex2){
  omni2_ex2_act[,colnames(omni2_ex2_act) %in% i]<-omni2_ex2_act[,colnames(omni2_ex2_act) %in% i]*60
}


########################Merge by exams
df_list_gen3 <- list(gen3_ex1pa,gen3_ex1_act,gen3_ex2pa,gen3_ex2_act)
gen3_pa<-df_list_gen3 %>% reduce(full_join, by='id')

df_list_nos <- list(nos_ex1pa,nos_ex1_act,nos_ex2pa,nos_ex2_act)
nos_pa<-df_list_nos %>% reduce(full_join, by='id')

df_list_omni2 <- list(omni2_ex1pa,omni2_ex1_act,omni2_ex2pa,omni2_ex2_act)
omni2_pa<-df_list_omni2 %>% reduce(full_join, by='id')


########################Reorder the columns so that the same activities can be ordered by the number of exams
new_order <- sort(colnames(gen3_pa))
gen3_pa <- gen3_pa[, new_order]
gen3_pa<-gen3_pa[,order( colnames(gen3_pa) )]
gen3_pa<- gen3_pa %>%select(id, everything())

new_order <- sort(colnames(nos_pa))
nos_pa <- nos_pa[, new_order]
nos_pa<-nos_pa[,order( colnames(nos_pa) )]
nos_pa<- nos_pa %>%select(id, everything())

new_order <- sort(colnames(omni2_pa))
omni2_pa <- omni2_pa[, new_order]
omni2_pa<-omni2_pa[,order( colnames(omni2_pa) )]
omni2_pa<- omni2_pa %>%select(id, everything())


########################Place the rest and activity variables at the beginning of the dataset
sleep<-paste0("sleep_",c(1,2))
seden<-paste0("seden_",c(1,2))
slight<-paste0("slightact_",c(1,2))
mod<-paste0("modact_",c(1,2))
heavy<-paste0("heavyact_",c(1,2))
pai<-paste0("pai_",c(1,2))
gen3_pa<-gen3_pa %>% relocate(c(id,sleep,seden,slight,mod,heavy,pai), .before =  everything())
nos_pa<-nos_pa %>% relocate(c(id,sleep,seden,slight,mod,heavy,pai), .before =  everything())
omni2_pa<-omni2_pa %>% relocate(c(id,sleep,seden,slight,mod,heavy,pai), .before =  everything())


########################Convert range 8-NA; in categorical variables
gen3_pa[,colnames(gen3_pa) %in% c("18modex_2","35modex_2","50modex_2")]<-replace(gen3_pa[,colnames(gen3_pa) %in% c("18modex_2","35modex_2","50modex_2")],gen3_pa[,colnames(gen3_pa) %in% c("18modex_2","35modex_2","50modex_2")]==8, NA)
nos_pa[,colnames(nos_pa) %in% c("18modex_2","35modex_2","50modex_2")]<-replace(nos_pa[,colnames(nos_pa) %in% c("18modex_2","35modex_2","50modex_2")],nos_pa[,colnames(nos_pa) %in% c("18modex_2","35modex_2","50modex_2")]==8, NA)
omni2_pa[,colnames(omni2_pa) %in% c("18modex_2","35modex_2","50modex_2")]<-replace(omni2_pa[,colnames(omni2_pa) %in% c("18modex_2","35modex_2","50modex_2")],omni2_pa[,colnames(omni2_pa) %in% c("18modex_2","35modex_2","50modex_2")]==8, NA)


########################Create idtype variable in the dataset
gen3_pa$framid<-30000+gen3_pa$id
gen3_pa$idtype<-"3"
gen3_pa<- gen3_pa %>%select(framid,idtype, everything())

nos_pa$framid<-20000+nos_pa$id
nos_pa$idtype<-"2"
nos_pa<- nos_pa %>%select(framid,idtype, everything())

omni2_pa$framid<-720000+omni2_pa$id
omni2_pa$idtype<-"72"
omni2_pa<- omni2_pa %>%select(framid,idtype, everything())


#Reorder variables so that the order fits in a more logical order
colorder<-c("framid"
            ,"idtype"
            ,"id"
            ,"sleep_1"
            ,"sleep_2"
            ,"seden_1"
            ,"seden_2"
            ,"slightact_1"
            ,"slightact_2"
            ,"modact_1"
            ,"modact_2"
            ,"heavyact_1"
            ,"heavyact_2"
            ,"pai_1"
            ,"pai_2"
            ,"modex18_2"
            ,"modex35_2"
            ,"modex50_2"
            ,"aerobic_1"
            ,"bike_1"
            ,"jog_1"
            ,"lowintense_1"
            ,"run_1"
            ,"swim_1"
            ,"stairs_1"
            ,"tennis_1"
            ,"vigex_1"
            ,"walk_1"
            ,"walkpace_1"
            ,"weights_1"
            ,"bike_2"
            ,"bikemon_2"
            ,"bikefreq_2"
            ,"biketime_2"
            ,"bowl_2"
            ,"bowlmon_2"
            ,"bowlfreq_2"
            ,"bowltime_2"
            ,"calisth_2"
            ,"calisthmon_2"
            ,"calisthfreq_2"
            ,"calisthtime_2"
            ,"homeact_2"
            ,"homeactmon_2"
            ,"homeactfreq_2"
            ,"homeacttime_2"
            ,"homemaintain_2"
            ,"homemaintainmon_2"
            ,"homemaintainfreq_2"
            ,"homemaintaintime_2"
            ,"jog_2"
            ,"jogmon_2"
            ,"jogfreq_2"
            ,"jogtime_2"
            ,"nonstren_2"
            ,"nonstrenmon_2"
            ,"nonstrenfreq_2"
            ,"nonstrentime_2"
            ,"strensport_2"
            ,"strensportmon_2"
            ,"strensportfreq_2"
            ,"strensporttime_2"
            ,"swim_2"
            ,"swimmon_2"
            ,"swimfreq_2"
            ,"swimtime_2"
            ,"vigclass_2"
            ,"vigclassmon_2"
            ,"vigclassfreq_2"
            ,"vigclasstime_2"
            ,"vigjob_2"
            ,"vigjobmon_2"
            ,"vigjobfreq_2"
            ,"vigjobtime_2"
            ,"vigsport_2"
            ,"vigsportmon_2"
            ,"vigsportfreq_2"
            ,"vigsporttime_2"
            ,"walkwork_2"
            ,"walkworkmon_2"
            ,"walkworkfreq_2"
            ,"walkworktime_2"
            ,"weights_2"
            ,"weightsmon_2"
            ,"weightsfreq_2"
            ,"weightstime_2"
            ,"compleisure_2"
            ,"sitleisure_2"
            ,"sitatwork_2"
            ,"standatwork_2"
            ,"walkatwork_2")

gen3_pa_order<-setcolorder(gen3_pa, neworder=colorder)
nos_pa_order<-setcolorder(nos_pa, neworder=colorder)
omni2_pa_order<-setcolorder(omni2_pa, neworder=colorder)


########################Add "core" in front of each exam number
colnames(gen3_pa_order)[-c(1:3)]<-gsub("_", "_core", colnames(gen3_pa_order)[-c(1:3)])
colnames(nos_pa_order)[-c(1:3)]<-gsub("_", "_core", colnames(nos_pa_order)[-c(1:3)])
colnames(omni2_pa_order)[-c(1:3)]<-gsub("_", "_core", colnames(omni2_pa_order)[-c(1:3)])