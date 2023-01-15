#import libraries
library(dplyr)
library(tidyverse)
library(viridis)

#Set working director
setwd("D:/Ramp/")
#Read weekly malaria data
malaria_wkly <- read.csv("./DHIS2 Reports/raw/mTrac_wk_38_44.csv")
#Select key indicators for this analysis
malaria_wkly <- malaria_wkly[ -c(2:5,8,10:13) ]

head(malaria_wkly)
#check structure of the datatset
str(malaria_wkly)
#find out the total NAs for each column in dataset
colSums(is.na(malaria_wkly))
#column names
colnames(malaria_wkly)

#===========================================================================================================
#DISTINCT PERIODIDS, REGIONS AND DISTRICTS

#Select data 
malaria_wkly <- malaria_wkly[ which( malaria_wkly$periodid == "2022W44" ) , ]# | malaria_wkly$periodid == "2022W37"

#build clean district level data for analysis
district_clean <- malaria_wkly[,1:3]#select target indicator
district_clean <- distinct(district_clean)#select distinct

#===========================================================================================================
#FACILITIES EXPECTED TO REPORT

#group by  orgunitlevel2 [district] and count the number of health facilities that are exported to report
exp_rpt_hf <- malaria_wkly[,c(3,4)] #select target indicator\
exp_rpt_hf <- exp_rpt_hf[which(!is.na(exp_rpt_hf$orgunitlevel5)), names(exp_rpt_hf) %in%  c("orgunitlevel3","orgunitlevel5")]
exp_rpt_hf <- exp_rpt_hf %>%
  group_by(orgunitlevel3) %>%
  summarise(
    orgunitlevel5 = n()
  )

#column bind the expected  reportingfacilities to district clean data
district_clean<-merge(district_clean , exp_rpt_hf, by = "orgunitlevel3") 

#===========================================================================================================
#REPORTING RATE PER DISTRICT

rpting_rate <- malaria_wkly[,c(3,15)] #select target indicator

rpting_rate <- rpting_rate %>%
  group_by(orgunitlevel3) %>%
  summarise(
    HMIS.033b...Weekly.Epidemiological.Surveillance.Report...Reporting.rate = round(mean(HMIS.033b...Weekly.Epidemiological.Surveillance.Report...Reporting.rate,na.rm = T),1)
  )
#column bind the reporting rate to district clean data
district_clean<-merge(district_clean , rpting_rate , by = "orgunitlevel3") 

#===========================================================================================================
#TOTAL OPD

#select target indicator
total_opd <- malaria_wkly[,c(3:4,6)] 
#check how skewed the data is
hist(total_opd$X033B.AP02..Total.OPD)
boxplot(total_opd$X033B.AP02..Total.OPD)#check for outliers 
#remove all records with NA
total_opd <- total_opd[which(!is.na(total_opd$X033B.AP02..Total.OPD)), names(total_opd) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.AP02..Total.OPD")]

#use boxplot.stats to identify and investigate potential outliers
out_opd <- boxplot.stats(total_opd$X033B.AP02..Total.OPD)$out 
#find all outliers in dataframe
out_opd <- which(total_opd$X033B.AP02..Total.OPD %in% c(out_opd))
#print all outliers and associated rows and columns
out_opd <- total_opd[out_opd,]

# View(out_opd)
#Exclude record 
# total_opd <- subset(total_opd, !total_opd$X033B.AP02..Total.OPD == 3770  & !total_opd$orgunitlevel5 =="Nakapelimoru HC III")

#check for outliers after data cleaning
boxplot(total_opd$X033B.AP02..Total.OPD)

total_opd<- total_opd %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.AP02..Total.OPD = sum(X033B.AP02..Total.OPD, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean , total_opd , by = "orgunitlevel3") 

#===========================================================================================================
#TOTAL DEATH
#select target indicator
total_death <- malaria_wkly[,c(3:4,5)] 
#remove all records with NA
total_death  <- total_death [which(!is.na(total_death$X033B.CD01b..Malaria..diagnosed....Deaths)), names(total_death) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.CD01b..Malaria..diagnosed....Deaths")]
#check how skewed the data is
hist(total_death$X033B.CD01b..Malaria..diagnosed....Deaths)
boxplot(total_death$X033B.CD01b..Malaria..diagnosed....Deaths)#check for outliers 

#use boxplot.stats to identify and investigate potential outliers
out_death <- boxplot.stats(total_death$X033B.CD01b..Malaria..diagnosed....Deaths)$out 
#find all outliers in dataframe
out_death <- which(total_death$X033B.CD01b..Malaria..diagnosed....Deaths %in% c(out_death))
#print all outliers and associated rows and columns
out_death <- total_death[out_death,]

# View(out_death)

#Exclude record for Nantabulirwa HC II and Maaji C HC II and . Possible error data. health center 2 dont have inpatient departments
total_death <- subset(total_death, !total_death$X033B.CD01b..Malaria..diagnosed....Deaths== 38  & !total_death$orgunitlevel5 =="Bumageni HC II")
total_death <- subset(total_death, !total_death$X033B.CD01b..Malaria..diagnosed....Deaths== 156  & !total_death$orgunitlevel5 =="Cwero HC III")
total_death <- subset(total_death, !total_death$X033B.CD01b..Malaria..diagnosed....Deaths== 5  & !total_death$orgunitlevel5 =="Debrose Medical Clinic HC II")
total_death <- subset(total_death, !total_death$X033B.CD01b..Malaria..diagnosed....Deaths== 30  & !total_death$orgunitlevel5 =="Mazinga HC II")
total_death <- subset(total_death, !total_death$X033B.CD01b..Malaria..diagnosed....Deaths== 8  & !total_death$orgunitlevel5 =="Victory Women's Medical Care")

##check for outliers after data cleaning
boxplot(total_death$X033B.CD01b..Malaria..diagnosed....Deaths)

total_death <- total_death  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.CD01b..Malaria..diagnosed....Deaths = sum(X033B.CD01b..Malaria..diagnosed....Deaths, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean , total_death , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE) 

#===========================================================================================================
#MALARIA DIAGNOSED CASES

#select target indicator
diag_cases <- malaria_wkly[,c(3:4,7)] 
#remove all records with NA
diag_cases <- diag_cases [which(!is.na(diag_cases$X033B.CD01a..Malaria..diagnosed.....Cases )), names(diag_cases) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.CD01a..Malaria..diagnosed.....Cases")]
#check how skewed the data is
hist(diag_cases$X033B.CD01a..Malaria..diagnosed.....Cases )
boxplot(diag_cases$X033B.CD01a..Malaria..diagnosed.....Cases )#check for outliers 

#use boxplot.stats to identify and investigate potential outliers
out_diag <- boxplot.stats(diag_cases$X033B.CD01a..Malaria..diagnosed.....Cases)$out 
#find all outliers in dataframe
out_diag <- which(diag_cases$X033B.CD01a..Malaria..diagnosed.....Cases %in% c(out_diag))
#print all outliers and associated rows and columns
out_diag <- diag_cases[out_diag,]

# View(out_diag)
#summarise data by district
diag_cases <- diag_cases  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.CD01a..Malaria..diagnosed.....Cases = sum(X033B.CD01a..Malaria..diagnosed.....Cases, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean , diag_cases , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE) 

#===========================================================================================================
#SUSPECTED MALARIA (FEVER)

#select target indicator
susp_mal <- malaria_wkly[,c(3:4,8)] 

#remove all records with NA
susp_mal<- susp_mal[which(!is.na(susp_mal$X033B.MA01..Suspected.Malaria..Fever.)), names(susp_mal) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.MA01..Suspected.Malaria..Fever.")]

#check how skewed the data is
hist(susp_mal$X033B.MA01..Suspected.Malaria..Fever. )
boxplot(susp_mal$X033B.MA01..Suspected.Malaria..Fever. )#check for outliers 

#use boxplot.stats to identify and investigate potential outliers
out_sus <- boxplot.stats(susp_mal$X033B.MA01..Suspected.Malaria..Fever.)$out 
#find all outliers in dataframe
out_sus <- which(susp_mal$X033B.MA01..Suspected.Malaria..Fever. %in% c(out_sus))
#print all outliers - associated rows and columns
out_sus <- susp_mal[out_sus,]

#View(out_sus)
#exclude error records
susp_mal <- subset(susp_mal, !susp_mal$X033B.MA01..Suspected.Malaria..Fever.== 4899  & !susp_mal$orgunitlevel5 =="Ofua (Uriama) HC III")#54 dead with opd attendance of 54
susp_mal <- subset(susp_mal, !susp_mal$X033B.MA01..Suspected.Malaria..Fever.== 4313  & !susp_mal$orgunitlevel5 =="Puranga HC III")#54 dead with opd attendance of 54

susp_mal <- susp_mal  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.MA01..Suspected.Malaria..Fever. = sum(X033B.MA01..Suspected.Malaria..Fever., na.rm = T)
  )

#column bind the total_opd to district clean data
district_clean<-merge(district_clean , susp_mal , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE) 

#=============================================================================================================
#RDT (MALARIA) TEST

#select target indicator
rdt_mal <- malaria_wkly[,c(3:4,9)] 
#remove all records with NA
rdt_mal <- rdt_mal  [which(!is.na(rdt_mal$X033B.TR08..RDT..Malaria..Tests)), names(rdt_mal) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.TR08..RDT..Malaria..Tests")]
#check how skewed the data is
hist(rdt_mal$X033B.TR08..RDT..Malaria..Tests )
boxplot(rdt_mal$X033B.TR08..RDT..Malaria..Tests  )#check for outliers 

#use boxplot.stats to identify and investigate potential outliers
out_rdt_mal <- boxplot.stats(rdt_mal$X033B.TR08..RDT..Malaria..Tests)$out 
#find all outliers in dataframe
out_rdt_mal <- which(rdt_mal$X033B.TR08..RDT..Malaria..Tests %in% c(out_rdt_mal))
#print all outliers - associated rows and columns
out_rdt_mal <- rdt_mal[out_rdt_mal,]

# View(out_rdt_mal)
#exclude error records
rdt_mal <- subset(rdt_mal, !rdt_mal$X033B.TR08..RDT..Malaria..Tests== 102500  & !rdt_mal$orgunitlevel5 =="Jomorogo Health Centre HC III")
rdt_mal <- subset(rdt_mal, !rdt_mal$X033B.TR08..RDT..Malaria..Tests== 76500  & !rdt_mal$orgunitlevel5 =="Olelpek HC III")
# rdt_mal <- subset(rdt_mal, !rdt_mal$X033B.TR08..RDT..Malaria..Tests== 54000  & !rdt_mal$orgunitlevel5 =="Pader Police HC II")
# rdt_mal <- subset(rdt_mal, !rdt_mal$X033B.TR08..RDT..Malaria..Tests== 53500  & !rdt_mal$orgunitlevel5 =="Tajar HC III")

#group by district and sum all suspected malaria cases
rdt_mal <- rdt_mal  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.TR08..RDT..Malaria..Tests = sum(X033B.TR08..RDT..Malaria..Tests, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean, rdt_mal , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE) 

#=============================================================================================================
#ACT (MALARIA) TEST

#select target indicator
act_tablets <- malaria_wkly[,c(3:4,10)] 

#remove all records with NA
act_tablets <- act_tablets  [which(!is.na(act_tablets$X033B.TR01..ACT..Tablets.)), names(act_tablets) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.TR01..ACT..Tablets.")]
#check how skewed the data is
hist(act_tablets$X033B.TR01..ACT..Tablets.)
boxplot(act_tablets$X033B.TR01..ACT..Tablets. )#check for outliers 

#use boxplot.stats to identify and investigate potential outliers
out_act <- boxplot.stats(act_tablets$X033B.TR01..ACT..Tablets.)$out 
#find all outliers in dataframe
out_act <- which(act_tablets$X033B.TR01..ACT..Tablets. %in% c(out_act))
#print all outliers - associated rows and columns
out_act <- act_tablets[out_act,]
# View(out_act)
#Exclude suspecious error data
act_tablets <- subset(act_tablets, !act_tablets$X033B.TR01..ACT..Tablets. == 289137600  & !act_tablets$orgunitlevel5 =="Kawaala Health Centre HC IV")
act_tablets <- subset(act_tablets, !act_tablets$X033B.TR01..ACT..Tablets. == 90002575  & !act_tablets$orgunitlevel5 =="Loputuk HC III")
act_tablets <- subset(act_tablets, !act_tablets$X033B.TR01..ACT..Tablets. == 14351500  & !act_tablets$orgunitlevel5 =="Abiriamajo HC II")

#group by district and sum all suspected malaria cases
act_tablets <- act_tablets  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.TR01..ACT..Tablets. = sum(X033B.TR01..ACT..Tablets., na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean, act_tablets , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE)

#=============================================================================================================
#CHECK WHERE RDT POSITIVES > RDT TESTS 
#select dataset
rdt_test_pos <- malaria_wkly[,c(3:4,11,12)] 
#head(rdt_tests)
#remove all records with NA
rdt_test_pos  <- rdt_test_pos   [which(!is.na(rdt_test_pos$X033B.MA02..Cases.Tested.with.RDT) & !is.na(rdt_test_pos$X033B.MA03..RDT.Positive.Cases)), names(rdt_test_pos ) %in%  
                                   c("orgunitlevel3","orgunitlevel5","X033B.MA02..Cases.Tested.with.RDT", "X033B.MA03..RDT.Positive.Cases")]
rdt_test_pos  <- rdt_test_pos   [which(rdt_test_pos$X033B.MA03..RDT.Positive.Cases > rdt_test_pos$X033B.MA02..Cases.Tested.with.RDT ), names(rdt_test_pos ) %in%  
                                   c("orgunitlevel3","orgunitlevel5","X033B.MA02..Cases.Tested.with.RDT", "X033B.MA03..RDT.Positive.Cases")]
# View(rdt_test_pos)

#=============================================================================================================
#CASES TESTED WITH RDTS

#select target indicator
rdt_tests <- malaria_wkly[,c(3:4,11)] 
#head(rdt_tests)
#remove all records with NA
rdt_tests <- rdt_tests  [which(!is.na(rdt_tests$X033B.MA02..Cases.Tested.with.RDT)), names(rdt_tests) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.MA02..Cases.Tested.with.RDT")]
#check how skewed the data is
hist(rdt_tests$X033B.MA02..Cases.Tested.with.RDT)
boxplot(rdt_tests$X033B.MA02..Cases.Tested.with.RDT)#check for outliers 
#use boxplot.stats to identify and investigate potential outliers
out_rdt_tests <- boxplot.stats(rdt_tests$X033B.MA02..Cases.Tested.with.RDT)$out 
#find all outliers in dataframe
out_rdt_tests <- which(rdt_tests$X033B.MA02..Cases.Tested.with.RDT %in% c(out_rdt_tests))
#print all outliers - associated rows and columns
out_rdt_tests <- rdt_tests[out_rdt_tests,]

# View(out_rdt_tests)

#Exclude record fwith errors
rdt_tests <- subset(rdt_tests, !rdt_tests$X033B.MA02..Cases.Tested.with.RDT == 2444  & !rdt_tests$orgunitlevel5 =="Buliisa HC IV")
#rdt_tests <- subset(rdt_tests, !rdt_tests$X033B.MA02..Cases.Tested.with.RDT == 14351500  & !rdt_tests$orgunitlevel5 =="Abiriamajo HC II")

#group by district and sum all suspected malaria cases
rdt_tests <- rdt_tests  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.MA02..Cases.Tested.with.RDT = sum(X033B.MA02..Cases.Tested.with.RDT, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean, rdt_tests , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE)

#=============================================================================================================
#RDT POSITIVE CASES

#select target indicator
rdt_pos <- malaria_wkly[,c(3:4,12)] 
#head(rdt_tests)
#remove all records with NA
rdt_pos <- rdt_pos  [which(!is.na(rdt_pos$X033B.MA03..RDT.Positive.Cases)), names(rdt_pos) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.MA03..RDT.Positive.Cases")]
#check how skewed the data is
hist(rdt_pos$X033B.MA03..RDT.Positive.Cases)
boxplot(rdt_pos$X033B.MA03..RDT.Positive.Cases)#check for outliers 
#use boxplot.stats to identify and investigate potential outliers
out_rdt_pos <- boxplot.stats(rdt_pos$X033B.MA03..RDT.Positive.Cases)$out 
#find all outliers in dataframe
out_rdt_pos <- which(rdt_pos$X033B.MA03..RDT.Positive.Cases %in% c(out_rdt_pos))
#print all outliers - associated rows and columns
out_rdt_pos <- rdt_pos[out_rdt_pos,]

#Exclude suspecious error record 

# View(out_rdt_pos)
#group by district and sum all suspected malaria cases
rdt_pos <- rdt_pos  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.MA03..RDT.Positive.Cases = sum(X033B.MA03..RDT.Positive.Cases, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean, rdt_pos , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE)

#=============================================================================================================
#CHECK WHERE MICROS POSITIVES > MICROS TESTS 
#select dataset
micros_test_pos <- malaria_wkly[,c(3:4,13,14)] 
#head(rdt_tests)
#remove all records with NA
micros_test_pos  <- micros_test_pos   [which(!is.na(micros_test_pos$X033B.MA04..Cases.Tested.with.Microscopy ) & !is.na(micros_test_pos$X033B.MA05..Microscopy.Positive.Cases )), names(micros_test_pos ) %in%  
                                   c("orgunitlevel3","orgunitlevel5","X033B.MA04..Cases.Tested.with.Microscopy", "X033B.MA05..Microscopy.Positive.Cases")]
micros_test_pos  <- micros_test_pos   [which(micros_test_pos$X033B.MA05..Microscopy.Positive.Cases > micros_test_pos$X033B.MA04..Cases.Tested.with.Microscopy ), names(micros_test_pos ) %in%  
                                   c("orgunitlevel3","orgunitlevel5","X033B.MA04..Cases.Tested.with.Microscopy", "X033B.MA05..Microscopy.Positive.Cases")]
# View(micros_test_pos)

#=============================================================================================================
#CASES TESTED WITH MICROSCOPY

#select target indicator
micros_test <- malaria_wkly[,c(3:4,13)] 
#head(micros_test)
#remove all records with NA
micros_test <- micros_test  [which(!is.na(micros_test$X033B.MA04..Cases.Tested.with.Microscopy)), names(micros_test) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.MA04..Cases.Tested.with.Microscopy")]
#check how skewed the data is
hist(micros_test$X033B.MA04..Cases.Tested.with.Microscopy)
boxplot(micros_test$X033B.MA04..Cases.Tested.with.Microscopy)#check for outliers 
#use boxplot.stats to identify and investigate potential outliers
out_micros_test <- boxplot.stats(micros_test$X033B.MA04..Cases.Tested.with.Microscopy)$out 
#find all outliers in dataframe
out_micros_test <- which(micros_test$X033B.MA04..Cases.Tested.with.Microscopy %in% c(out_micros_test))
#print all outliers - associated rows and columns
out_micros_test <- micros_test[out_micros_test,]

# View(out_micros_test)

#Exclude record fwith errors
# micros_test <- subset(micros_test, !micros_test$X033B.MA04..Cases.Tested.with.Microscopy == 28  & !micros_test$orgunitlevel5 =="Cwero HC III") 

#group by district and sum all suspected malaria cases
micros_test <- micros_test  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.MA04..Cases.Tested.with.Microscopy = sum(X033B.MA04..Cases.Tested.with.Microscopy, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean, micros_test , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE)

#=============================================================================================================
#MICROSCOPY POSITIVE CASES

#select target indicator
micros_pos <- malaria_wkly[,c(3:4,14)] 
#head(micros_pos)
#remove all records with NA
micros_pos <- micros_pos[which(!is.na(micros_pos$X033B.MA05..Microscopy.Positive.Cases)), names(micros_pos) %in%  c("orgunitlevel3","orgunitlevel5", "X033B.MA05..Microscopy.Positive.Cases")]
#check how skewed the data is
hist(micros_pos$X033B.MA05..Microscopy.Positive.Cases)
boxplot(micros_pos$X033B.MA05..Microscopy.Positive.Cases)#check for outliers 
#use boxplot.stats to identify and investigate potential outliers
out_micros_pos <- boxplot.stats(micros_pos$X033B.MA05..Microscopy.Positive.Cases)$out 
#find all outliers in dataframe
out_micros_pos <- which(micros_pos$X033B.MA05..Microscopy.Positive.Cases %in% c(out_micros_pos))
#print all outliers - associated rows and columns
out_micros_pos <- micros_pos[out_micros_pos,]

# View(out_micros_pos)

#Exclude record fwith errors
# micros_pos <- subset(micros_pos, !micros_pos$X033B.MA05..Microscopy.Positive.Cases == 11015  & !micros_pos$orgunitlevel5 =="Kamira HC III") 

boxplot(micros_pos$X033B.MA05..Microscopy.Positive.Cases)

#group by district and sum all suspected malaria cases
micros_pos <- micros_pos  %>%
  group_by(orgunitlevel3) %>%
  summarise(
    X033B.MA05..Microscopy.Positive.Cases = sum(X033B.MA05..Microscopy.Positive.Cases, na.rm = T)
  )
#column bind the total_opd to district clean data
district_clean<-merge(district_clean, micros_pos , by = "orgunitlevel3",all = TRUE, all.x = TRUE, all.y = TRUE)



#Export final data to csv
write.csv(district_clean,file = "./DHIS2 Reports/clean/mTrac_2022w43.csv")

