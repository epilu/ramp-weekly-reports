#import libraries
library(data.table)

#Set working directory
setwd("D:/Ramp/")

#Arguements
file_path <- "./DHIS2 Reports/raw/wkly_data 2022.csv"
#read data
wkdt<- fread(file_path, header = TRUE)
#convert to data.table
wkdt <- as.data.table(wkdt)
colnames(wkdt)
#select indicators
wkdt<-wkdt[,.(periodid)]

wkdt
