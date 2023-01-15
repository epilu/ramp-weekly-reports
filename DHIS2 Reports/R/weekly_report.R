library(readxl)
library(data.table)
library(ggplot2)
library(rgdal)
library(tmap)
library(sf)
library(dplyr)
library(viridis)
library(fpp2)
library(lubridate)
library(xts)
library(haven)

data_dir <- "./DHIS2 Reports/2023_01_13.01"
clean_dir <- "./DHIS2 Reports/clean"
period_year <- 2022
period_week <- 52
previous_week <- 50

tmmap_mode <- "plot"


replace_na_zero <- function(indicator) {
  indicator[is.na(indicator)] <- 0
  return(indicator)
}

#import location_details
location_detail <- fread("D:/DHIS2_Data/location_details.csv", header = TRUE)
UgLakes <- readOGR("D:/DHIS2_Data/shapefiles/Uganda_Districts_2019/UG_Lakes/lakes.shp", verbose = FALSE)
UgDistrict_shp <- readOGR("D:/DHIS2_Data/shapefiles/Uganda_Districts_2019/uganda_districts_2019-wgs84.shp", verbose = FALSE)

UgDistrict_shp$DName2022 <- UgDistrict_shp$DName2019
UgDistrict_shp[UgDistrict_shp$DName2022 == "SSEMBABULE"] <- "SEMBABULE"
UgDistrict_shp[UgDistrict_shp$DName2022 == "MADI OKOLLO"] <- "MADI-OKOLLO"
districts <- fread("D:/DHIS2_Data/location_details.csv", header = TRUE)
districts <- districts[, .(`Facility_total` = .N), by = .(region,district)]

# REPORTING RATE

# REPORTING RATE
reporting_rates_path <- file.path(data_dir, "/reporting_rates.csv")
reporting_rates <- fread(reporting_rates_path, header = TRUE)
location_hierarchy <- fread("./DHIS2 Reports/tables/location_hierarchy.csv")
location_hierarchy <- location_hierarchy[level==3,.(district = location_name),.(location_id)]
reporting_rates <- merge(location_hierarchy,reporting_rates, by="location_id") # merge deaths and location_details
reporting_rates <- reporting_rates[week == period_week]
reporting_rates <- reporting_rates[year == period_year]
write.csv(reporting_rates, file.path(clean_dir, "reporting_rates.csv"), row.names = F)
avg_reporting_rates <- reporting_rates[,.(avg_reporting_rate = round(mean(reporting_rate),2)), .(year,week)]
write.csv(avg_reporting_rates, file.path(clean_dir, "avg_reporting_rates.csv"), row.names = F)

reporting_rates <- reporting_rates[,.(`DName2022`=toupper(gsub(' District','',district))),.(year,week,reporting_rate)]
reporting_rates <- merge(UgDistrict_shp, reporting_rates, by = "DName2022")
reporting_rates <- tm_shape(reporting_rates) +
  tmap_mode(tmmap_mode)+
  tm_polygons(col = "reporting_rate",
              style = "cont",
              title = "Reporting",
              breaks = c(0,50,60,70,80,90,100),
              palette=c('#ff0000','yellow','darkgreen')) +
  tm_view() +
  tm_layout(frame = FALSE, main.title.size = 1,legend.outside = TRUE, legend.position = c("left","bottom"))+
  tm_shape(UgLakes) +
  tmap_mode() +
  tm_fill(col="#0285e3")
#reporting_rates

# CASES
all_cases <- file.path(data_dir, "/all_cases.csv")
all_cases <- fread(all_cases, header = TRUE)
all_cases <- all_cases[week == 44 & location_name == "St. Nektarios Orthodox HC III"] # week == 52 & location_name == "Obim HC II"
all_cases <- ggplot(all_cases, aes(x = location_name, y=cases, color = as.factor(potential_outlier))) + 
  geom_point()
all_cases 


cases_path <- file.path(data_dir, "/cases.csv")
cases <- fread(cases_path, header = TRUE)

cases_week <- cases[week == period_week & year==period_year,.(district,year,week,total_cases)]
write.csv(cases_week, file.path(clean_dir, "cases_week.csv"), row.names = F)


upsurges <- cases
cases <- cases[,.(`total_cases`=sum(total_cases)),.(year,week)]
summary(cases$total_cases)
write.csv(cases, file.path(clean_dir, "cases_clean.csv"), row.names = F)

# start <- c(1)
# end <- c( week(today()))
# # # creation of a ts object
# cases <- ts(cases$total_cases,
#             frequency = 1,
#             start = start,
#             end = end)
# #
# # cases <- stl(cases , s.window="periodic")
# plot(cases)
# 
# #############################################
# # Use benchmark method for forecasting
# # using the seasonal naive method as our benchmark
# #############################################
# fit_naive <- snaive(cases)  #Residual sd: 53410.0468 
# print(summary(fit_naive))
# checkresiduals(fit_naive)
# 
# #############################################
# # Fit on ETS model
# #############################################
# fit_ets <- ets(cases)  #Residual sigma:  45978.84
# print(summary(fit_ets))
# checkresiduals(fit_ets)
# 
# #############################################
# # Fit on arima model
# #############################################
# fit_arima <- auto.arima(cases,d=1,D=1,stepwise = FALSE, approximation = FALSE,trace = TRUE)  #Residual s
# print(summary(fit_arima))
# checkresiduals(fit_arima)
# 
# 
# #############################################
# # Forecast arima model
# #############################################
# fcst <- forecast(fit_ets,h=52)
# autoplot(fcst)


# DEATHS
death_path <- file.path(data_dir, "/deaths.csv")
deaths <- fread(death_path, header = TRUE)
deaths_week <- deaths[week == period_week & year==period_year,.(district,year,week,total_death)]
write.csv(deaths_week, file.path(clean_dir, "deaths_week.csv"), row.names = F)
total_deaths <- deaths[,.(total_deaths = sum(total_death)),.(year,week)]
write.csv(total_deaths, file.path(clean_dir, "total_deaths.csv"), row.names = F)

deaths <- merge(districts, deaths_week, by = "district", all.x = TRUE,all.y = TRUE)
deaths <- deaths[,.(`DName2022`=toupper(gsub(' District','',district)),`total_death` =replace_na_zero(total_death)), by=.(region)]
deaths <- merge(UgDistrict_shp, deaths, by = "DName2022")
deaths <- tm_shape(deaths) +
  tmap_mode(tmmap_mode)+
  tm_polygons(col = "total_death",
              style = "cont", 
              title = "Deaths",
              breaks = c(0,1,2,3,4,5,6),
              palette=c('#ffffff','#ff0000')) +
  tm_view() +
  tm_layout(frame = FALSE, main.title.size = 1,legend.outside = TRUE,legend.position = c("left","bottom"))+
  tm_shape(UgLakes) +
  tmap_mode() +
  tm_fill(col="#0285e3")
#deaths

# TPR
tpr_path <- file.path(data_dir, "/tpr.csv")
tpr <- fread(tpr_path, header = TRUE)
tpr <- tpr[week == period_week & year==period_year,.(district,year,week,tpr_district,tpr_status)]
write.csv(tpr, file.path(clean_dir, "tpr.csv"), row.names = F)
tpr_nat_avg <- tpr[,.(tpr_avg = round(mean(tpr_district),1)), by=.(year,week)]
write.csv(tpr_nat_avg, file.path(clean_dir, "tpr_nat_avg.csv"), row.names = F)
tpr <- merge(districts, tpr, by = "district", all.x = TRUE,all.y = TRUE)
tpr <- tpr[,.(`DName2022`=toupper(gsub(' District','',district)),`tpr_district` =tpr_district), by=.(region)]
tpr <- merge(UgDistrict_shp, tpr, by = "DName2022")
tpr <- tm_shape(tpr) +
  tmap_mode(tmmap_mode)+
  tm_polygons(col = "tpr_district",
              style = "cont", 
              title = "TPR",
              breaks = c(0,20,40,60,80,100),
              palette=c('darkgreen','yellow','#ff0000'),
              textNA= "NA",
              colorNA = "#27292e"
              ) +
  tm_view()+
  tm_layout(frame = FALSE, main.title.size = 1,legend.outside = TRUE,legend.position = c("left","bottom"))+
  tm_shape(UgLakes) +
  tmap_mode() +
  tm_fill(col="#0285e3")


# UPSURGES
# import historical facility data to calculate 75th percentile
weekly_2020 <- read_dta("./DHIS2 Reports/raw/weekly_2020.dta")# import weekly data  - old dhis2
weekly_2022 <- fread("./DHIS2 Reports/raw/weekly_2021_22.csv") # import weekly data - new dhis2
upsurges_colnames <- c("district","periodid","total_cases","year") # define new column names
weekly_2020 <- weekly_2020[,c("District","periodid","Cases","year")] # Select indicators for old dhis2 weekly data
colnames(weekly_2020) <- upsurges_colnames # assign column names

weekly_2022 <- weekly_2022[,c("organisationunitname","periodid","033B-CD01a. Malaria (diagnosed)  - Cases")] # Select indicators for old dhis2 weekly data
weekly_2022 <- weekly_2022[,.(year=substr(periodid,1,4)),c("organisationunitname","periodid","033B-CD01a. Malaria (diagnosed)  - Cases")] #create a year variable from periodid
colnames(weekly_2022) <- upsurges_colnames # assign new column names

hist_weekly <- list(weekly_2020,weekly_2022) # list datasets
hist_weekly <- rbindlist(hist_weekly, use.names=TRUE) # rbind datasets

hist_weekly <- hist_weekly[year >=2018 & year <=2022] # select data for the last 5 years (2018-2022)
hist_weekly <- hist_weekly[,.(percent75th = round(quantile(total_cases,0.75),1)), by=.(district)] # calculate 75 percentile

upsurges <- upsurges[year == period_year & week >= previous_week & week <= period_week] # select  data for last 3 weeks
upsurges <- upsurges[,.(`total_cases`=sum(total_cases)),.(district,year,week)] # Sum health facility cases for by district

upsurges <- merge(upsurges, hist_weekly, by = "district",all = TRUE) # merge hist_weekly and upsurge  data
upsurges <- upsurges[total_cases > percent75th] # select cases greater than 75th percentile
upsurges <- upsurges[,.(diff_CasesPercent75th = total_cases - percent75th), .(district,year,week,total_cases,percent75th)] # calculate the difference between total cases and 75th percentile cases
upsurges <- upsurges[,.(percentDiff = round(diff_CasesPercent75th/total_cases* 100,1)), .(district,year,week,total_cases,percent75th,diff_CasesPercent75th)] # calculate the percentage difference
upsurges <- upsurges[,.(upsurgeIndicator = ifelse(percentDiff > 0 , 1, 0)), .(district,year,week,total_cases,percent75th,diff_CasesPercent75th,percentDiff)] # calculate the upsurge indicator

upsurges <- upsurges[upsurgeIndicator == 1] # select records with indicator = 1

upsurges <- upsurges[,.(IndicatorCounter = .N), .(district,upsurgeIndicator)] # calculate the upsurge indicator

upsurges <- upsurges[IndicatorCounter == 3] # calculate the upsurge indicator

write.csv(upsurges, file.path(clean_dir, "upsurge_districts.csv"), row.names = F)

upsurges <- merge(districts, upsurges, by = "district", all.x = TRUE,all.y = TRUE)
upsurges <- upsurges[,.(`DName2022`=toupper(gsub(' District','',district)),`upsurgeIndicator` =replace_na_zero(upsurgeIndicator)), by=.(region)]
upsurges <- merge(UgDistrict_shp, upsurges, by = "DName2022")
upsurges <- tm_shape(upsurges) +
  tmap_mode(tmmap_mode)+
  tm_polygons(col = "upsurgeIndicator",
              style = "cont", 
              title = "Epidemic districts",
              breaks = c(0,1),
              palette=c('#FBFDBD','#ff0000')) +
  tm_view() +
  tm_layout(frame = FALSE, main.title.size = 1,legend.show = FALSE)+
  tm_shape(UgLakes) +
  tmap_mode() +
  tm_fill(col="#0285e3")
#upsurges

# TX_STOCK
tx_stock_path <- file.path(data_dir, "/tx_stock.csv")
tx_stock <- fread(tx_stock_path, header = TRUE)
tx_stock <- tx_stock[week == period_week & year == period_year,.(district,year,week,tx_stock_weeks,tx_stockout)]
tx_stock <- merge(districts, tx_stock, by = "district", all.x = TRUE,all.y = TRUE)

write.csv(tx_stock, file.path(clean_dir, "tx_stock.csv"), row.names = F)

tx_stock <- tx_stock[,.(`DName2022`=toupper(gsub(' District','',district)),`tx_stock_weeks` =replace_na_zero(tx_stock_weeks)), by=.(region,tx_stockout,week,year)]
tx_stock <- merge(UgDistrict_shp, tx_stock, by = "DName2022")
tx_stock <- tm_shape(tx_stock) +
  tmap_mode(tmmap_mode)+
  tm_polygons(col = "tx_stockout",
              style = "cont",
              title = "ACT Stock",
              palette=c('yellow','darkgreen','#ff0000')) +
  tm_view() +
  tm_layout(frame = FALSE, main.title.size = 1,legend.outside = TRUE, legend.position = c("left","bottom"))+
  tm_shape(UgLakes) +
  tmap_mode() +
  tm_fill(col="#0285e3")
#tx_stock

# RDT_STOCK
rdt_stock_path <- file.path(data_dir, "/rdt_stock.csv")
rdt_stock <- fread(rdt_stock_path, header = TRUE)
rdt_stock <- rdt_stock[week == period_week,.(district,year,week,rdt_stock_weeks,rdt_stockout)]

write.csv(rdt_stock, file.path(clean_dir, "rdt_stock.csv"), row.names = F)

rdt_stock <- merge(districts, rdt_stock, by = "district", all.x = TRUE,all.y = TRUE)
rdt_stock <- rdt_stock[,.(`DName2022`=toupper(gsub(' District','',district)),`rdt_stock_weeks` =replace_na_zero(rdt_stock_weeks)), by=.(region,rdt_stockout,week,year)]
rdt_stock <- merge(UgDistrict_shp, rdt_stock, by = "DName2022")
rdt_stock <- tm_shape(rdt_stock) +
  tmap_mode(tmmap_mode)+
  tm_polygons(col = "rdt_stockout",
              style = "cont",
              title = "RDT Stock",
              palette=c('yellow','darkgreen','#ff0000')) +
  tm_view() +
  tm_layout(frame = FALSE, main.title.size = 1,legend.outside = TRUE, legend.position = c("left","bottom"))+
  tm_shape(UgLakes) +
  tmap_mode() +
  tm_fill(col="#0285e3")
#rdt_stock

# tmap_arrange(reporting_rates,tpr,deaths,rdt_stock,tx_stock)

reporting_rates
tpr
deaths
rdt_stock
tx_stock
upsurges
