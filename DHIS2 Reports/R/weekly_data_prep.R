library(data.table)
library(mice)
setwd("D:/Ramp")
code_dir <- "./DHIS2 Reports/R"
source(file.path(code_dir, "pull_data.R"))
source(file.path(code_dir, "get_output_dir.R"))

#import location_details
location_detail <- fread("D:/DHIS2_Data/location_details.csv", header = TRUE)

## Arguments
output_dir <- "./DHIS2 Reports"
summary_path <- file.path(output_dir, "weekly_summary_dt.csv")
out_dir <- get_output_dir(output_dir, "today")

# Date range
curr_week <- as.integer(strftime(Sys.Date(), format = "%V"))
year_start <- 2022
week_start <- 1
year_end <- 2023
week_end <- curr_week

pull_weekly_data <- function(weeks) {
  # Read in data
  message("Pulling district data...")
  district_dt <- pull_data(f = "weekly", p = weeks$period, l = 3)
  message("Pulling facility data...")
  facility_dt <- pull_data(f = "weekly", p = weeks$period, l = 5)
  
  # Combine and clean names
  dt <- rbind(district_dt, facility_dt, fill = T)
  dt[, week := as.integer(tstrsplit(period, "W")[[2]])]
  dt[, year := as.integer(tstrsplit(period, "W")[[1]])]
  dt <- dt[order(year, week)]
}

prep_weekly_summary <- function(path){
  if(file.exists(path)) {
    summary_dt <- fread(path)
  } else {
    curr_week <- as.integer(strftime(Sys.Date(), format = "%V"))
    year_start <- 2022
    week_start <- 1
    year_end <- 2022
    week_end <- curr_week
    weeks <- gen_week_dt(year_start, week_start, year_end, week_end)
    dt <- pull_weekly_data(weeks)
    
  }
  return(dt)
}
#repalce na with 0 function
replace_na_zero <- function(indicator) {
  indicator[is.na(indicator)] <- 0
  return(indicator)
}

save_weekly_report <- function(weeks, out_dir, summary_path) {
  weeks <- gen_week_dt(year_start, week_start, year_end, week_end)
  dt <- pull_weekly_data(weeks)

  # Rearrange for calculation
  dt <- dcast(dt, location_id + location_name + level + period + ownership + year + week ~ code_name, value.var = "value")
 
  # FACILITY LEVEL DATA
  
  # REPORTING RATES
  reporting_rates <- pull_reporting_rates(weeks = weeks$period)
  reporting_rates[, reporting_rate := as.numeric(value)]
  reporting_rates[, location_id := orgUnit]
  reporting_rates[, year := sub("W.*","",period)]
  reporting_rates[, week := sub(".*W","",period)]
 
  # DEATH
  deaths <- rbindlist(lapply(unique(dt$period), function(p) {
    dt[level == 5 & period == p & !is.na(deaths)][rev(order(deaths)), .(location_id, location_name, deaths,cases, year, week)]
  }))
  deaths <- merge(location_detail,deaths, by="location_id") #merge deaths and location_details
  deaths <- deaths[, percent_death := 100 * deaths / cases] #Calculate the proportion of recorded death from recorded cases
  deaths_Over50_excluded <- deaths[percent_death > 50.00] #Exclude records with over 50% death proportional to cases.
  deaths <- deaths[percent_death <= 50.00] #select records below 50%
  
  # Outliering with Z-SCORE
  deaths <- deaths[, mn:= mean(deaths,na.rm=TRUE), by = "subcounty"][] # Calculate the mean
  deaths <- deaths[, sd:= sd(deaths,na.rm=TRUE), by = "subcounty"][] # Calculate the standard deviation
  deaths <- deaths[, potential_outlier := ifelse(deaths > mn + 4 * sd, 1, 0)] # Add outlier indicator, with 1 as potential outlier and 0 as potential non-outlier
  deaths_outliers_excluded <- deaths[potential_outlier == 1] # Output potential outliers
  deaths <- deaths[,.(total_death = sum(deaths)), by = .(district,year,week)] # generate district summary
  
  # CASES
  cases <- rbindlist(lapply(unique(dt$period), function(p) {
    dt[level == 5 & period == p & !is.na(cases)][rev(order(deaths)), .(location_id,location_name, cases, year, week)]
    #!deaths >= cases
  }))
 
  cases <- merge(location_detail,cases, by="location_id") # merge cases and location_details
  cases <- cases[ , q1:= quantile(cases, na.rm=TRUE, 0.025), by = .(subcounty,location_name)][] # Calculate quantile 1
  cases <- cases[ , q3:= quantile(cases, na.rm=TRUE, 0.975), by = .(subcounty,location_name)][] # Calculate quantile 3
  cases <- cases[ , iqr:= q3-q1, by = .(subcounty,location_name)][]# Calculate iqr 
  
  cases <- cases[, potential_outlier := ifelse(cases > q3 + 1.5 * iqr , 1, 0)][] # Add outlier indicator, with 1 as potential outlier and 0 as potential non-outlier
  
  all_cases <- cases
  write.csv(all_cases, file.path(out_dir, "all_cases.csv"), row.names = F)

  cases_outlier_excluded <- cases[potential_outlier == 1] # Output potential outliers
  cases <- cases[potential_outlier == 0] # Select potential non-outliers 
  cases <- cases[,.(total_cases = sum(cases)), by = .(district,year,week)] # generate district summary
  
  # TEST POSITIVITY RATE - TPR
  tpr <- rbindlist(lapply(unique(dt$period), function(p) {
    dt[level == 5 & period == p][rev(order(rdt_tested )), .(location_id,location_name, rdt_tested, micro_tested,rdt_pos, micro_pos, year, week)]
    }))

  tpr <- merge(location_detail,tpr, by="location_id")
  
  tpr <-  tpr[!rdt_pos > rdt_tested]  # Should we include this records
  tpr <-  tpr[!micro_pos > micro_tested] # Should we include this records
  tpr <-  tpr[,total_tests:= replace_na_zero(rdt_tested) + replace_na_zero(micro_tested)] 
  tpr <-  tpr[,total_pos:= replace_na_zero(rdt_pos) + replace_na_zero(micro_pos)]
  tpr <-  tpr[,tpr:= round(100* total_pos / total_tests,1)]
  tpr <-  tpr[,.(tpr_district = mean(round(tpr,1))), by = .(district,year,week)]# generate district summary
  
  tpr_levels <- c("1. 0-10%", "2. 10-25%", "3. 25-40%","4. 40-55%","5. 55-70%","6. >70%")
  
  tpr <- tpr[, tpr_status := "0-10"]
  tpr <- tpr[tpr_district <=10, tpr_status := "1. 0-10%"]
  tpr <- tpr[tpr_district > 10 & tpr_district <= 25 , tpr_status := "2. 10-25%"]
  tpr <- tpr[tpr_district > 25 & tpr_district <= 40 , tpr_status := "3. 25-40%"]
  tpr <- tpr[tpr_district > 40 & tpr_district <= 55 , tpr_status := "4. 40-55%"]
  tpr <- tpr[tpr_district > 55 & tpr_district <= 70 , tpr_status := "5. 55-70%"]
  tpr <- tpr[tpr_district > 70 , tpr_status := "6. >70%"]
  tpr <- tpr[is.na(tpr_district) , tpr_status := NA]
  tpr <- tpr[, tpr_status := factor(tpr_status, levels = tpr_levels)]
  
  # STOCK 
  stock_levels <- c("Understock", "Adequate stock", "Overstock")
  
  #ACTs
  tx_stock <- rbindlist(lapply(unique(dt$period), function(p) {
    dt[level == 5 & period == p & !is.na(tx_stock)][rev(order(tx_stock)), .(location_id,location_name, tx_stock,cases,year, week)]
  }))
  tx_stock <- merge(location_detail, tx_stock, by="location_id")
  
  tx_stock <- tx_stock[ , q1:= quantile(tx_stock, na.rm=TRUE, 0.025), by = .(subcounty,location_name)][] # Calculate quantile 1
  tx_stock <- tx_stock[ , q3:= quantile(tx_stock, na.rm=TRUE, 0.975), by = .(subcounty,location_name)][] # Calculate quantile 3
  tx_stock <- tx_stock[ , iqr:= q3-q1, by = .(subcounty,location_name)][] # Calculate iqr
  
  tx_stock <- tx_stock[, potential_outlier := ifelse(tx_stock > q3 + 1.5 * iqr , 1, 0)][] # Add outlier indicator, with 1 as potential outlier and 0 as potential non-outlier
  
  tx_stock_outlier_excluded <- tx_stock[potential_outlier == 1] # Output potential outliers
  
  tx_stock <- tx_stock[potential_outlier == 0] # Select potential non-outliers
  tx_stock <- tx_stock[,.(total_tx_stock = sum(tx_stock, na.rm = TRUE)), by = .(district,year,week)]
  
  tx_stock <- merge(tx_stock, cases, by= c("district","year","week"), all.x=TRUE, all.y=TRUE)
  tx_stock <- tx_stock[,tx_available:= total_tx_stock/17]
  tx_stock <- tx_stock[,tx_stock_weeks:= tx_available / total_cases]
  
  tx_stock <- tx_stock[, tx_stockout := "Understock"]
  tx_stock <- tx_stock[tx_stock_weeks >= 8 & tx_stock_weeks <= 20, tx_stockout := "Adequate stock"]
  tx_stock <- tx_stock[tx_stock_weeks >= 20, tx_stockout := "Overstock"]
  tx_stock <- tx_stock[is.na(tx_stock_weeks), tx_stockout := NA]
  tx_stock <- tx_stock[, tx_stockout := factor(tx_stockout, levels = stock_levels)]
  
  # SUSPECTED
  suspected <- rbindlist(lapply(unique(dt$period), function(p) {
    dt[level == 5 & period == p & !is.na(suspected)][rev(order(suspected)), .(location_id,location_name, suspected, year, week)]
  }))
  suspected <- merge(location_detail,suspected, by="location_id") # merge suspected and location_details

  suspected <- suspected[ , q1:= quantile(suspected, na.rm=TRUE, 0.02), by = .(subcounty,location_name)][] # Calculate quantile 1
  suspected <- suspected[ , q3:= quantile(suspected, na.rm=TRUE, 0.985), by = .(subcounty,location_name)][] # Calculate quantile 3
  suspected <- suspected[ , iqr:= q3-q1, by = .(subcounty,location_name)][] # Calculate iqr
  
  suspected <- suspected[, potential_outlier := ifelse(suspected > q3 + 1.5 * iqr , 1, 0)][] # Add outlier indicator, with 1 as potential outlier and 0 as potential non-outlier
  
  suspected_outlier_excluded <- suspected[potential_outlier == 1] # Output potential outliers
  
  suspected <- suspected[potential_outlier == 0] # Select potential non-outliers 
  suspected <- suspected[,.(total_suspected = sum(suspected)), by = .(district,year,week)] # generate district summary
  
  # RDTs
  rdt_stock <- rbindlist(lapply(unique(dt$period), function(p) {
    dt[level == 5 & period == p ][rev(order(rdt_stock)), .(location_id,location_name,rdt_stock,year,week)]
  }))
  rdt_stock <- merge(location_detail, rdt_stock, by="location_id")
  
  rdt_stock <- rdt_stock[ , q1:= quantile(rdt_stock, na.rm=TRUE, 0.025), by = .(subcounty,location_name)][] # Calculate quantile 1
  rdt_stock <- rdt_stock[ , q3:= quantile(rdt_stock, na.rm=TRUE, 0.975), by = .(subcounty,location_name)][] # Calculate quantile 3
  rdt_stock <- rdt_stock[ , iqr:= q3-q1, by = .(subcounty,location_name)][] # Calculate iqr
  
  rdt_stock <- rdt_stock[, potential_outlier := ifelse(rdt_stock > q3 + 1.5 * iqr & rdt_stock < q3 - 1.5 * iqr , 1, 0)][] # Add outlier indicator, with 1 as potential outlier and 0 as potential non-outlier
  
  rdt_stock_outlier_excluded <- rdt_stock[potential_outlier == 1] # Output potential outliers
  
  rdt_stock <- rdt_stock[potential_outlier == 0] # Select potential non-outliers
  rdt_stock <- rdt_stock[,.(total_rdt_stock= sum(rdt_stock, na.rm = TRUE)), by = .(district,year,week)]

  rdt_stock <- merge(rdt_stock, suspected, by= c("district","year","week"), all.x=TRUE, all.y=TRUE)
  rdt_stock <- rdt_stock[,rdt_stock_weeks:= total_rdt_stock/total_suspected]
  
  rdt_stock <- rdt_stock[, rdt_stockout := "Understock"]
  rdt_stock <- rdt_stock[rdt_stock_weeks >= 8 & rdt_stock_weeks <= 20, rdt_stockout := "Adequate stock"]
  rdt_stock <- rdt_stock[rdt_stock_weeks >= 20, rdt_stockout := "Overstock"]
  rdt_stock <- rdt_stock[is.na(rdt_stock_weeks), rdt_stockout := NA]
  rdt_stock <- rdt_stock[, rdt_stockout := factor(rdt_stockout, levels = stock_levels)]
  
  
  #merge all datasets
  all_indicators <- merge(cases, deaths, by= c("district","year","week"), all.x=TRUE, all.y=TRUE)
  all_indicators <- merge(all_indicators,tpr, by= c("district","year","week"), all.x=TRUE, all.y=TRUE)
  all_indicators <- merge(all_indicators,tx_stock, by= c("district","year","week"), all.x=TRUE, all.y=TRUE)
  all_indicators <- merge(all_indicators,suspected, by= c("district","year","week"), all.x=TRUE, all.y=TRUE)
  all_indicators <- merge(all_indicators,rdt_stock, by= c("district","year","week"), all.x=TRUE, all.y=TRUE)
  
  write.csv( all_indicators, file.path(out_dir, "all_indicators.csv"), row.names = F)
  
  
  
  write.csv(reporting_rates, file.path(out_dir, "reporting_rates.csv"), row.names = F)
  
  write.csv(deaths, file.path(out_dir, "deaths.csv"), row.names = F)
  write.csv(deaths_outliers_excluded, file.path(out_dir, "deaths_outliers_excluded.csv"), row.names = F)
  write.csv(deaths_Over50_excluded, file.path(out_dir, "deaths_Over50_excluded.csv"), row.names = F)
  
  write.csv(cases, file.path(out_dir, "cases.csv"), row.names = F)
  write.csv(cases_outlier_excluded, file.path(out_dir, "cases_outlier_excluded.csv"), row.names = F)

  write.csv(suspected, file.path(out_dir, "suspected.csv"), row.names = F)
  write.csv(suspected_outlier_excluded, file.path(out_dir, "suspected_outlier_excluded.csv"), row.names = F)
  
  write.csv(tx_stock, file.path(out_dir, "tx_stock.csv"), row.names = F)
  write.csv(tx_stock_outlier_excluded, file.path(out_dir, "tx_stock_outlier_excluded.csv"), row.names = F)
  
  write.csv(rdt_stock, file.path(out_dir, "rdt_stock.csv"), row.names = F)
  write.csv(rdt_stock_outlier_excluded, file.path(out_dir, "rdt_stock_outlier_excluded.csv"), row.names = F)
  
  write.csv(suspected_outlier_excluded, file.path(out_dir, "suspected_outlier_excluded.csv"), row.names = F)
  
  write.csv(tpr, file.path(out_dir, "tpr.csv"), row.names = F)

  message("Done! Outputs here: ", out_dir)
  
}


save_weekly_report(weeks, out_dir)# Summary_path