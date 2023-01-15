## Set up
library(httr); 
library(jsonlite); 
library(data.table);

cred <- jsonlite::read_json("D:/DHIS2_Data/dhis2_credentials.json")
getwd()
setwd("D:/Ramp")
options(dhis2_username = cred$new$username)
options(dhis2_password = cred$new$password)
options(dhis2_base = "https://hmis.health.go.ug/api/")

## Tables
loc_table <-  fread("./DHIS2 Reports/tables/location_hierarchy.csv", header = TRUE)
indicator_table <- fread("./DHIS2 Reports/tables/dhis_indicator_table.csv", header = TRUE)


#' Generate a URL with a set of data element, period and organization strings
#' @param dx_string Data elements string
#' @param pe_string Periods string
#' @param ou_string Organizational units string
#' @param base String with API website base
#' @param extra_string Extra part to API call

gen_url <- function(dx_string, pe_string, ou_string, base = getOption("dhis2_base"), extra_string = NULL) {
  
    url <- paste0(base, "analytics/dataValueSet.json?",
                  "dimension=dx:", dx_string,
                  "&dimension=pe:", pe_string,
                  "&dimension=ou:", ou_string)
    
    if (!is.null(extra_string)) {
      
        url <- paste0(url, "&dimension=", extra_string)
        
    }
    return(url)
}

#' Pull data using the DHIS2 API
#' @param url String with the URL to get
#' @param username String with DHIS2 username value
#' @param password String with DHIS2 password value

pull_url <- function(url, username = getOption("dhis2_username"), password = getOption("dhis2_password")) {
  
    get <- GET(url, authenticate(username, password))
    get_text <- content(get, "text")
    get_json <- fromJSON(get_text, flatten = T)
    data <- data.table(get_json$dataValues)
    
    if ("period" %in% names(data)) {
      
        data <- data[order(period)]
        data[, value := as.numeric(value)]
        data[, c("storedBy", "created", "lastUpdated", "comment") := NULL]
        
    }
    
    return(data)
}

#' Pull data from DHIS2 for a specific reporting frequency, range of periods,
#' and location level
#' @param f String for reporting frequency - either "weekly" or "monthly"
#' @param p Vector of strings for period values
#' @param l Integer location level

pull_data <- function(f, p, l) {
  # Grab data elements and locations from their tables
  indicators <- indicator_table[frequency == f]$dhis_id
 
  locs <- unique(loc_table[level == l]$location_id)
  
  # Construct strings
  dx_string <- paste(indicators, collapse = ";")
  pe_string <- paste(p, collapse = ";")
  
  # Loop through locations if there are many (>500)
  total <- length(locs)
  if (total > 500) {
    step <- 500
    idx_lowers <- seq(1, total, by = step)
    pull_dt <- rbindlist(lapply(idx_lowers, function(idx_lower) {
      idx_range <- idx_lower:min((idx_lower + step - 1), total)
      ou_string <- paste(locs[idx_range], collapse = ";")
      url <- gen_url(dx_string, pe_string, ou_string)
      data <- pull_url(url)
      return(data)
    }))
  } else {
    ou_string <- paste(locs, collapse = ";")
    url <- gen_url(dx_string, pe_string, ou_string)
    pull_dt <- pull_url(url)
  }
  # Generate a data frame with all strata values to account for missing elements
  full_dt <- data.table(expand.grid(
    dataElement = indicators,
    period = p,
    orgUnit = locs
  ))
  pull_dt <- merge(full_dt, pull_dt, by = c("dataElement", "period", "orgUnit"), all.x = T)
  
  # Clean up and merge on human readable names
  setnames(pull_dt, c("dataElement", "orgUnit"), c("dhis_id", "location_id"))
  dt <- merge(pull_dt, loc_table[, .(location_id, location_name)], by = "location_id")
  if (l == 5) {
    dt <- merge(dt, loc_table[, .(location_id, facility_level, operational_status, ownership)], by = "location_id")
  }
  dt <- merge(dt, indicator_table[, .(dhis_id, code_name)], by = "dhis_id")
  dt[, c("level", "frequency") := .(l, f)]
  return(dt[])
}

pull_reporting_rates <- function(weeks) {
  pe_string <- paste(weeks, collapse = ";")
  url <- paste0("https://hmis.health.go.ug/api/29/analytics/dataValueSet.json?dimension=dx:C4oUitImBPK.REPORTING_RATE&dimension=pe:", pe_string, "&dimension=ou:LEVEL-3&displayProperty=NAME")
  data <- pull_url(url)
  return(data)
}

pull_district_monthly_age_specific <- function(months) {
  age_sex_table <- fread("./DHIS2 Reports/tables/age_sex_table.csv")
  indicators <- indicator_table[frequency == "monthly" & grepl("EP01", dhis_name)& grepl("eaths", dhis_name)]$dhis_id
  dx_table <- expand.grid(indicators, age_sex_table$age_sex_id)
  dx_table$dhis_id <- paste(dx_table$Var1, dx_table$Var2, sep = ".")
  dx_string <- paste(dx_table$dhis_id, collapse = ";")
  pe_string <- paste(months, collapse = ";")
  ou_string <- paste(unique(loc_table[level == 1]$location_id), collapse = ";")
  url <- gen_url(dx_string, pe_string, ou_string)
  data <- pull_url(url)
  
  # Merge on location and element names
  setnames(data, c("dataElement", "orgUnit", "categoryOptionCombo"), c("dhis_id", "location_id", "age_sex_id"))
  district_dt <- merge(data, unique(loc_table[, .(location_id, location_name)]), by = "location_id")
  district_dt <- merge(district_dt, indicator_table[, .(dhis_id, code_name)], by = "dhis_id")
  district_dt[, c("dhis_id", "location_id") := NULL]
  district_dt <- merge(district_dt, age_sex_table, by = "age_sex_id")
  
  return(district_dt)
}

gen_week_dt <- function(year_start, week_start, year_end, week_end) {
  week_dt <- data.table()
  for(y in year_start:year_end) {
    if (y == year_start) {
      if(year_end == year_start) {
        week_range <- week_start:week_end
      } else {
        week_range <- week_start:53
      }
    } else if (y == year_end) {
      week_range <- 1:week_end
    } else {
      week_range <- 1:53
    }
    week_dt <- rbind(week_dt, data.table(year = y, week = week_range))
  }
  week_dt[, period := paste0(year, "W", week)]
  return(week_dt)
}

gen_month_dt <- function(year_start, month_start, year_end, month_end) {
  month_dt <- data.table()
  for(y in year_start:year_end) {
    if (y == year_start) {
      if(year_end == year_start) {
        month_range <- month_start:month_end
      } else {
        month_range <- month_start:12
      }
    } else if (y == year_end) {
      month_range <- 1:month_end
    } else {
      month_range <- 1:12
    }
    month_dt <- rbind(month_dt, data.table(year = y, month = month_range))
  }
  month_dt[, period := paste0(year, month)]
  month_dt[nchar(month) == 1, period := paste0(year, "0", month)]
  return(month_dt[])
}

agg_to_parent <- function(dt) {
  dt <- merge(dt, loc_table[, .(location_id, parent)], by = "location_id")
  sum_dt <- dt[, .(value = sum(value, na.rm = T)), by = .(parent, code_name, period)]
  setnames(sum_dt, "parent", "location_id")
  agg_dt <- merge(sum_dt, loc_table[, .(location_id, location_name)], by = "location_id")
  return(agg_dt)
}
