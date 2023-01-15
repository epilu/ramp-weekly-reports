library(data.table); library(httr); library(jsonlite)
setwd("D:/Ramp")
# Functions
get_json <- function(call) {
  get <- GET(call, authenticate(USERNAME, PASSWORD))
  get_text <- content(get, "text")
  get_json <- fromJSON(get_text, flatten = T)
  return(get_json)
}

pull_ids <- function(ids) {
  dt <- data.table()
  for (i in ids) {
    print(which(ids == i))
    call <- file.path(base, "organisationUnits", i)
    unit <- get_json(call)
    out_dt <- data.table(
      name = unit$name,
      parent = unit$parent$id,
      path = unit$path
    )
    dt <- rbind(dt, out_dt, fill = T)
  }
  return(dt)
}

get_last <- function(string) {
  unlist(lapply(strsplit(string, "/"), tail, n = 1))
}

get_level <- function(string) {
  unlist(lapply(strsplit(string, "/"), length)) - 1
}

# Set up
cred <- jsonlite::read_json("D:/DHIS2_Data/dhis2_credentials.json")
USERNAME <- cred$new$username
PASSWORD <- cred$new$password


base <- "https://hmis.health.go.ug/api"

# Facility
call <- file.path(base, "organisationUnits?paging=false&level=5")
facilities <- get_json(call)
facility_ids <- unique(facilities$organisationUnits$id)
f_dt <- data.table()
for (i in facility_ids) {
  print(which(facility_ids == i))
  call <- file.path(base, "organisationUnits", i)
  unit <- get_json(call)
  out_dt <- data.table(
    name = unit$name,
    parent = unit$parent$id,
    path = unit$path
  )
  if("geometry" %in% names(unit)) {
    out_dt$latitude <- unit$geometry$coordinates[2]
    out_dt$longitude <- unit$geometry$coordinates[1]
  }
  f_dt <- rbind(f_dt, out_dt, fill = T)
}

# Pull admin regions
s_dt <- pull_ids(unique(f_dt$parent))
d_dt <- pull_ids(unique(s_dt$parent))
r_dt <- pull_ids(unique(d_dt$parent))
n_dt <- pull_ids(unique(r_dt$parent))

# Combine the different levels
dt <- rbindlist(list(f_dt, s_dt, d_dt, r_dt, n_dt), fill = T)

# Clean up

dt[, id := get_last(path)]
dt[, level := get_level(path)]
dt[, path_to_top_parent := substring(gsub("/", ",", path), 2)]

setnames(dt, c("name", "id"), c("location_name", "location_id"))
dt[, path := NULL]

# Pull facility group sets
call <- file.path(base, "organisationUnitGroupSets")
sets <- get_json(call)
for (i in sets$organisationUnitGroupSets$id) {
  name <- tolower(gsub(" ", "_", sets$organisationUnitGroupSets[sets$organisationUnitGroupSets$id == i,]$displayName))
  if(grepl("complex", name)) next
  print(name)
  call <- file.path(base, "organisationUnitGroupSets", i)
  groups <- get_json(call)
  ids <- groups$items$id
  for (j in ids) {
    call <- file.path(base, "organisationUnitGroups", j)
    get <- GET(call, authenticate(USERNAME, PASSWORD))
    get_text <- content(get, "text")
    group <- fromJSON(get_text, flatten = T)
    dt[location_id %in% group$organisationUnits$id, (name) := group$name]
  }
}

write.csv(dt, "./DHIS2 Reports/tables/location_hierarchy.csv", row.names = F)

