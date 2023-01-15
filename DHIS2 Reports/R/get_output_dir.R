#' Get output directory for results to save in
#'
#' Returns an appropriate path to save results in, creating it if necessary.
#'
#' @param root path to root of output results
#' @param date character date in form of "YYYY_MM_DD" or "today". "today" will be interpreted as today's date.
get_output_dir <- function(root, date) {
  if (date == "today") {
    date <- format(Sys.Date(), "%Y_%m_%d")
  }
  cur.version <- get_latest_output_date_index(root, date = date)
  # 
  dir.name <- sprintf("%s.%02i", date, cur.version + 1)
  dir.path <- file.path(root, dir.name)
  if (!dir.exists(dir.path)) {
    # handle quirk with singularity image default umask
    old.umask <- Sys.umask()
    Sys.umask("002")
    dir.create(dir.path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    Sys.umask(old.umask)
  }
  return(dir.path)
}

#' get the latest index for given an output dir and a date
#'
#' directories are assumed to be named in YYYY_MM_DD.VV format with sane
#' year/month/date/version values.
#'
#' @param dir path to directory with versioned dirs
#' @param date character in be YYYY_MM_DD format
#'
#' @return largest version in directory tree or 0 if there are no version OR
#' the directory tree does not exist
get_latest_output_date_index <- function(dir, date) {
  currentfolders <- list.files(dir)
  
  # subset to date
  pat <- sprintf("^%s[.]\\d{2}$", date)
  date_dirs <- grep(pat, currentfolders, value = T)
  
  if (length(date_dirs) == 0) {
    return(0)
  }
  
  # get the index after day
  date_list <- strsplit(date_dirs, "[.]")
  
  inds <- unlist(lapply(date_list, function(x) x[2]))
  if (is.na(max(inds, na.rm = T))) inds <- 0
  
  return(max(as.numeric(inds)))
}