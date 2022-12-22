load_packages <- function() {

  # Determine which packages are missing and must be installed
  package_list <- c("dplyr", "readxl", "magrittr", "stringr", "purrr",
                    "lubridate", "fuzzyjoin", "tidyverse", "openxlsx", "here")
  packages_new <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(packages_new)) install.packages(packages_new)
  
  # Load each desired package
  library(dplyr)
  library(readxl)
  library(magrittr)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(fuzzyjoin)
  library(tidyverse)
  library(openxlsx)
  library(here)
  
}


drift_check <- function(flags_col, n2o_area_col, n2o_std_col) {
  
  flags <- c()
  
  flags_og <- unique(flags_col)
  # Setting empty flags to NULL makes concatenating the flags vector into a 
  # string simpler
  flags_og <- if(flags_og == "") NA_character_ else {flags_og}
  
  if (length(n2o_area_col) == 2) {
    # The std for both rows are the same, but both are read in here, hence unique()
    # is used
    std <- unique(n2o_std_col)
    
    n2o_area_perc_diff <- 100*diff(n2o_area_col)/(sum(n2o_area_col)/2)
    # This is the only case in which flags would be changed
    if (!is.na(n2o_area_perc_diff)) { 
      if (abs(n2o_area_perc_diff) > 10) {
        flags <- c(flags,
                   sprintf(
                     "Low N2O Reproducibility (percent difference: %.2f%%)", 
                     n2o_area_perc_diff))
      }
    }
  }
  
  flags <- c(flags, flags_og)
  flags <- flags[!is.na(flags)]
  flags <- paste(flags, collapse = ",")
  
  return(flags)
  
}

stnd_checks <- function(n2o_cal, calibration_ppm, check_pattern) {
  # TBD I am making an assumption here, that the checks would NOT be named
  # "0.1 ppm N2O", even though that COULD happen. Alter code and/or change
  # future GC runs?
  
  # If there's a decimal point in the ppm value, the regex will need to escape 
  # the decimal point, as "." in regex indicates "any character", while \\. in 
  # regex indicates a decimal point
  calibration_ppm_for_regex <- ifelse(grepl("\\.", calibration_ppm),
                                      str_replace(calibration_ppm, 
                                                  "\\.", 
                                                  "\\\\."),
                                      calibration_ppm)
  
  # Regex pattern that'll match IDs that do contain the # (ex 0.1) but not the 
  # check string (LOW/REF/HIGH).
  # For 0.1 ppm, calibration_pattern looks like: "^(?!.*?low).*0\\.1"
  calibration_pattern <-  paste0("^(?!.*?", check_pattern, ").*", 
                                 calibration_ppm_for_regex)
  
  if (any(grepl(pattern     = calibration_pattern,
                x           = n2o_cal$exetainer_ID,
                ignore.case = TRUE,
                perl        = TRUE))) {
    
    # If we have calibrations, we can discard any checks
    n2o_cal <- n2o_cal %>%
      filter(!grepl(pattern     = check_pattern,
                    x           = exetainer_ID,
                    ignore.case = TRUE))
    
  } else {
    
    # If there are NO calibrations but there are checks, rename the ID to be
    # more descriptive. We don't need to check if there are any checks, as the
    # mutate statement will only mutate checks, and otherwise won't make any 
    # changes (but we will flag the nonexistence of the standard later)
    n2o_cal <- n2o_cal %>%
      mutate(exetainer_ID = ifelse(grepl(pattern     = check_pattern,
                                         x           = exetainer_ID,
                                         ignore.case = TRUE),
                                   paste0(calibration_ppm, " ppm N2O"),
                                   exetainer_ID))
  }
  
  return(n2o_cal)
}