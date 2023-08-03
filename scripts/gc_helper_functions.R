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

drift_check <- function(flags, n2o_area, n2o_area_mean, n2o_std) {
  
  # Can use unique because each group input to the fn will have the same flags
  flags <- unique(flags)
  # Setting empty flags to NA makes concatenating the flags vector into a 
  # string simpler
  flags <- if(flags == "") NA_character_ else {flags}
  
  flags_new <- c()
  
  # Only checking the st dev for values with 2 acceptable n2o areas
  if (length(n2o_area) == 2 & n2o_area_mean != 0) {
    # Can use unique because each group input to the fn will have the same std
    std <- unique(n2o_std)
    
    n2o_area_perc_diff <- 100*diff(n2o_area)/mean(n2o_area)
    # This is the only case in which flags would be changed
    if (!is.na(n2o_area_perc_diff)) { 
      if (abs(n2o_area_perc_diff) > 10) {
        flags_new <- c(flags_new,
                       sprintf(
                         "Low N2O reproducibility (percent difference: %.2f%%)", 
                         n2o_area_perc_diff))
      }
    }
  }
  
  flags <- c(flags_new, flags)
  flags <- flags[!is.na(flags)]
  flags <- paste(flags, collapse = ", ")
  
  return(flags)
  
}

correct_dry_to_wet <- function(values) {
  T <- 20+273.15 # Lab temp K
  # Partial pressure of water at lab temperature converted to kPa
  pH2O <- exp(24.4543-67.4509*(100/T)-4.8489*log(T/100))*101.325
  
  corrected <- (values/10^6)*(1-pH2O/101.325)*10^6
}
