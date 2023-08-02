process_gc <- function() {
  
  # Select input files ---------------------------------------------------------
  filenames <- list.files("data/input/", full.names = T)
  # Remove hidden files (temporary files created when a GC output file is open)
  filenames <- filenames[grep("~\\$", filenames, invert = TRUE)]
  
  if (length(filenames) == 0) {
    stop("The data/input folder is empty. Try again.")
  }
  
  # Process all data -----------------------------------------------------------
  
  print("Reading in the data...")
  
  data <- map(filenames, read_gc)
  
  print("Calibrating N2O values...")
  
  n2o_calibrants <- data %>%
    map(get_n2o_calibrants) %>%
    map(flag_n2o_calibrants)
  
  data_calibrated <- map2(n2o_calibrants, data, n2o_calibration) %>%
    map(apply_correction)
  
  # Write data -----------------------------------------------------------------
  print("Writing the data to file (check the output folder!)...")
  
  # In case if these directories are not yet created, create them
  dir.create(path = "data/output", showWarnings = FALSE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  # To name output files using the input filename (without path)
  filenames_out <- as.list(file.path(here(), 
                             "data/output", 
                             paste0(tools::file_path_sans_ext(basename(filenames)),
                                    "_processed_",
                                    timestamp,
                                    ".xlsx")))
  
  # Grrr the xlsx library crashes on this version of RStudio. This is a less
  # beautiful workaround
  pmap(list(data_calibrated, n2o_calibrants, filenames_out), write_data)
  
  # So that the "data/input" folder is empty for the next runs of the script
  print("Removing files in the input folder...")
  options(warn = -1)
  file.remove(list.files("data/input/", full.names = T))
  options(warn = 0)
  
  print("Complete.")
  
}

read_gc <- function(filename) {
  
  data <- filename %>%
    read_xlsx(sheet = 1) %>%
    select(exetainer_ID = RUNINFO, 
           file,
           ch4_tcd  = ends_with("TCD_QTY"), 
           ch4      = ends_with("CH4_QTY"), 
           co2_ppmv = ends_with("CO2_QTY"),
           n2o_area = contains("N2O_AREA")) %>%
    # At the 50000 ppm mark, we switch from using FID detector values to TCD
    # detector values, as TCD values are more accurate past that level
    mutate(ch4_ppmv = if_else(ch4 > 50000 | ch4 == 0, ch4_tcd, ch4)) %>%
    select(-c(ch4_tcd, ch4))
  
  return(data)
  
}

get_n2o_calibrants <- function(data) {
  
  data <- data %>%
    mutate(row_num = row_number())
  
  # Want a sheet in the final excel that indicates any flags of interest. Here
  # we produce the dataframe that will be stored in that sheet
  n2o_std_all <- data.frame(n2o_std_regex = c("0\\.1", 
                                              "0\\.317", 
                                              "0\\.69", 
                                              "0\\.98", 
                                              "9\\.52", 
                                              "80"))
  
  n2o_calibrants <- filename %>%
    read_xlsx(sheet = 2) %>%
    rename(exetainer_ID  = RUNINFO,
           n2o_std = STANDARD) %>%
    left_join(data, by = join_by(exetainer_ID)) %>%
    # Only use the 1st 2 instances of each stnd from the 1st 50 rows
    filter(row_num <= 50) %>%
    select(n2o_std, n2o_area) %>%
    # Standardize all of the standards (ie collapse 0.3171 and 0.317 into one 
    # std, collapse 0.98 and 0.989 into one std)
    regex_left_join(n2o_std_all, by = c("n2o_std" = "n2o_std_regex")) %>%
    # This ugly looking "\\\\" is to remove the back slashes that exist in the
    # n2o_std_regex column (double backslash is the escape character that lets
    # R know that the period that follows means a period rather than 'any
    # character', which is the other meaning of a period in regular expressions)
    mutate(n2o_std = as.numeric(str_remove(n2o_std_regex, "\\\\"))) %>%
    # Filter out any non-N2O stds (any standards with non-numeric inputs or inputs
    # that don't match the expected standards)
    filter(!is.na(n2o_std)) %>%
    select(n2o_std, n2o_area) %>% 
    group_by(n2o_std) %>%
    mutate(flags = ifelse(all(n2o_area == 0), "All areas are zero", 
                          case_when(sum(n2o_area != 0) == 1 ~ "One non-zero area", 
                                        TRUE ~ ""))) %>%
    # For all standards except for those where all values are 0, remove the rows
    # where N2O area == 0 to later calculate the mean
    filter(str_detect(flags, "All") | 
             (!str_detect(flags, "All") & n2o_area != 0)) %>%
    # Only first two instances of each standard are desired. Drift is more 
    # likely if later standards are used (ex. if GC pulls up the stnd multiple 
    # times and it runs out, it'll pull up lab air, leading to drift)
    slice(1:2) %>% 
    summarise(n2o_area_mean = round(mean(n2o_area, na.rm = TRUE), 4),
              # If there is a 10% or more difference between repeats of each stnd
              # (if repeats exist), there might be issues with the curve fits
              flags   = drift_check(flags, n2o_area, n2o_area_mean, n2o_std)) %>%
    mutate(n2o_area = n2o_area_mean) %>%
    select(-n2o_area_mean)
    
}

flag_n2o_calibrants <- function(n2o_calibrants) {
  
  # Want a sheet in the final excel that indicates any flags of interest. Here
  # we produce the dataframe that will be stored in that sheet
  n2o_std_all <- data.frame(n2o_std = c(0.1, 0.317, 0.69, 0.98, 9.52, 80, 100))
  
  n2o_calibrants <- n2o_calibrants %>% 
    right_join(n2o_std_all, by = c("n2o_std")) %>%
    rowwise() %>%
    mutate(flags = ifelse(is.na(n2o_area), "Standard was not run", flags)) %>%
    select(n2o_std, n2o_area, flags)
  
  return(n2o_calibrants)
  
}

n2o_calibration <- function(n2o_calibrants, data) {
  
  model_0.98 <- lm(n2o_std ~ -1 + n2o_area + I(n2o_area^2), 
                   n2o_calibrants, 
                   subset = n2o_std <= 0.99  & 
                            !is.na(n2o_area) & 
                            n2o_area > 0)
  
  # Comment 1: Cam determined that there exist issues with the 9.52 ppm 
  # standard. For now, do not use the 9.52 model NOR the standard when running 
  # the 80 model.
  model_9.52 <- lm(n2o_std ~ -1 + n2o_area + I(n2o_area^2), 
                   n2o_calibrants, 
                   subset = n2o_std <= 9.52    & 
                     n2o_std != 9.52  & # See Comment 1
                     !is.na(n2o_area) & 
                     n2o_area > 0)
  
  model_80   <- lm(n2o_std ~ -1 + n2o_area + I(n2o_area^2), 
                   n2o_calibrants, 
                   subset = n2o_std <= 80    & 
                     n2o_std != 9.52  & # See Comment 1
                     !is.na(n2o_area) & 
                     n2o_area > 0)
  
  
  # If the 9.52 calibration is corrected, edit this to match with the 80 setup!
  use_952_model <- FALSE
  
  if (is.na(filter(n2o_calibrants, n2o_std == 80)$n2o_area)) {
    # No 80 stnd means that we will not be calculating the 80 model (note that
    # the 9.52 stnd will also not be used as per Comment 1)
    use_80_model <- FALSE
  } else {
    use_80_model <- TRUE
  }
  
  data_calibrated <- data %>%
    mutate(n2o_0.98 = model_0.98$coefficients[2] * n2o_area^2 + 
                      model_0.98$coefficients[1] * n2o_area,
           n2o_9.52 = model_9.52$coefficients[2] * n2o_area^2 +
                      model_9.52$coefficients[1] * n2o_area,
           n2o_80   = model_80$coefficients[2]   * n2o_area^2 + 
                      model_80$coefficients[1]   * n2o_area) %>%
    # See Comment 1. In future edit this mutate to account for use of the 9.52 model
    mutate(n2o_model_used = case_when(isTRUE(use_80_model) & n2o_0.98 >= 5 ~ 80,
                                      TRUE ~ 9.52),
           n2o_ppmv = case_when(n2o_model_used == 80 ~ n2o_80,
                                TRUE                 ~ n2o_0.98)) %>%
    select(file, exetainer_ID, ch4_ppmv, co2_ppmv, n2o_ppmv, n2o_model_used) 
   
  return(data_calibrated)
      
}

apply_correction <- function(data_calibrated) {

  data_calibrated <- data_calibrated %>%
    mutate(ch4_ppmv = correct_dry_to_wet(ch4_ppmv),
           co2_ppmv = correct_dry_to_wet(co2_ppmv),
           n2o_ppmv = correct_dry_to_wet(n2o_ppmv))
    
}

write_data <- function(data_calibrated, n2o_calibrants, filenames_out) {
  
  sheets <- list('calibrated_data' = data_calibrated, 
                 'calibration_flags' = n2o_calibrants)
  
  write.xlsx(sheets, filenames_out)
  
}
    