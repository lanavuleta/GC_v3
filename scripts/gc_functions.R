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
  
  data_calibrated <- map2(n2o_calibrants, data, n2o_calibration)
  
  # Write data -----------------------------------------------------------------
  print("Writing the data to file (check the output folder!)...")
  
  # In case if these directories are not yet created, create them
  dir.create(path = "data/output", showWarnings = FALSE)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M_")
  # To name output files using the input filename (without path)
  filenames_out <- as.list(file.path(here(), 
                             "data/output", 
                             paste0(timestamp,
                                    "processed_",
                                    map(filenames, basename))))
  
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
           co2      = ends_with("CO2_QTY"),
           n2o_area = contains("N2O_AREA")) %>%
    # At the 50000 ppm mark, we switch from using FID detector values to TCD
    # detector values, as TCD values are more accurate past that level
    mutate(ch4 = if_else(ch4 > 50000 | ch4 == 0, ch4_tcd, ch4)) %>%
    select(-ch4_tcd)
  
  return(data)
  
}

get_n2o_calibrants <- function(data) {

  standards_regex <- "0\\.1|0\\.317|0\\.69|0\\.98|9\\.52|(\\D|^)80|ref|low|high"
  
  # For some runs, the 9.52 ppm is used as the high check instead of 80 ppm.
  # This is usually if the run was of atmospheric samples or reruns known to 
  # have low N2O concentrations. This would (usually) be indicated by IDs like
  # "high 9.52 N2O", or IDs in which both the keywords "high" and "9.52" exist
  high_stnd <- ifelse(any(grepl("high", data$exetainer_ID) & 
                            grepl("9.52", data$exetainer_ID)),
                      9.52,
                      80)
  
  n2o_calibrants <- data %>% 
    select(exetainer_ID, n2o_area) %>%
    filter(grepl(exetainer_ID, 
                 pattern = standards_regex,
                 ignore.case = TRUE)) %>%
    get_stnd_info(high_stnd) %>%
    # exetainer_ID is not consistent between files in naming each standard,
    # so we pull out the ppm value from each ID
    mutate(n2o_std = as.numeric(str_extract(exetainer_ID, 
                                            pattern = standards_regex))) %>%
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
              flags         = drift_check(flags, n2o_area, n2o_area_mean, n2o_std)) %>%
    mutate(n2o_area = n2o_area_mean) %>%
    select(-n2o_area_mean)
    
}

flag_n2o_calibrants <- function(n2o_calibrants) {
  
  # Want a sheet in the final excel that indicates any flags of interest. Here
  # we produce the dataframe that will be stored in that sheet
  n2o_std_all <- data.frame(n2o_std_regex = c("0\\.1", 
                                              "0\\.317", 
                                              "0\\.69", 
                                              "0\\.98", 
                                              "9\\.52", 
                                              "80"))
  
  n2o_calibrants <- n2o_calibrants %>% 
    # So that regex_right_join can match rows, change n2o_std to character
    mutate(n2o_std = as.character(n2o_std)) %>%
    regex_right_join(n2o_std_all, by = c("n2o_std" = "n2o_std_regex")) %>%
    rowwise() %>%
    mutate(n2o_std = ifelse(is.na(n2o_std),
                            # Store numbers rather than regex in n2o_std column
                            str_remove(n2o_std_regex, "\\\\"),
                            n2o_std),
           flags = ifelse(is.na(n2o_area), "Standard was not run", flags)) %>%
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
  # the 80 model. Code exists here if ever future use of this model are desired.
  # model_9.52 <- lm(n2o_std ~ -1 + n2o_area + I(n2o_area^2), 
  #                  n2o_calibrants, 
  #                  subset = n2o_std <= 9.52  & 
  #                          !is.na(n2o_area) & 
  #                           n2o_area > 0)
  
  model_80   <- lm(n2o_std ~ -1 + n2o_area + I(n2o_area^2), 
                   n2o_calibrants, 
                   subset = n2o_std <= 80    & 
                            n2o_std != 9.52  & # See Comment 1
                            !is.na(n2o_area) & 
                            n2o_area > 0)
  
  data_calibrated <- data %>%
    mutate(n2o_0.98 = model_0.98$coefficients[2] * n2o_area^2 + 
                      model_0.98$coefficients[1] * n2o_area,
           # See Comment 1
           # n2o_9.52 = model_9.52$coefficients[2] * n2o_area^2 + 
           #            model_9.52$coefficients[1] * n2o_area, 
           n2o_80   = model_80$coefficients[2]   * n2o_area^2 + 
                      model_80$coefficients[1]   * n2o_area) %>%
    mutate(n2o = case_when(n2o_0.98 <= 1                 ~ n2o_0.98,
                           # See Comment 1
                           # n2o_9.52 > 1 & n2o_9.52 <= 10 ~ n2o_9.52,
                           TRUE                          ~ n2o_80),
           n2o_model_used = case_when(n2o_0.98 <= 1                 ~ 0.98,
                                      # See Comment 1
                                      # n2o_9.52 > 1 & n2o_9.52 <= 10 ~ 9.52,
                                      TRUE                          ~ 80)) %>%
    select(file, exetainer_ID, ch4, co2, n2o, n2o_model_used) 
   
  return(data_calibrated)
      
}

write_data <- function(data_calibrated, n2o_calibrants, filenames_out) {
  
  sheets <- list('calibrated_data' = data_calibrated, 
                 'calibration_flags' = n2o_calibrants)
  
  write.xlsx(sheets, filenames_out)
  
}
    