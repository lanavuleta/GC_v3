#############################
## SmartChem processing code

## Authors: LV

## 2022/11/02
#############################

# How to: 
# 1) COPY the files you'd like to process in the "data/input" folder.
#    This folder will be emptied by the script.
# 2) Click "Run" (button in the top right corner).
# 3) Check the data/output file for results.

source("scripts/gc_functions.R")
source("scripts/gc_helper_functions.R")

suppressWarnings(suppressMessages(load_packages()))

process_gc()
