# GC_v3

## Summary

Gas Chromatograph processing code v3.

## Highly Qualified Personnel

- Lana Vuleta, lana.j.vuleta@gmail.com
- Colin Whitfield, colin.whitfield@usask.ca

## Instructions

 1) Download R and RStudio. Follow the instructions at https://rstudio-education.github.io/hopr/starting.html.
 2) From the GC Github page, go to *<> Code* -> *Download ZIP*. Unzip the folder.
 3) COPY the GC files to process into the *data/input* folder. This folder will be emptied upon completion of the script. If this is your first time using the tool, the input folder contains an example file for you to test.
 4) Edit the GC files as is described in *process.pdf*.
 5) Open RStudio.
 6) Go to File -> Open Project, and go to the downloaded GC folder. Select *GC_v3.Rproj*.
 7) Go to File -> Open File, and select *scripts/gc_run_me.R*.
 8) Select all code in the script (for example, by using Ctrl+a).
 9) Run the code, either by clicking *Run* (button in the top right corner) or by pressing *Ctrl+Enter*.
 10) Check the data/output file for results. Outputs are named as: runDate_runTime_numberOfProcessedFiles.

## Repo content information

### data/input

Place the raw GC files that you wish to process into this folder. Files will be erased from the folder upon completion of the script. Make sure you add the standards sheet to each file. See process.pdf for more info.

### data/output

Folder created upon script completion. Contains processed csv files.

### scripts

Scripts for processing raw data into standard format.

