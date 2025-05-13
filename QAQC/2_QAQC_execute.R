# Executable Script for QAQC data using peakPantheR 
# Usage as Rexecutable with a settings.yaml file in the same directory
#####################
# Load packages     #
#####################
library(yaml)
library(mzR)
library(peakPantheR)
####################
# Parameters outside of the settingsfile:
MZML_directory <- "./mzml"
OUTPUT_directory <- "./" #=> Where to generate a folder with the output (Results)
INPUT_directory <- "./"  #=> where to find input lists
#Parameters defined in settings file
settings <- yaml::read_yaml("./settings.yaml")
group_pattern <- settings$Samplepattern # => which pattern of the mzml filenames should be used?
list_pattern <- settings$listname
colorcode <- settings$colorcode
Samplegroupfile <- settings$Samplegroupfile
rt_var <- as.numeric(settings$rt_var)
ppm_val <- as.numeric(settings$ppm)
cores <- settings$cores
####################
# Version control  #
####################
version <- "1.0_QEx"
write(paste("Script version: ", version), file=paste0(OUTPUT_directory, "hist_QAQC.txt"))
# store version info into directory
hist <- paste("settings:", '\t', settings )
write(hist, file=paste0(OUTPUT_directory, "hist_QAQC.txt"), append=T)
####################
# Error handling   #
####################
zz <- file("errors_QAQC.txt", open="wt")
sink(zz, type="message")
#####################
# Load in Functions #
#####################
#function for checking file polarity
getPolarity <- function(x){
  MSF    <- mzR::openMSfile(x)
  polarity <- mzR::header(MSF,1)$polarity
  if(polarity ==1){return("Pos")}
  if(polarity==0){return("Neg")}
}
#function for calculation of ppm
ppm <- function(mass, dppm, u=FALSE, l=FALSE){
  if(u) return(mass * (1 + dppm*1e-6))
  if(l) return(mass * (1 - dppm*1e-6))
}
####################
files <- list.files(path= MZML_directory, pattern=".mzML$", full.names = T, recursive=F) #list files in MZML directory
if(nchar(group_pattern)>0){
 message("Use only a part of files")
 files <- files[grep(group_pattern, files)]
}
message("MZML Files:", files)
list_csv <- list.files(path= INPUT_directory, pattern=".csv$", full.names = T) #list files in csv directory
message("Use csv file ", list_csv[grep(list_pattern, list_csv)])
data <- read.csv(list_csv[grep(list_pattern, list_csv)]) 
####################
#check mzml        #
####################
#get files polarity
polarity <- settings$mode
# use the polarity of the first file  
if(polarity == "auto"){
  polarity <- getPolarity(files[1]) 
}
# use mz depending on polarity
if(polarity == "Pos"){
  message("Pos mode detected")
  data$mz <- data$Pos
  }
if(polarity=="Neg"){
  message("Neg mode detected")
  data$mz <- data$Neg
  }
# get Regions of interest from file
ROI <- data.frame(cpdID=as.character(data$No), cpdName= as.character(data$Name), rtMin=(data$RT-rt_var)*60, rt=data$RT*60, rtMax=(data$RT+rt_var)*60, mzMin=ppm(data$mz, ppm_val, l=T),mz=data$mz, mzMax= ppm(data$mz, ppm_val, u=T), stringsAsFactors=FALSE)
ROI <- ROI[which(!is.na(ROI$mzMin)),]
# use peakPantheR
init <-peakPantheRAnnotation(spectraPaths = files, targetFeatTable = ROI)
annotation_result <- peakPantheR_parallelAnnotation(init, ncores=cores, maxApexResidualRatio = 0.8, verbose=TRUE)
annotationObject <- annotationParamsDiagnostic(annotation_result[[1]])
# generate output folder
dir <- paste0(OUTPUT_directory, paste0(list_pattern, "_", polarity))
dir.create(dir)
# generate Plots
outputAnnotationDiagnostic(annotationObject, saveFolder = dir)
# save results
outputAnnotationResult(annotationObject, saveFolder = dir)
save(annotationObject, file=paste0(dir, "/Data.rdata"))
sink(type="message")
close(zz)
message("Success")
Sys.sleep(5)
