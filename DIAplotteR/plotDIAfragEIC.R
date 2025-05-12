#!/usr/bin/env Rscript

# Script for searching DIA, object aligned -csv output required
# usage Rscript  plotDIAfragEIC.R Iteration Fraglist.csv DIA_Info.txt result_tagged.csv mzmML_dir outputdir
args = commandArgs(trailingOnly=TRUE)

########################
# testing only
#args <- vector()
#args[1] <- 1 # iteration over the annotated list, used in bash for parallelization in case a large fraglist is applied
#args[2] <- "/test/Fraglist_example.csv" # csv list with compound
#args[3] <- "/test/DIA_info.csv" # csv with DIA window information
#args[4] <- "/test/Alignment_Table.csv" # list with annotated results the DIA MS2 spectra should be shown
#args[5] <- "/test" # directory with mzml files
#args[6] <- "/test" # directory to plot the results
########################
library(xcms)
library(stringr)
library(ggplot2)
########################
# assign commandline arguments to variable names
whichTag <- as.numeric(args[1])
fraglist <- read.csv(args[2])
DIA_info <- read.csv(args[3])
tagged_result <- read.csv(args[4])
filedir <- args[5]
output <- args[6]
########################
# Function used for ppm calculation (@author Michael A. Stravs, Eawag <michael.stravs@@eawag.ch>)
ppm <- function(mass, dppm, l=FALSE, p=FALSE)
{
    if(p) return(mass*dppm*1e-6)
    dmass <- mass * (1 + dppm*1e-6)
    if(l) dmass <- c(mass * (1 - dppm*1e-6), dmass)
    return(dmass)
}

########################
#get peakID
peak_ID <- as.character(fraglist[whichTag, 2])

#get intensity row in tagged result
tagged_result <- tagged_result[which(tagged_result$Peak_ID==peak_ID),14:ncol(tagged_result)]
if(nrow(tagged_result)>1){message("More than one Peak ID found in Result table")}
if(nrow(tagged_result)==0){message("No corresponding Peak ID found in Result table")}

#get samplename from colname of max
name <- str_replace(colnames(tagged_result)[which.max(tagged_result)], "Intensity_", "")
#somehow . where in the place of - change if not needed
name <- str_replace(name, "\\.", "-")
name <- str_replace(name, "\\.", "-")
name <- str_replace(name, "\\.", "-")
#
name <- paste0(filedir, "/", name, ".mzML")

message("[plotDIAfragEIC]: Using file ", name )
if(!file.exists(name)){message("Filepath not correct, file does not exist")}

########################
# get mz and rt window
mz <- as.numeric(fraglist$Precursor.m.z[whichTag])

#get mz range for precursor chromatogram
ppm_value <- 8
mz_range <- ppm(mz, ppm_value, l=T)

#get rt window for plotting
rtwindow_min <- 0.3 # in minutes
rt <- c((fraglist[whichTag,5]-rtwindow_min)*60, (fraglist[whichTag,5]+rtwindow_min)*60 )

#get fragments
frag <- as.numeric(fraglist[whichTag, grep("Fragment", colnames(fraglist))])
frag <- frag[which(!is.na(frag))]

#get range of fragments
ppm_value_MS2 <- 40
frag_range <- data.frame()
for(i in 1:length(frag)){
    frag_range[1:2,i] <- ppm(frag[i], ppm_value_MS2, l=T)
}

#get DIA window
window_index <- which(as.numeric(DIA_info$Min) < mz & as.numeric(DIA_info$Max) > mz)
window_index<- window_index[1]

########################
message("[plotDIAfragEIC]: Opening file with mzR" )
# read in file 
MSF <- readMSData(name, mode="onDisk")
if(nrow(DIA_info)!=length(unique(precursorMz(MSF)))-1){message("The size of the DIA_info matrix and the applied mzML files do not match in their length :( ") 
                                            unique(precursorMz(MSF))}

# Get right MS2 window
MSF2 <- extractPrecSpectra(MSF, prec= DIA_info[window_index, 3])

# get precusor chromatogramm
chr_MS1 <- MSnbase::chromatogram(MSF, rt=rt, mz=mz_range, msLevel=1, aggregationFun= "sum")
# get fragments chromatogramms
chr_MS2 <- list()
for(i in 1:ncol(frag_range)){
  x <- MSnbase::chromatogram(MSF2, rt=rt, mz=frag_range[,i], msLevel=2)
  chr_MS2[[i]] <- data.frame(RT=x@.Data[[1]]@rtime/60, Intensity= x@.Data[[1]]@intensity)
}

#Plot them together
MS1 <- data.frame(RT=(chr_MS1@.Data[[1]]@rtime/60), Intensity=chr_MS1@.Data[[1]]@intensity)
MS1 <- MS1[which(!is.na(MS1$Intensity)),]

theme_set(theme_classic())
 c <- ggplot(data=MS1, aes(x=RT, y=Intensity)) + 
        geom_line(data= MS1, color="darkblue") +
        labs(title= paste0("TIC for ", fraglist$Name[whichTag]),
             subtitle= paste0("Precursor MS1: ", fraglist$Precursor.m.z[whichTag]))

for(i in 1:length(chr_MS2)){
    c <- c + geom_line(data=chr_MS2[[i]], linetype= i+1, color="red") 
}
c
c + scale_linetype_manual(  values= c( "dotdash", "dotted" )) #values= c("MS1", frag))
#########################

