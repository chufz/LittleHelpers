# load Packages which are needed
library(yaml)
library(doParallel)
####################
# print working directory
setwd(".")
message(getwd())
####################
# Error handling   #
####################
zz <- file("errors_mzml.txt", open="wt")
sink(zz, type="message")

# read in settings file
settings <- read_yaml("./settings.yaml")

message("Use msconvert at ", settings$msconvertpath)

# list raw files in folder
raw <- list.files(path="../", pattern=".raw", full.names=TRUE)

if(length(raw)== 0){
message("No raw files found in the upper folder, Sorry :(")
Sys.sleep(2)
stop()
}

if(settings$last_file == TRUE){
message("Last file will be skipped")
raw <- raw[-length(raw)]
}
raw_name <- tools::file_path_sans_ext(basename(raw))

if(!dir.exists("mzml")){
message("First init, creating folder")
dir.create("./mzml")
}else{
message("Found preexisting mzml, will only add new ones")
mzml_name <- tools::file_path_sans_ext(list.files("./mzml/"))
# remove files from raw vector
j=0
for(i in 1:length(raw_name)){
if(raw_name[i] %in% mzml_name){
message(raw_name[i], " already exists", i)
raw <- raw[-(i+j)]
j=j+1
}
}
message(raw_name)
message(raw)
if(length(raw)== 0){
message("No new raw files found, i have nothing to add")
Sys.sleep(2)
stop()
}
}

message("Start running conversion, please wait...")
cl <- makeCluster(settings$cores)
registerDoParallel(cl)
foreach(i=1:length(raw)) %dopar% {
    cmd <- paste0(settings$msconvertpath, " --mzML --64 --zlib --filter \"peakPicking vendor msLevel=1-\" ", raw[i], " -o ", "./mzml")
    system(cmd)
  }
sink(type="message")
close(zz)
message("Success")
Sys.sleep(5)
