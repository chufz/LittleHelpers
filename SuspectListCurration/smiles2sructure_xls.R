# manipulation of excel files, adding information 
# Jchemstyle

smiles2structure_xlsx <- function(input,sheet_no, smilecolumn, outputcolumn, resolution, outputfilename){
    library(xlsx)
    library(rcdk)
    rcdkplot <- function(iatom,width=200,height=200) {
        par(mar=c(0,0,0,0)) # set margins to zero since this isn't a real plot
        dep <- rcdk::get.depictor(width=width, height=height)
        temp1 = rcdk::view.image.2d(iatom,dep) # get Java representation into an image matrix. set number of pixels you want horiz and vertical
        plot(NA,NA,xlim=c(1,10),ylim=c(1,10),xaxt='n',yaxt='n',xlab='',ylab='') # create an empty plot
        rasterImage(temp1,1,1,10,10) # boundaries of raster: xmin, ymin, xmax, ymax. here i set them equal to plot boundaries
    }
    if(!dir.exists("./temp_files")){dir.create("./temp_files")}
    file <- xlsx::read.xlsx2(input, sheet_no, header=TRUE, colClasses="character")
    for(i in 1:nrow(file)){
        jpeg(paste0("./temp_files/",i,"_temp.jpeg"), quality = 75, width = 200, height = 200)
        #read in smile
        iatom <- rcdk::parse.smiles(smiles = as.vector(file[i, smilecolumn]))[[1]]
        #plot structure 2D
        rcdkplot(iatom)
        dev.off()
    }
    #get list of all files
    files <- list.files("./temp_files",full.names = T) 
    #get excel file
    wb <- xlsx::loadWorkbook(file=input)
    sheet <- xlsx::getSheets(wb)
    rows <- xlsx::getRows(sheet[[sheet_no]],rowIndex=2:(length(files)+1))
    
    xlsx::setRowHeight(rows, 152)
    xlsx::setColumnWidth(sheet[[sheet_no]], outputcolumn, 28.5)
    #put pictures into excel file
    for(i in 1:nrow(file)){
        if(file.exists(paste0("./temp_files/",i,"_temp.jpeg"))){
        xlsx::addPicture(sheet[[sheet_no]], file=paste0("./temp_files/",i,"_temp.jpeg"),  scale=1, startRow=1+i, startColumn=outputcolumn)
    }}
    return(wb)
    #xlsx::saveWorkbook(wb, file= outputfilename)
}
smiles2inchikey_xlsx <- function(input,sheet_no, smilecolumn, outputcolumn, outputfilename){
    library(xlsx)
    library(ChemmineOB)
    
    file <- xlsx::read.xlsx2(input, sheet_no, header=TRUE, colClasses="character")
    inchikey <- vector(length=nrow(file))
    #read in smile
    smiles <- file[, smilecolumn]
    for(i in 1:nrow(file)){
        #get inchikey
        inchikey[i] <- ChemmineOB::convertFormat("SMI", "INCHIKEY", smiles(i))
            }
    #get excel file
    wb <- xlsx::loadWorkbook(file=input)
    sheet <- xlsx::getSheets(wb)
    rows <- xlsx::getRows(sheet[[sheet_no]],rowIndex=2:(nrow(file)+1))
    cells <- xlsx::createCell(rows ,outputcolumn)
    xlsx::setCellValue(cells[i+1, outputcolumn][[1]],i)
    xlsx::saveWorkbook(wb, file= outputfilename)
}
smiles2pubchemlink_xlsx <- function(input,sheet_no, smilecolumn, outputcolumn, outputfilename){}
smiles2exactmass_xlsx <- function(input,sheet_no, smilecolumn, outputcolumn, outputfilename){}
smiles2XlogP_xlsx <- function(input,sheet_no, smilecolumn, outputcolumn, outputfilename){}
smiles2tpsa_xlsx <- function(input,sheet_no, smilecolumn, outputcolumn, outputfilename){}
smiles2molformula_xlsx <- function(input,sheet_no, smilecolumn, outputcolumn, outputfilename){}



smiles2structure_xlsx("./Schreibtisch/test.xlsx", 1, 2,3, 200, "./Schreibtisch/test_2.xlsx")
wb <- smiles2structure_xlsx(input="~/test.xlsx",sheet_no = 1 ,smilecolumn =  6 , outputcolumn =   8,resolution =  200, outputfilename = "~/Documents/test.xlsx" )
xlsx::saveWorkbook(wb, file="./Schreibtisch/Ufz_PesticidesMarker_revised.xlsx")
