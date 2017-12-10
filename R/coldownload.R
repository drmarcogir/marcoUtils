#'
#' @ var= temperature, chlorophyll or all
#'

coldownload<-function(var=NULL,tempres,from,to){
  library(stringi)
  #--------Temperature-------#
  # read web page
  html <- readLines("https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/Monthly/4km/sst")
  # get lines containing the pattern .nc
  ncfiles<-html[grep('\\.nc',html)]
  # filter crap from lines
  v1<-stri_split_fixed(ncfiles,"href='",simplify=TRUE)[,2]
  file.l1<-stri_split_fixed(v1,"'",simplify=TRUE)[,1]
  #--------Chlorophyll-------#
  html <- readLines("https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/Monthly/4km/chlor_a")
  # get lines containing the pattern .nc
  ncfiles<-html[grep('\\.nc',html)]
  # filter crap from lines
  v1<-stri_split_fixed(ncfiles,"href='",simplify=TRUE)[,2]
  file.l<-stri_split_fixed(v1,"'",simplify=TRUE)[,1]
  options(warn=-1)  # suppress warning messages
  if(var=="chlor"){
    for (i in 1:length(file.l)){
      cat("...Clorophyll a file",i,"out of",length(file.l),"...")
      cat("\n")
      tmp<-stri_split_fixed(file.l[i],"/",simplify = TRUE)
      tmp1<-try(download.file(file.l[i],destfile=tmp[,6],quiet=TRUE),silent = TRUE)
    }
  }
  if(var=="temp"){
    for (i in 1:length(file.l1)){
      cat("...Sea surface temperature file",i,"out of",length(file.l1),"...")
      cat("\n")
      tmp<-stri_split_fixed(file.l1[i],"/",simplify = TRUE)
      tmp1<-try(download.file(file.l1[i],destfile=tmp[,6],quiet=TRUE),silent = TRUE)
    }
  }
  if(var=="all"){
    for (i in 1:length(file.l)){
      cat("...Clorophyll a file",i,"out of",length(file.l),"...")
      cat("\n")
      tmp<-stri_split_fixed(file.l[i],"/",simplify = TRUE)
      tmp1<-try(download.file(file.l[i],destfile=tmp[,6],quiet=TRUE),silent = TRUE)
    }

    for (i in 1:length(file.l1)){
      cat("...Sea surface temperature file",i,"out of",length(file.l1),"...")
      cat("\n")
      tmp<-stri_split_fixed(file.l1[i],"/",simplify = TRUE)
      tmp1<-try(download.file(file.l1[i],destfile=tmp[,6],quiet=TRUE),silent = TRUE)
    }
  }
}
