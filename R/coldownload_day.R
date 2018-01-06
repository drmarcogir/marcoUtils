#' Function for downloading ocean color data
#'
#' @ var= temperature, chlorophyll or all
#' @ from = start year
#' @ to = end year
#'

coldownload<-function(var=NULL,from,to){
  library(stringi)
  # generate sequence of years
  year.l<-seq(from=from,to=to,by=1)
#--Get file URLs
  file.l1<-vector("list",length(year.l))
  for (i in 1:length(year.l)){
  yrstr<-paste("https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/Daily/4km/sst/",
        year.l[i],"/",sep="")
  #--------Temperature-------#
  # read web page
  html <- readLines(yrstr)
  # get lines containing the pattern .nc
  ncfiles<-html[grep('\\.nc',html)]
  # filter crap from lines
  v1<-stri_split_fixed(ncfiles,"href='",simplify=TRUE)[,2]
  fileltmp<-stri_split_fixed(v1,"'",simplify=TRUE)[,1]
  fileltmp1<-fileltmp[stri_detect_fixed(fileltmp,"_SST")]
  file.l1[[i]]<-fileltmp1
  }
  file.l1<-do.call("c",file.l1)

  #--------Chlorophyll-------#
  file.l<-vector("list",length(year.l))
  for (i in 1:length(year.l)){
  yrstr<-paste("https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/Daily/4km/chlor_a/",
        year.l[i],"/",sep="")
  #--------Temperature-------#
  # read web page
  html <- readLines(yrstr)
  # get lines containing the pattern .nc
  ncfiles<-html[grep('\\.nc',html)]
  # filter crap from lines
  v1<-stri_split_fixed(ncfiles,"href='",simplify=TRUE)[,2]
  fileltmp<-stri_split_fixed(v1,"'",simplify=TRUE)[,1]
  file.l[[i]]<-fileltmp
  }
  file.l<-do.call("c",file.l)
#-- Download files
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
