#'
#' Download pictures from inaturalist and save them into a folder
#' inlist=list of species for which pictures are desired
#'

downloadpics<-function(inlist,quality,maxresults){
  logfile<-file.create("logfile.txt",overwrite=TRUE)
  for (i in 1:length(inlist)){
    # get observations for a given species
    myobs<-try(get_inat_obs(inlist[i],maxresults=maxresults,quality=quality),silent=TRUE)
    if(class(myobs)=="try-error"){
      next
    } 
    # filter only research level observations
    myobs1<-subset(myobs,quality_grade=="research")
    if(dim(myobs1)[1]==0){
      next
    }
    dir.create(str_replace_all(inlist[i]," ","_"),showWarnings = FALSE)
    url.l<-myobs1$url
    for (x in 1:length(url.l)){
      cat(paste(inlist[i]," image ",x," out of ",length(url.l)))
      cat("\n")
      # read page
      page<-try(readLines(url.l[x]),silent = TRUE)
      if(class(page)=="try-error"){
        next 
      }
      # extract url
      tmp<-page[str_detect(page,"original")][1]
      tmp1<-gsub('\"', "", tmp, fixed = TRUE)
      tmp2<-try(stri_split_fixed(tmp1,"content=",simplify=T)[,2],silent=TRUE)
      if(class(tmp2)=="try-error"){
        next 
      }
      tmp3<-stri_split_fixed(tmp2,">",simplify=T)[,1]
      filen<-paste(str_replace_all(inlist[i]," ","_"),"/",str_replace_all(inlist[i]," ","_"),x,".JPG",sep="")
      # download file
      dfile<-try(download.file(tmp3,destfile=filen,quiet=TRUE),silent=TRUE)
      if(class(dfile)=="try-error"){
        next 
      } else {
        # append rows onto log file
        if(x==length(url.l)){
          sprow<-paste(str_replace_all(inlist[i]," ","_")," completed!")
          write(sprow,file="logfile.txt",append=TRUE)
        }
      }     
    }
  }
}
