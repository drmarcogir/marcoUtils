#'Function for downloading data from GBIF when synonyms are present
#'
#' library(rgbif) #
#' options(gbif_user="myusername", 
#' gbif_pwd="mypassword", gbif_email="myemailaddress") 
#' download.mg("Canis lupus") 
#'@indat=input data frame containing species names
#'@sp=character vector indicating column containing species names
#'@syn=character vector indicanting column containing synonyms
#'

downloadgbifsyn<- function(indat,sp,syn) {
  sp1<-unique(indat[,sp])
  results <- NULL
  for (i in 1:length(sp1)) {
    cat(as.character(sp1[i]))  
    cat("\n")
    spname <- name_backbone(name = sp1[i])$species
    key<- name_backbone(name = sp1[i])$speciesKey
    if(is.null(spname)){
      next
    } else {
      tmpspecies<-download.mg(sp=sp1[i])
      spres<-data.frame(tmpspecies,synonym=NA,gbifspecies=spname,gbifsynonym=NA)
      spname<-unique(indat[indat[,sp] %in% sp1[i],]$species)
      binomial<-word(string =spname,start=1,end=2,sep=fixed(" "))
      binomial1<-gsub(" ", "", binomial, fixed = TRUE)
      filen1<-paste(binomial1,".csv",sep="")
      write.csv(spres,file=filen1,row.names=FALSE) 
      results<-rbind(spres,results)
    }
    tmpdat<-indat[indat[,sp] %in% sp1[i],]
    syn1<-as.character(unique(tmpdat[,syn]))
    for (j in 1:length(syn1)){
      cat(paste(as.character(sp1[i]),"+++","synonym",j))
      cat("\n")
      key1<- name_backbone(name = syn1[j])$speciesKey
      if(is.null(key1)){
       next
      } else {
        
      if(key==key1){
        next
      } else {
        spname1<- name_backbone(name = syn1[j])$species
      }
      if(is.null(spname1)){
        next
      } else {
        tmpspecies1<-download.mg(sp=syn1[j])
      }
      if(is.null(tmpspecies1)){
        next
      } else {
        spres1<-data.frame(species=sp1[i],tmpspecies1[2:4],synonym=syn1[j],gbifspecies=spname,gbifsynonym=spname1)
        results<-rbind(spres1,results)
      }
      }
     filen<-paste(binomial1,"_syn_",j,".csv",sep="")
     write.csv(spres1,file=filen,row.names=FALSE)     
    }
  }
  return(results)
}

