#' Create spp zonation file
#'
#' @speclist = species names where these are stored
#' @IUCN = list of IUCN threat levels
#' @dirname = names of the directory where species files are store
#' @outfilepath= path where to save .spp file

zonationspp<-function(speclist=NULL,IUCN=NULL,dirname=NULL,outfilepath=NULL){
  iucndf<-data.frame(IUCN=unique(IUCN))
  iucndf1<-merge(iucndf,iucnw)
  iucndf2<-arrange(iucndf1,weight)
  dataweights<-data.frame(IUCN)
  dataweights1<-merge(iucndf2,dataweights)
  spptab<-paste(dataweights1$weight," 1.0 ","1 ","1 ","0.25 ",dirname,"/",speclist,sep="")
  write.table(spptab,file=outfilepath,row.names=F,quote=F,col.names=F)
  return(spptab)
}
