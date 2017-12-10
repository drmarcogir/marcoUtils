#' Overlay two raster objects and create a summary dataframe
#'
#' @ inrast = input raster object. This is the raster from which values will be extracted and summarized according to the categories of inrast1
#' @ inrast1= input raster object. This is the raster which will be used for summarizing the first raster.
#'
raststats<-function(inrast,inrast1){
  tmpr<-raster(inrast)
  tmpr1<-raster(inrast1)
  tmpr1df<-rastertodf(tmpr1)
  catlist<-unique(tmpr1df[,3])
  results<-NULL
  for (i in 1:length(catlist)){
    tmpr2<-raster(inrast1)
    tmpr2[tmpr2!=catlist[i]]<-NA
    tmpmask<- mask(tmpr, tmpr2)
    restmp<-data.frame(inrast_value=rastertodf(tmpmask)[,3],inrast1_value=catlist[i])
    results<-rbind(restmp,results)
  }
  return(results)
}


