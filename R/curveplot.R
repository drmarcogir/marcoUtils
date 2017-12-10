#'
#' Convenience function for getting response curves 
#' for INLA models fitted for Gerardo's project
#' @indat = input dataframe containing original data 
#' @inres = input dataframe containing results from modelling exercise
#'


curveplot<-function(indat,inres){
  tmp<-subset(indat,is.na(Farm)) # not general enough
  var.l<-as.character(unique(tmp$predrespcurve)) # not general enough
  res<-NULL
  for(i in 1:length(var.l)){
    tmp1<-subset(tmp,predrespcurve==var.l[i]) 
    cols<-c(var.l[i],paste(var.l[i],"s",sep=""))
    tmp2<-data.frame(predrespcurve=var.l[i],tmp1[,cols])
    colnames(tmp2)[2:3]<-c("value","values")
    res<-rbind(tmp2,res)
  }
  tmp1<-merge(inres,res)
  return(tmp1)
}
