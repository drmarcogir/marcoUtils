#'
#'  Function recoding variable names for partial dependency plots
#'  invarmod@= column containing model name
#'  replacevar@= new name for variable of interest
#'


recodevar<-function(invarmod,replacevar,responsetitle){
  # subset by model
  tmp<-subset(results,model==invarmod)
  # lookupdataframe 
  lookup<-data.frame(varname=unique(tmp$varname))
  lookup$varname<-as.character(lookup$varname)
  # replace with new variable name
  lookup$newname<-replace(lookup$varname,lookup$varname==invarmod,replacevar)
  lookup$id<-1:dim(lookup)[1]
#  lookup$newname<-as.factor(lookup$newname)
  # make sure that new variable is the last one
  replonly<-subset(lookup,newname==replacevar)
  other<-subset(lookup,newname!=replacevar)
  newdf<-data.frame(newname1=factor(c(sort(other$newname),replonly$newname),levels=c(sort(other$newname),replonly$newname)),
                    id=c(other$id,replonly$id))
  newdf$newid<-1:dim(newdf)[1]
  dd<-merge(newdf,lookup)
  dd1<-dplyr::arrange(dd,newid)
  # merge with tmp dataframe
  tmp1<-merge(tmp,dd1[,c("newname1","varname")])
  tmp1$responsename<-responsetitle
  return(tmp1)
}

