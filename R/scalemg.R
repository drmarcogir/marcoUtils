#' Rescale variables by a grouping factor
#' 
#' @indf = name of input dataframe
#' @group = character vector providing the name of the grouping column
#' @colstand = character vector providing the name of the columns to standardize
#'

scalemg<-function(indf,group,colstand){
  id.l<-unique(indf[,group]) # get unique colum
  results<-NULL
  for (i in 1:length(id.l)){
    tmp<-indf[indf[,group] %in% id.l[i],] # subset dataframe by group
    for(y in 1:length(colstand)){    # loop through columns and standardize them
      tmp$tmp<-scale(tmp[,colstand[y]]) 
      end<-dim(tmp)[2]
      colnames(tmp)[end]<-paste(colstand[y],"s",sep="")
    }
    results<-rbind(tmp,results) # bind group-specific results
  }
  return(results)
}
