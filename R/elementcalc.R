#' Given a list, dataframes or matrices it gives the dimensions of each one of them
#' This is a support function for the thin_wrap_p function
#' @ inlist = input list
#'


elementcalc<-function(inlist){
  result<-NULL
  for (b in 1:length(inlist)){
    df<-data.frame(veclen=dim(inlist[[b]])[1],elem=b)
    result<-rbind(df,result)
  }
  return(result)
}
