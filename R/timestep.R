#'
#' Calculate time step between successive ARGOS observations
#'@ = indf input data frame
#'@ = column containing dates

timestep<-function(indf,incol){
    indf <- indf[order(indf[,incol]),]
    datev<-indf[,incol]
    vecl<-length(datev)-1
    res<-NULL
    for (i in 1:vecl){
        tmp<-difftime(datev[i],datev[i+1], units = "hours")
        tmp1<-stri_split_fixed(tmp,"fh")[[1]]
        res[i]<-tmp1
    }
    res1<-as.numeric(res)
    return(res1)
}