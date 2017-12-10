#' Spatial thinning for biodiversity records
#'
#' @ indat = input dataframe containing biodiversity records
#' @ sp= colunm name for species
#' @ thindist = thinning distance i.e. spacing between records
#' @nrep = number of replicates
#'

thin_wrap<-function(indat,sp,thindist,nrep){
    sp.l<-unique(indat[,sp])
    finalresults<-NULL
    for (i in 1:length(sp.l)){
        cat(paste("Thinning -", sp.l[i]))
        cat("\n")
        spdat<-indat[indat[,sp] %in% sp.l[i],]
        th1<-thin(loc.data = spdat,lat.col = "lat", long.col = "lon", 
                  spec.col = sp, thin.par = thindist, reps = nrep,
                  locs.thinned.list.return = TRUE, write.files = FALSE, 
                  write.log.file = FALSE)
        result<-NULL
        for (b in 1:length(th1)){
            df<-data.frame(veclen=dim(th1[[b]])[1],elem=b)
            result<-rbind(df,result)
        }
        maxdf<-subset(result,veclen==max(result$veclen))
        thilatlon<-th1[[maxdf[1,]$elem]]
        colnames(thilatlon)<-c("lon","lat")
        spdat1<-merge(spdat,thilatlon)
        finalresults<-rbind(spdat1,finalresults)
    }
    return(finalresults)
}