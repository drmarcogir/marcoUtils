#' Spatial thinning for biodiversity records
#'
#' @ indat = input dataframe containing biodiversity records
#' @ sp= colunm name for species
#' @ thindist = thinning distance i.e. spacing between records
#' @nrep = number of replicates
#' @ncores = number of CPUS

thin_wrap_p<-function(indat,sp,thindist,nrep,ncores){
  sp.l<-as.character(unique(indat[,sp])) # list of species
  cl<-makeCluster(ncores) # create cluster
  registerDoParallel(cl) # register cores
  finalresults<-foreach (i=sp.l,.packages="spThin") %dopar% { # foreach loop: note indexing!!!
    spdat<-indat[indat[,sp] %in% sp.l,] # subset dataframe
    th1<-thin(loc.data = spdat,lat.col = "lat", long.col = "lon", # thinning 
              spec.col = sp, thin.par = thindist, reps = nrep,
              locs.thinned.list.return = TRUE, write.files = FALSE, 
              write.log.file = FALSE)   
    tmpresult<-elementcalc(th1) 
    maxdf<-subset(tmpresult,veclen==max(tmpresult$veclen))
    thinlatlon<-th1[[maxdf[1,]$elem]]
    colnames(thinlatlon)<-c("lon","lat")
    thinlatlon$species<-unique(spdat$species)
    thinlatlon
  }
  stopCluster(cl)
  finalresults1<-do.call("rbind",finalresults)
  return(finalresults1)
}