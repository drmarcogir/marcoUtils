#'
#'  Function creating plots from state-space model output
#'  inmod@= paths to model objects
#'

ssmdataget<- function(inmod){
  # list where to store results
  results<-NULL
  for (i in 1:length(inmod)){
    # load model object
    inmod1<-get(load(inmod[i]))
    # if not hierarchical
    if(class(inmod1)=="ssm"){
      dplyr::select(inmod1[[1]]$summary,c(id,date,lon,lat,b,b.5)) %>% as.data.frame() ->tmp
    } else {
      # if hierarchical
      dplyr::select(inmod1$summary,c(id,date,lon,lat,b,b.5)) %>% as.data.frame() ->tmp
    }
    results<-rbind(tmp,results)
  }
  return(results)
}
