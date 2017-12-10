
ensembledf<-function(path,splist,projname){
    results<-NULL
    for (i in 1:length(splist)){
        file<-paste(path,splist[i],"/",projname,"/",projname,"_",splist[i],"_ensemble.grd",sep="")
        tmpr<-try(raster(file))
        if(class(tmpr)=="try-error"){
            next
        } else {
            tmpdf<-rastertodf(tmpr)
            tmpdf$species<-splist[i]
            results<-rbind(tmpdf,results)
        }
    }
    results1<-spread(results,species,value)    
    return(results1)
}



