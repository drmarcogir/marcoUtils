#' Create raster from Birdlife ranges (in shapefile format)
#'
#' @speclist = species names where these are stored
#' @inpath = path where shapefiles are store
#' @rastg = raster grid
#' @outpath = path of directory where to write the rasters
#' @inproj= projection of input shapefiles
#' @outproj= projection outputfiles


birdliferast<-function(speclist=NULL,inpath=NULL,outpath=NULL,rastg=NULL,season=NULL,inproj=NULL,outproj=NULL){
    #results<-NULL
    gridf<-rastertodf(rastg)
    inproj1<-CRS(inproj)
    outproj1<-CRS(outproj)
    #for (i in 1:length(speclist)){
      foreach(i=1:length(speclist)) %dopar% {
        cat(as.character(speclist[i]))
        cat("\n")
        binomial<-str_split_fixed(speclist[i],".shp",3)[,1]
        filep<-paste(inpath,"/",speclist[i],sep="")
        tmpshp<-readShapePoly(filep,force_ring=TRUE,delete_null_obj=TRUE,proj4string=inproj1)
        tmpshp1<-spTransform(tmpshp,outproj1)
        tmpshp2<-try(subset(tmpshp1,PRESENCE < 3))
        tmpshp3<-try(subset(tmpshp2,ORIGIN < 3))
        cats<-raster::extract(rastg,tmpshp3,small=T) # extract categories
        data.frame(value=as.numeric(na.exclude(unlist(cats))),presabs=1) %>% # create data frame with presabs
        full_join(gridf,all.x=TRUE) %>% # merge with raster data frame
        mutate(presabs=ifelse(is.na(presabs),0,presabs)) %>%
        dplyr::select(x,y,presabs)  %$%
        rasterFromXYZ(.) ->allr # create final raster
        filen<-paste(binomial,"_fullrange.tif",sep="") # filename
        fullpathfilen<-paste(outpath,"/",filen,sep="") # filename + path
        writeRaster(allr,file=fullpathfilen,overwrite=TRUE) # write raster
        lookup1<-data.frame(binomial,filename=filen) # data frame with file info
        #results<-rbind(lookup1,results)  # store data frame
        if(season=="TRUE"){
            # sesonal list (breeding,non-breeding,passage)
            seas.l<-list(c(1,2,5),c(1,3),c(4))
            names(seas.l)<-c("breeding","nonbreeding","passage")
            # season data frame
            for (z in 1:length(seas.l)){
                # subset breeding range
                tmpshp.b<-tmpshp3[tmpshp3$SEASONAL %in% seas.l[[z]],]
                if(length(tmpshp.b$SEASONAL)==0){
                    next
                } else {
                    cats<-raster::extract(rastg,tmpshp.b,small=T) # extract categories
                    catsdf<-data.frame(value=unlist(cats),presabs=1) # create data frame with presabs
                    alldf<-merge(gridf,catsdf,all.x=TRUE) # merge with raster data frame
                    alldf$presabs<-replace(alldf$presabs,is.na(alldf$presabs),0) # insert 0s
                    allr<-rasterFromXYZ(alldf[,c("x","y","presabs")]) # create final raster
                    filen<-paste(binomial,"_",names(seas.l[z]),".tif",sep="") # filename
                    fullpathfilen<-paste(outpath,"/",filen,sep="") # filename + path
                    writeRaster(allr,file=fullpathfilen) # write raster
                    lookup1<-data.frame(binomial,filename=filen) # data frame with file info
                    #results<-rbind(lookup1,results)  # store data frame
                }
            }
        }
    }
    #return(results)
}
