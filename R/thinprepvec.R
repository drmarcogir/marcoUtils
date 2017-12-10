#' Prepare data for thinning. Filter records on the basis of the range of the species
#'
#' @ indat = input dataframe
#' @ infile= path to shapefiles used for the filtering
#'
#'


thinprepvec<-function(indat,infile){
    res<-NULL
    for (i in 1:dim(infile)[1]){
        # shapefile
        filel<-paste("/media/marco/birdsproject/Birdlifeshapefilesuncompressed/",
                     infile[i,][,1],sep="")
        shape<-readShapePoly(filel)
        tmpshp2<-try(subset(shape,PRESENCE < 3))
        tmpshp3<-try(subset(tmpshp2,ORIGIN < 3))
        proj4string(tmpshp3)<-latlon
        # occurence records
        sp<-as.character(infile[i,][,2])
        tmp<-subset(indat,species==sp)
        coordinates(tmp)<-~lon+lat
        proj4string(tmp)<-latlon
        filtered<- !is.na(over(tmp, as(tmpshp3, "SpatialPolygons")))
        tmp<-tmp[filtered,]
        tmp<-as.data.frame(tmp)
        # bind results
        res<-rbind(tmp,res)
    }
    return(res)
}