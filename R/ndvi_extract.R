

ndvi_extract<-function(indf,inproj,outproj,coln,mapname,bufferad){
    results<-NULL
    sitem<-indf[,coln]
    coordinates(sitem)=c(names(indf[,coln])) # convert coordinates into spatial points data frame
    proj4string(sitem)<-inproj # insert projection
    coor1<-spTransform(sitem,CRS(outproj)) # reproject
    tmp<-data.frame(sitem) # recomposes data frame original projection
    colnames(tmp)<-c("Xor","Yor")
    tmp1<-data.frame(coor1) # recomposes data frame reprojected coordinates
    coorprojected<-cbind(tmp,tmp1)
    for (i in 1:dim(coorprojected)[1]){ # start looping through sites and maps
        cat(paste("site",i)) 
            cat("\n")
        # adjust region for creating buffer
        reg.adj<-paste("g.region ","n=",coorprojected[i,4]+(bufferad+20000) ," s=",coorprojected[i,4]-(bufferad+20000)," e=",coorprojected[i,3]+(bufferad+20000)," w=",coorprojected[i,3]-(bufferad+20000),sep="")
        system(command=reg.adj) 
       circle<-paste("r.circle --quiet coor=",coorprojected[i,3],",",coorprojected[i,4]," out=test"," min=0 ","max=",bufferad," --o",sep="")
system(command=circle) 
        # zoom onto buffer
        system("g.region rast=test zoom=test")
        # set all pixels within buffer=1
        system("r.mapcalc 'test1 = test/test' --o")
        system("r.mask raster=test1 maskcats=1 --qq")
        for (x in 1:length(mapname)){ # loop through each NDVI map
            stats<-paste("r.stats -lAn",mapname[x],"> tmp --o --quiet")  # extract statistics
            system(command=stats)    
            tmp<-try(read.table("tmp")) # if data frame empty skip
            if(class(tmp)=="try-error"){
                next    
            } else {
                dfres<-data.frame(mean=mean(tmp$V1),map=mapname[x],coorprojected[i,])
                results<-rbind(dfres,results)
            }
        }
        system("r.mask -r --qq")
        system("g.region -d")
    }
    return(results)
}
