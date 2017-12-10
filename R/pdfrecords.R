#' Map records from species distributions using birdlife files
#'
#' @ infile = list of shapefiles
#' @ outfile = output pdf files
#' @ indat = input dataframe containing species records
#'


pdfrecords<-function(outfile,infile,indat){
    pdf(outfile)    
    for (i in 1:dim(infile)[1]){
        # shapefile
        filel<-paste("/media/marco/birdsproject/Birdlifeshapefilesuncompressed/",
                     infile[i,][,1],sep="")
        shape<-readShapePoly(filel)
        tmpshp2<-try(subset(shape,PRESENCE < 3))
        tmpshp3<-try(subset(tmpshp2,ORIGIN < 3))    
        # occurence records
        sp<-as.character(infile[i,][,2])
        tmp<-subset(indat,species==sp)
        # plot
        plot(tmpshp3,border="red")
        plot(world,add=T)
        points(tmp$lon,tmp$lat,pch="+",col="blue")
        title(infile[i,][,2])
    }
    dev.off()    
}

