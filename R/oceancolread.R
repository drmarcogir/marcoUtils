#' Read in temperature data from ocean color dataset and convert them into a dataframe
#'
#' @filelist = character vector containing the list of files to be read and converted into data frames
#'
oceancolread<-function(filelist){
    for (i in 1:length(filelist)){
        nc <- nc_open(filelist[i])
        total<-length(filelist)
        cat(paste("file",i,"out of",total,"files"))
        cat("\n")
        sst <- ncvar_get(nc, varid = 'geophysical_data/sstref')
        lon <- ncvar_get(nc, varid = 'navigation_data/longitude')
        lat <- ncvar_get(nc, varid = 'navigation_data/latitude')
        nc_close(nc)
        p <- data.frame(lon=c(lon), lat=c(lat), sst=c(sst))
        coordinates(p) <- ~lon+lat
        proj4string(p) <- '+init=epsg:4326'
        r <- rasterize(p, raster(extent(p) * 1.04,res=0.05), 'sst', fun=mean)
        mydf<-rastertodf(r)
        return(mydf)
    }
}
