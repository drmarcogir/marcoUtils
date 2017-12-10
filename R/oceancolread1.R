#' Read in temperature data from ocean color dataset and convert 
#' them into a dataframe
#'
#' @filelist = character vector containing the list of files to be read and converted into data frames
#' @extent = extend of study area
#' @=cropf = shapefile to filter land masses
#' 

oceancolread1<-function(filelist,fextent,cropf){
    results<-NULL
    total<-length(filelist)
    fextent1<-extent(fextent)
    for (i in 1:total){
        cat(paste("file",i,"out of",total,"files"))
        cat("\n")
        tmpr<-raster(filelist[i],varname="sst")
        tmpr1<- crop(tmpr, fextent1)
        # exclude terceira
        mask1<-mask(tmpr1,cropf) # create mask
        ter_r<-rastertodf(mask1) # crop raster for terceira
        temp2<-rastertodf(tmpr1)
        temp3<-suppressMessages(anti_join(temp2,ter_r))
        meanvalue<-mean(temp3$value)
        res.df<-data.frame(meanT=meanvalue,filename=filelist[i])
        results<-rbind(res.df,results)
    }
    return(results)
}