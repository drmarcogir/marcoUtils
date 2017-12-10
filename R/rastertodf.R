#' Convert objects from the raster package into a dataframe 
#'
#' @inrast = name of raster map to be converted into a data frame
#'
rastertodf<-function(inrast){
    tmpdf<-na.exclude(data.frame(coordinates(inrast),value=getValues(inrast)))
    return(tmpdf)
}


