#'Function for creating grid from shapefile
#' 
#' @pathshape = path of the directory where the shapefile is stored
#' @shapename = name of shapefile
#' @inproj= projection of original shapefile
#' @outproj= output project
#' @outres= output resolution 
#'
  
gridcreatetotal<-function(pathshape,shapename,inproj,outproj,outres){
  inproj1<-CRS(inproj)
  inproj2<-CRS(outproj)
  # read in shapefile
  shapetmp <- readOGR(pathshape,shapename)
  if(is.na(proj4string(shapetmp))){
      proj4string(shapetmp)<-inproj1
  }
  shapetmp1<-spTransform(shapetmp,inproj2)
  # create raster from extent
  r <- raster(extent(shapetmp1))
  projection(r) <- proj4string(shapetmp1)
  res(r)=outres
  # rasterize
  r1<- rasterize(shapetmp1, field=names(shapetmp1), r)
  r1[is.na(r1)]<-1
  r2<-na.exclude(data.frame(coordinates(r1),cat=getValues(r1)))
  r2$cat<-1:dim(r2)[1]
  finalr<-rasterFromXYZ(r2)
  finalrdf<-rastertodf(finalr)
  cats<-unlist(raster:::extract(finalr,shapetmp1,small=TRUE))
  catdf<-data.frame(value=cats)
  finalrdf1<-merge(finalrdf,catdf)
  finalr1<-rasterFromXYZ(finalrdf1[,c("x","y","value")])
  return(finalr1)
  }

  