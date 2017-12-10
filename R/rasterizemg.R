
proj4string(world)<-latlon
world1<-spTransform(world,CRS(ml))
inshape=world
outres=0.299
inproj=latlon


rasterizemg<-function(inshape,outres,inproj){
r <- raster(extent(inshape))
proj4string(inshape)<-CRS(inproj)
projection(r) <- proj4string(inshape)
res(r)=outres
r1<- rasterize(inshape,field=1,r)
r2<-rastertodf(r1)
r2$value<-1:dim(r2)[1]
r3<-rasterFromXYZ(r2)
return(r3)
}





