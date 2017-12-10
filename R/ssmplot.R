#'
#'  Function creating plots from state-space model output
#'  inmod@= paths to model objects
#'

ssmplot<- function(inmod){
    # list where to store results
    results<-vector("list")
    for (i in 1:length(inmod)){
        # boundaries of study area
        library(rworldxtra)
        data(countriesHigh)
        wm <- suppressMessages(fortify(countriesHigh))
        # load model object
        inmod1<-get(load(inmod[i]))
        # if state-space model (not hierarchical!)
        if(class(inmod1)=="ssm"){
            # extends the range of the data
            xl <- extendrange(inmod1[[1]]$data$lon, f = 0.1)
            yl <- extendrange(inmod1[[1]]$data$lat, f = 0.1)
            # create plot
            p <- ggplot() + geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),fill="grey",
                                         colour="black",size=0.2) +coord_cartesian(xlim = xl, ylim = yl) + xlab("Longitude") + ylab("Latitude")+
                theme_bw()+geom_point(data =inmod1[[1]]$data,aes(x=lon,y=lat),colour=grey(0.7),pch="+",size=4)+
                geom_point(data =inmod1[[1]]$summary,aes(x=lon,y=lat,group = NULL,colour =b),size = 1.25)+
                scale_colour_gradient2(midpoint = 1.5, low = "blue", mid = "white", high = "red")
            # add title
            infotmp<-data.frame(id=names(inmod1))
            infotmp1<-as.character(merge(infotmp,turtleinfo)$original_id)
            # add title to the plot
            p1<-p+labs(title=infotmp1)
            results<-append(results,p1)
        } else {
            turtle.l<-unique(inmod1$data$id)
            for (y in 1:length(turtle.l)){
                # subset data for a given turtle (observed data)
                turtletmp<-subset(inmod1$data,id==turtle.l[y])
                # subset data for a given turtle (modelled data)
                turtletmp1<-subset(inmod1$summary,id==turtle.l[y])
                # extends the range of the data
                xl <- extendrange(turtletmp$lon, f = 0.1)
                yl <- extendrange(turtletmp$lat, f = 0.1)
                p <- ggplot() + geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),fill="grey",
                                             colour="black",size=0.2) +coord_cartesian(xlim = xl, ylim = yl) + xlab("Longitude") + ylab("Latitude")+
                    theme_bw()+geom_point(data =turtletmp,aes(x=lon,y=lat),colour=grey(0.7),pch="+",size=4)+
                    geom_point(data = turtletmp1,aes(x=lon,y=lat,group = NULL,colour =b),size = 1.25)+
                    scale_colour_gradient2(midpoint = 1.5, low = "blue", mid = "white", high = "red")
                # add title
                infotmp<-data.frame(id=turtle.l[y])
                infotmp1<-as.character(merge(infotmp,turtleinfo)$original_id)
                # add title to the plot
                p1<-p+labs(title=infotmp1)
                results<-append(results,p1)
            }
        }
    }
    return(results)
}