#'Function for species occurence map pdf by overlaying it onto grid
#' 
#' @indat = data frame with input data
#' @x = name of column containin x coordinate values
#' @iy = name of column containin y coordinate values
#' @sp = name of column containing species names
#' @map = name map (vector) to use a background map
#'

mappdf<-function(indat,x,y,sp,map){
    pdf("speciesmap.pdf")
    sp.l<-unique(indat[,sp])
    for (i in 1:length(sp.l)){
        sptmp<-indat[indat[,sp] %in% sp.l[i],]
        title<-as.character(unique(sptmp[,sp]))
        coordinates(sptmp)<-~x+y
        plot(map,main=title)
        plot(sptmp,col="red",cex=0.8,add=T)
    }
    dev.off()
}