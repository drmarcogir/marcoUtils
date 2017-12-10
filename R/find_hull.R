#'
#' Find convex hull for a number of levels  across a data frame 
#' @dat= input dataframe
#' @col1 = column containing values for x axis
#' @col2 = column containing values for y axis
#' @groupcol = column grouping it 
#' 

find_hull <- function(dat,col1,col2,groupcol){
    list.v<-as.character(na.exclude(unique(dat[,groupcol])))
    results<-NULL
    for (i in 1:length(list.v)){
        tmp<-dat[dat[,groupcol] %in% list.v[i],]
        tmp1<-tmp[chull(tmp[,col1], tmp[,col2]),]
        results<-rbind(tmp1,results)
    }
    return(results)
}