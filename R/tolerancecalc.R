#' Calculate tolerance to humans using Birdlife habitat species
#' Input is a data frame containing habitat preferences 
#' @indf = input data frame
#' @sp = column containing species names 
#' @level1 = column containing information on level1 habitat categories
#' 
tolerancecalc<-function(sp,level1,indf){
    results<-NULL
    sp.l<-as.character(unique(indf[,sp]))
    for (i in 1:length(sp.l)){
        tmp<-indf[indf[,sp] %in% sp.l[i],]
        tmp2<-subset(tmp,Level1=="Artificial/Terrestrial")
        if(dim(tmp2)[1]==0){
        df<-data.frame(Scientific.name=sp.l[i],tolerance=c("lowtolerance"))
        results<-rbind(df,results)
        } else {
            #tolvalue<-dim(tmp2)[1]/dim(tmp)[1]
            tolvalue<-dim(tmp2)[1]/6
            if(tolvalue < 0.33){
                df<-data.frame(Scientific.name=sp.l[i],tolerance=c("lowtolerance"))
                results<-rbind(df,results)
            }
            if(tolvalue > 0.33 &  tolvalue < 0.66){
                df<-data.frame(Scientific.name=sp.l[i],tolerance=c("mediumtolerance"))
                results<-rbind(df,results)    
            }
            if(tolvalue > 0.66){
                df<-data.frame(Scientific.name=sp.l[i],tolerance=c("hightolerance"))
                results<-rbind(df,results)    
            }
        } 
    }
    return(results)
}