#' Calculate tolerance to humans using Birdlife habitat species
#' Input is a data frame containing habitat preferences 
#' @indf = input data frame
#' @sp = column containing species names 
#' @imp = column containing information on level1 habitat categories
#' 

gbifconvert<-function(sp,level1,indf){
    results<-NULL
    esa.l<-unique(gbifrules4$ESA)
    for (x in 1:length(esa.l)){
    df<-subset(gbifrules4,ESA==esa.l[x]) # subset esa
    nas<-subset(df,is.na(value))
    nas$score<-1
    df1<-subset(df,!is.na(value))
    df1$score<-ntile(df1$value,3)
    results<-rbind(df1,results)
    results<-rbind(nas,results)
    }

    sp.l<-as.character(unique(indf[,sp]))
    for (i in 1:length(sp.l)){
        tmp<-indf[indf[,sp] %in% sp.l[i],]
        tmp2<-subset(tmp,Level1=="Artificial/Terrestrial")
        if(dim(tmp2)[1]==0){
            df<-data.frame(Scientific.name=sp.l[i],tolerance=c("lowtolerance"))
            results<-rbind(df,results)
        } else {
            tolvalue<-dim(tmp2)[1]/dim(tmp)[1]
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