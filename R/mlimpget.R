#' Load results from rf brt/models from saved objects
#'
#' @path = path of folder where models are stored
#' @modtype = character "rf" or "brt"
#' 


mlimpget<-function(path,modtype){
    if(modtype=="rf"){
        file.l<-list.files(path,full.names=T)
        results<-NULL
        for (i in 1:length(file.l)){
            load(file.l[i])
            lab<-stri_split_fixed(file.l[i],"/",simplify=TRUE)
            lab<-lab[,dim(lab)[2]]
            df<-data.frame(predictor=row.names(mod$importance),mod$importance,variable=lab)
            row.names(df)<-1:dim(df)[1]
            results<-rbind(df,results)
        }
        res1<-spread(results[,c("predictor","MeanDecreaseAccuracy","variable")],variable,MeanDecreaseAccuracy)
        res1<-spread(results[,c("predictor","X.IncMSE","variable")],variable,X.IncMSE)
    }
    if(modtype=="brt"){    
        file.l<-list.files(path,full.names=T)
        results<-NULL
        for (i in 1:length(file.l)){
            load(file.l[i])
            lab<-stri_split_fixed(file.l[i],"/",simplify=TRUE)
            lab<-lab[,dim(lab)[2]]
            df<-data.frame(summary(mod,plotit=FALSE),variable=lab)
            row.names(df)<-1:dim(df)[1]
            results<-rbind(df,results)
        }  
        res1<-spread(results,variable,rel.inf)
        
    }
    return(res1)
}