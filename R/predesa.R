#' Load gbm models and create predictions
#'
#' @path = path where models are stored
#' @newdat = new data needed for prediction 
#' 


predesa<-function(path,newdat){
    mod.l<-list.files(path,full.names = TRUE) # list of models
    results<-NULL
    for (i in 1:length(mod.l)){
        load(mod.l[i]) # load model
        preval<-predict(mod,type="response",newdata=newdat,n.trees=as.numeric(gbm.perf(mod)[1])) # predicted value
        esacat<-stri_split_fixed(mod.l[i],"/",simplify=TRUE)[,7]
        dfres<-data.frame(esa=esacat,preval)
        results<-rbind(dfres,results)
    }
    return(results)
}