#' Fit rf models using a loop give a set of responses
#'
#' @resp = list of responses
#' @pred= list of predictors
#' @modsave = path of folder where to save models
#' @indat = input dataset
#' 

rfwrap1<-function(resp,pred,modsave,indat,class){
    results<-NULL
    if(class==TRUE){
        for (x in 1:length(resp)){
        rs<-paste(pred,collapse="+")
        form<-as.formula(paste(resp[x],"~",rs))
        indat[,resp[x]]<-as.factor(indat[,resp[x]])
        mod<-randomForest(form,data=indat,importance=TRUE)
        filep<-paste(modsave,"/",resp[x],sep="")
        save(mod,file=filep)
        df<-data.frame(predictor=row.names(mod$importance),mod$importance,variable=resp[x])
        row.names(df)<-1:dim(df)[1]
        results<-rbind(df,results)
    }
    } else {
        for (x in 1:length(resp)){
            rs<-paste(pred,collapse="+")
            form<-as.formula(paste(resp[x],"~",rs))
            mod<-randomForest(form,data=indat,importance=TRUE)
            filep<-paste(modsave,"/",resp[x],sep="")
            save(mod,file=filep)
            df<-data.frame(predictor=row.names(mod$importance),mod$importance,variable=resp[x])
            row.names(df)<-1:dim(df)[1]
            results<-rbind(df,results)
        }
    }
return(results)
}



        
