#' Fit gbm models using a loop give a set of responses
#'
#' @resp = list of responses
#' @pred= list of predictors
#' @modsave = path of folder where to save models
#' @indat = input dataset
#' 

gbmwrap<-function(resp,pred,modsave=NULL,indat,family){
    results<-foreach (x=1:length(resp),.errorhandling=c('remove'),.combine="rbind")%dopar% {
        mod<-gbm.step(data=indat,gbm.x=match(pred,names(indat)),gbm.y=match(resp[x],
        names (indat)),family=family,tree.complexity = 3,learning.rate=0.001,bag.fraction =0.5,step.size=50)
        filep<-paste(modsave,"/",resp[x],sep="")
        save(mod,file=filep)
        df<-data.frame(summary(mod,plotit=FALSE),variable=resp[x])
    }
    return(results)
}


