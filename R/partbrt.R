#'
#'  Function for creating dataframe for partial dependency plots for BRT models
#'  inmod@= list of models stored in a directory
#'

partbrt<-function(inmod,nrep){
  mod.l=inmod
  results<-NULL
  for (x in 1:length(mod.l)){
    print(mod.l[x])
    # load model
    load(mod.l[x])
    # extract dataframe
    dat<-mod$gbm.call$dataframe
    # list of predictors
    pred.l<-mod$var.names
    names(pred.l)<-pred.l
    for (i in 1:length(pred.l)){
      print(pred.l[i])
      # sequence with predictor of interest
      pi<-seq(from=min(dat[,pred.l[i]],na.rm=TRUE),to=max(dat[,pred.l[i]],na.rm=TRUE),length.out = 1000)
      df<-data.frame(pi)
      colnames(df)<-pred.l[i]
      # mean value for all other predictors
      op<-dat[,pred.l[pred.l!=pred.l[i]]]
      meandf<-sapply(op,mean,na.rm=TRUE)
      meandf1<-mefa:::rep.data.frame(meandf,nrep)
      # bind two dataframes
      predictdf<-data.frame(df,meandf1)
      # issue predictions
      resdf<-data.frame(fitted=predict(mod,predictdf,n.trees=mod$n.trees,type="response"))
      resdf1<-data.frame(resdf,predictor=pi,varname=as.character(pred.l[i]),model=mod$gbm.call$response.name)
      results<-rbind(resdf1,results)
    }
  }
  return(results)        
}