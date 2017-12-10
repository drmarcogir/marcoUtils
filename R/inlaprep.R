inlaprep<-function(indat,pred,ref=NULL,resp,after=NULL,logt){
    if(after==TRUE){ # assemble first standardize later
        # generate values
        newdat<-evprep(inpred=pred,indat=indat,ref=ref)
        newdat1<-evprep1(inlist=newdat,resp=resp)
        write.csv(newdat1,file="newdat1.csv",row.names=FALSE)
        newdat1<-read.csv("newdat1.csv")
        # bind values to main dataframe
        indat$predrespcurve<-NA
        indat1<-indat[,names(newdat1)]
        indat2<-rbind(indat1,newdat1)
        # standardize variables
        retdf<-standat(indatc=indat2,pred=pred,logt=logt)
    }
        if(after==FALSE){  # standardize first assemble later
        # standardize variables
        indats<-standat(indatc=indat,pred=pred,logt=logt)
        # generate values
        preds<-paste(pred,"s",sep="")
        newdat<-evprep(inpred=preds,indat=indats,ref=ref)
        newdat1<-evprep1(inlist=newdat,resp=resp)
        write.csv(newdat1,file="newdat1.csv",row.names=FALSE)
        newdat1<-read.csv("newdat1.csv")
        # bind values to main dataframe
        indats$predrespcurve<-NA
        indats1<-indats[,names(newdat1)]
        retdf<-rbind(indats1,newdat1)
    }
    return(retdf)
}
    