evprep1<-function(inlist,resp){
    inlist1<-inlist[lapply(inlist,length)>0]
    # create new prediction data
    newdat2<-NULL
    for (y in 1:length(inlist1)){
        tmpdf<-data.frame(inlist1[[y]],predrespcurve=names(inlist1[y]))
        newdat2<-rbind(tmpdf,newdat2)
    }
    # add response variables
    respm<-matrix(ncol=length(resp),nrow=dim(newdat2)[1])
    colnames(respm)<-resp
    newdat3<-data.frame(newdat2,respm)
    return(newdat3)
}