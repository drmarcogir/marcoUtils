standat<-function(indatc,pred,logt){
# log and standardize continous predictors
if(logt==TRUE){
    for (x in 1:length(pred)){
        if(class(indatc[,pred[x]])=="factor"){
            indatc$tmp<-indatc[,pred[x]]    
            colnames(indatc)[dim(indatc)[2]]<-paste(pred[x],"s",sep="")    
        } else {
            indatc$tmp<-scale(sqrt(indatc[,pred[x]]))
            colnames(indatc)[dim(indatc)[2]]<-paste(pred[x],"s",sep="")
        }    
    } 
} else {
    for (x in 1:length(pred)){
        if(class(indatc[,pred[x]])=="factor"){
            indatc$tmp<-indatc[,pred[x]]    
            colnames(indatc)[dim(indatc)[2]]<-paste(pred[x],"s",sep="")    
        } else {
            indatc$tmp<-scale(indatc[,pred[x]]+1)
            colnames(indatc)[dim(indatc)[2]]<-paste(pred[x],"s",sep="")
        }    
    } 
}
return(indatc)
}