standat1<-function(indatc,pred){
  for (x in 1:length(pred)){
    if(class(indatc[,pred[x]])=="factor"){
      indatc[,pred[x]]<-indatc[,pred[x]]    
    } else {
      indatc[,pred[x]]<-as.numeric(scale(indatc[,pred[x]]))
    }    
  } 
  return(indatc)
}
