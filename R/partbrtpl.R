#'
#'  Function for partial dependency plots from BRT models
#'  indf@= brt dataframe with predictions
#'  filen@=name of file type of plot
#'

partbrtpl<-function(indf,model,x,y,varname,filen){
    mod.l<-unique(indf[,model])
  for (i in 1:length(mod.l)){
    # title for response
     tmp<-indf[indf[,model] %in% mod.l[i],]
    f<-substr(as.character(unique(tmp[,model])), start=1, stop=1)
    l<-substr(as.character(unique(tmp[,model])),start=nchar(as.character(unique(tmp[,model]))),
              stop=nchar(as.character(unique(tmp[,model]))))
    filen1<-paste(f,filen,l,".JPG",sep="")
    dynylab<-paste(f,filen,l," richness",sep="")
    
    # create plot
    p<-ggplot(data=tmp,aes_string(x=x,y=y))+geom_line()+
      theme_bw()+ylab(dynylab)+xlab("Predictor")+facet_wrap(~varname,scale="free")+
      theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))
    ggsave(p,file=filen1,width=8,height=7)
  }
}