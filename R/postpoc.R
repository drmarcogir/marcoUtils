#' Post-processing of results for
#' Gerardo
#'
#' @inlist = list containing results
#' @inlist = path of the folder where figures will be stored
#'

postproc<-function(inlist,pathfig){
  # plot of response curves (first bars!
  p2<-ggplot(data=inlist[[3]],aes(x=value,ymin=predicted))+
    geom_crossbar(data=inlist[[3]],aes(x=value,ymin=predicted, ymax=predicted,y=predicted,col=es))+
    geom_line(data=inlist[[4]],aes(x=values,y=predicted,col=es))+theme_bw()+facet_wrap(~predrespcurve,scale="free")+
    xlab("Value of predictor")+ylab("Predicted ES value")
  # file path
  fpath<-paste(pathfig,"/","rcurves.png",sep="")
  # save figure
  ggsave(p2,filename=fpath,dpi=400,width=9,height=7,device="png")
  # plot of posterior distributions
  # create plot
  post<-ggplot(data=inlist[[2]],aes(x=x,y=y))+geom_line()+theme_bw()+facet_grid(response~predictor,scale="free")+
    xlab("Betas (coefficient values)")+ylab("Probability (Betas | Data)")
  # file path
  fpath<-paste(pathfig,"/","postd.png",sep="")
  # save figure
  ggsave(post,filename=fpath,dpi=400,width=9,height=7,device="png")
  # coefficients
  df<-coefstable(incoef=inlist[[1]])
  return(df)
}
