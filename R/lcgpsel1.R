#'  Function for computing all possible model combinations using INLA
#' 
#'  
#'  @ predl= character vector containing the name of the predictors  
#'  @ insp= name of dataframe containing species data (together with environmental data) 
#'  @ inenv = name of dataframe containing environmental data for the study area
#'  @ cutoff = cutoff value for mesh
#'  @ max.edge = max.edge values for mesh
#'

lgcpsel1<-function(predl,insp,inenv,cutoff,max.edge,combos,from=1){
  # if all combinations, otherwise set by input number
    totlength=length(predl)
  ###################
  # Data preparation
  ###################
    #---- create mesh
  Bound<-inla.nonconvex.hull(as.matrix(insp[c("x_sp","y_sp")]))
  mesh1<-inla.mesh.2d(boundary=Bound,max.edge=max.edge,cutoff=cutoff)
  
  #---- create matern correlation
  spde = inla.spde2.matern(mesh1,alpha=2)
  #----- define weight factors
  sp.mat = inla.spde.make.A(mesh1, as.matrix(insp[c("x_sp","y_sp")]))

  pred.mat = inla.spde.make.A(mesh1, as.matrix(inenv[c("x","y")]))
  #--- dataframe with predictors (spenv predictors + species id)
  spenv.st<-insp[,c("id",predl)]
  env.st<-inenv[,c(predl)]

  #--- define the spatial field
  w.index<-inla.spde.make.index(name="w",n.spde=spde$n.spde,n.group=1,n.repl=1)
 
  #--- generate stacks
  
  stk.sp = inla.stack(data = list(Intensity = insp$pa, e = insp$expsp), A = list(1,1,sp.mat),tag="sp",
  effects = list(Intercept = rep(1,dim(spenv.st)[1]),spenv.st,w=w.index))
    
  stk.pred= inla.stack(data = list(Intensity=NA), A = list(1,1,pred.mat),tag="pred",
  effects = list(Intercept = rep(1,dim(env.st)[1]),env.st,w=w.index))
  
  stk.all = inla.stack(stk.sp,stk.pred)

  #################
  # Model fitting
  #################
  # results<-NULL
#  for (i in from:combos){
#    # create model combinations
#    tmp <- combinations(totlength, i, predl) 
#    for (z in 1:dim(tmp)[1]){
#      # predictors
#      predl.tmp<-paste(tmp[z,],collapse="+")
      form<-as.formula(paste("Intensity~-1+slas+chlors+ssts+depths+slopes+f(w, model = spde)",sep=""))
      # create formula
#      form<-as.formula(paste("Intensity~-1+",predl.tmp,"+f(w, model = spde)",sep=""))
      # fit model
      mod<-inla(form,family = "poisson",data = inla.stack.data(stk.all),control.predictor = list(A  = inla.stack.A(stk.all),compute = TRUE,link=1),control.compute = list(dic = TRUE),E =inla.stack.data(stk.all)$e)

      # save model file
#     filen<-paste(tmp[z,],collapse="_")
      filen="test"
      save(mod,file=filen)
      # store results
#      tmpresults<-data.frame(model=filen,variable=row.names(mod$summary.fixed),coef=mod$summary.fixed[,1],dic=mod$dic$dic)
#      results<-rbind(tmpresults,results)
#    }
#  }
  
#  return(results)
}

