
#' Fit Log Gaussian Cox Point Process models to a series of species
#' 
#' @insp = name of input dataframe containing species data
#' @inenv = name of input dataframe containing environmental data
#' @spcol = character vector providing the name of the column containing species binomial name/id etc.
#' @predl = character vector containing list of environmental predictors to be used for the model
#'

fitszn<-function(insp,inenv,spcol,predl,cutoff=NULL,form,max.edge){
  if(is.null(cutoff)){
    cutoff=0.5
  }
  sp.l<-as.character(unique(insp[,spcol])) 
  results<-NULL
  for (i in 1:length(sp.l)){
   ##############################
   # Data preparation
   ##############################
   # subset data for a given species 
   sptmp<-insp[insp[,spcol] %in% sp.l[i],] 
   env<-inenv[inenv[,spcol] %in% sp.l[i],] 
   # create mesh
   mesh1<-inla.mesh.2d(as.matrix(env[c("x","y")]),max.edge=max.edge,cutoff=cutoff)
   # domain size to be used as priors
   # create matern correlation
   spde = inla.spde2.matern(mesh1,alpha=2)
   # cross points with the mesh
   sp.mat = inla.spde.make.A(mesh1, as.matrix(sptmp[c("x_sp","y_sp")]))
   env1<-env[!env$cat %in% unique(sptmp$cat),]
   set.seed(123)
   quad.df<-env1[sample(1:dim(env1)[1],2000),]
   quad.mat = inla.spde.make.A(mesh1, as.matrix(quad.df[c("x","y")]))
   pred.mat = inla.spde.make.A(mesh1, as.matrix(env[c("x","y")]))
   # spenv
   spenv.st<-sptmp[,c("id",predl)]
   # env
   env.st<-env[,c(predl)]
   quad.st<-quad.df[,c(predl)]
   # generate stacks
   stk.sp = inla.stack(data = list(Intensity = 1, e = 0), A = list(sp.mat, 1),tag="sp",
   effects = list(list(i = 1:mesh1$n),data.frame(Intercept = rep(1,dim(spenv.st)[1]),spenv.st)))
   stk.quad = inla.stack(data = list(Intensity=0,e=1), A = list(quad.mat, 1),tag="quad",
   effects = list(list(i = 1:mesh1$n),data.frame(Intercept = rep(1,dim(quad.df)[1]), quad.st)))
   stk.pred = inla.stack(data = list(Intensity=NA), A = list(pred.mat, 1),tag="pred",
   effects = list(list(i = 1:mesh1$n),data.frame(Intercept = rep(1,dim(env.st)[1]),env.st)))
   stk.all = inla.stack(stk.sp, stk.quad,stk.pred)
   ##############################
   # Model fitting
   ##############################
   form1<-as.formula(form)
   mod<-inla(form1,family = "poisson",data = inla.stack.data(stk.all),control.predictor = list(A = inla.stack.A(stk.all),
   compute = TRUE,link=1),E = inla.stack.data(stk.all)$e,control.compute = list(dic = TRUE,cpo=TRUE,config = TRUE),
   control.fixed = list(expand.factor.strategy = "inla"))
   filen<-paste("mod_sp",i,sep="")
   save(mod,file=filen)
   ##############################
   # Post-processing
   #############################
   I0 = inla.stack.index(stk.all, "pred")
   E0 = mod$summary.fitted.values[I0$data,]
   suit<-data.frame(env[,c("x","y")],m=E0,species=sp.l[i])
   results<-rbind(suit,results)
  }
  return(results)
}

