
stackcreate<-function(insp,inenv,spcol,predl,cutoff,max.edge){
  if(is.null(cutoff)){
    cutoff=0.5
  }
  sp.l<-as.character(unique(insp[,spcol])) 
  results<-vector("list",length(sp.l))
  names(results)<-sp.l
  for (i in 1:length(sp.l)){
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
    results[[i]]<-stk.all
  }
  return(results)
}  
