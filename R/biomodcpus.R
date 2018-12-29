biomodcpus<-function(inenv,indat){
    #length(myResp)    
    # modelling options 
    myBiomodOption <- BIOMOD_ModelingOptions(
        MAXENT.Phillips= list(path_to_maxent.jar = "/mnt/data1tb/Dropbox/maxentjarfile/maxent.jar",
                              maximumiterations = 200,
                              visible = FALSE,
                              linear = TRUE,
                              quadratic = TRUE,
                              product = TRUE,
                              threshold = TRUE,
                              hinge = TRUE,
                              lq2lqptthreshold = 80,
                              l2lqthreshold = 10,
                              hingethreshold = 15,
                              beta_threshold = -1,
                              beta_categorical = -1,
                              beta_lqp = -1,
                              beta_hinge = -1,
                              defaultprevalence = 0.5,
                              betamultiplier=2.0))
    
    
    #  compute models 
    myBiomodModelOut <- BIOMOD_Modeling(
        indat,
        models = c("GBM","MAXENT.Phillips","RF","GLM"),
        models.options = myBiomodOption,
        NbRunEval=1,
        DataSplit=80,
        Prevalence=0.5,
        VarImport=0,
        models.eval.meth = c('TSS','ROC'),
        SaveObj = TRUE,
        rescal.all.models = TRUE,
        do.full.models = FALSE,
        modeling.id ="vulture")
    
    # create projections for invaded ranges
    predsuit<- BIOMOD_Projection(
        modeling.output = myBiomodModelOut,
        new.env = inenv,
        proj.name = 'current',
        selected.models = 'all',
        binary.meth = 'TSS',
        compress = 'xz',
        clamping.mask = F,
        output.format = '.grd')
    

    # create model ensemble
    myBiomodEM <- try(BIOMOD_EnsembleModeling(
        modeling.output = myBiomodModelOut,
        chosen.models = 'all',
        em.by='all',
        eval.metric = c('TSS'),
        eval.metric.quality.threshold = c(0.5),
        prob.mean = T,
        prob.cv = F,
        prob.ci = F,
        prob.ci.alpha = 0.05,
        prob.median = T,
        committee.averaging = T,
        prob.mean.weight = T,
        prob.mean.weight.decay = 'proportional' ))
    
    if(class(myBiomodEM)=="try-error"){
        print("NO ENSEMBLE!")
    } else {
        # create consensus forecast
        myBiomodEF <- BIOMOD_EnsembleForecasting(
            EM.output = myBiomodEM,
            projection.output=predsuit,binary.meth="TSS")
        
    }
}
