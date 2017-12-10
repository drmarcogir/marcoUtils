#' Fit models using INLA get response curves, posteriors and mean of posteriors
#'
#' @indat = dataframe containing input data
#' @resp = character vector containing list of responses 
#' @pred = character vector containing list of predictors 
#' @ref = character vector indicating random effect
#' @family = distribution of the response
#' 
#' 

inlawrap<-function(indat,resp,pred,ref,family){
    coefs<-NULL
    posteriordis<-NULL
    responsecurves<-NULL
    for (i in 1:length(resp)){
        # fit INLA models 
        form<-paste(pred,collapse="+")
        form1<-as.formula(paste(resp[i],"~",form,"+f(",ref,",",'model="iid")',sep=""))
        mod<-inla(form1,family=family,data=indat,control.predictor=list(link=1,compute=TRUE,
        quantiles=c(0.025,0.975)))
        # save model object
        filepn<-paste(resp[i]) 
        save(mod,file=filepn)
        # get posterior distributions
        post<-mod$marginals.fixed
        # stacked on top of each other
        postres<-NULL
        for(x in 1:length(post)){
            tmpdf<-data.frame(post[[x]],predictor=names(post)[x])
            postres<-rbind(tmpdf,postres)
        }
        # insert response name
        postres$response<-resp[i]
        posteriordis<-rbind(postres,posteriordis) # store results
        # summary of posterior distributions
        sumpost<-mod$summary.fixed
        sumpost1<-data.frame(predictor=row.names(sumpost),sumpost,response=resp[i])
        row.names(sumpost1)<-1:dim(sumpost1)[1]
        coefs<-rbind(sumpost1,coefs)
        # get predictions for response curves (remember to insert new variable!!)
        totl<-dim(indat[is.na(indat[,resp[i]]),])[1]
        b<-(dim(indat)[1]-totl)+1
        e<-dim(indat)[1]
        df<-data.frame(indat[b:e,],predicted=mod$summary.fitted.values$mean[b:e],es=resp[i])
        responsecurves<-rbind(df,responsecurves) # store results
        
    } # end of i loop
    # reshape data for response curves
    c.l<-as.character(unique(responsecurves$predrespcurve))
    responsecurves1<-NULL
    responsecurves2<-NULL
    for (z in 1:length(c.l)){
       if(class(responsecurves[,c.l[z]])=="factor"){
        tmp<-subset(responsecurves,predrespcurve==c.l[z])
        tmp1<-tmp[,c("predicted","predrespcurve","es",c.l[z])]
        colnames(tmp1)[dim(tmp1)[2]]<-c("value")
        tmp2<-unique(tmp1)
        responsecurves1<-rbind(tmp2,responsecurves1)
        } else {
        dmp<-subset(responsecurves,predrespcurve==c.l[z])
        dmp1<-dmp[,c("predicted","predrespcurve","es",c.l[z])]
        colnames(dmp1)[dim(dmp1)[2]]<-c("value")
        write.csv(dmp1,file="dmp1.csv",row.names=F);dmp1<-read.csv("dmp1.csv")
        responsecurves2<-rbind(dmp1,responsecurves2)
        }
    }
    # sort out factorial variables
    responsecurves1<-unique(responsecurves1)
    write.csv(responsecurves2,file="responsecurves2.csv",row.names=FALSE);responsecurves2<-read.csv("responsecurves2.csv")
   ## support function 
    curveplot<-function(indat,inres){
        tmp<-subset(indat,is.na(Farm)) # not general enough
        var.l<-as.character(unique(inres$predrespcurve)) # not general enough
        res<-NULL
        for(i in 1:length(var.l)){
            tmp1<-subset(tmp,predrespcurve==var.l[i]) 
            cols<-c(var.l[i],paste(var.l[i],"s",sep=""))
            tmp2<-data.frame(predrespcurve=var.l[i],tmp1[,cols])
            colnames(tmp2)[2:3]<-c("value","values")
            res<-rbind(tmp2,res)
        }
        tmp1<-merge(inres,res)
        return(tmp1)
    }
responsecurves2<-curveplot(indat=indat,inres=responsecurves2)
    finalres<-list(coefs,posteriordis,responsecurves1,responsecurves2)
    names(finalres)<-c("coefficients","posteriordis","respcurves1","responsecurves2")
    return(finalres)
} 
