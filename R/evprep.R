#' Prepare data for calculating the evaluation strip
#'
#' @inpred = list of predictors
#' @indat = dataframe containing predictors
#' @ref = random effect
#' 


evprep<-function(inpred,indat,ref=NULL){
   ##### Random effect #####
    if(is.null(ref)==FALSE){                            # random effect, held constant throughout
        freq<-as.data.frame(table(indat[,ref]))         # table of frequencies for re levels
        indat[,ref]<-freq[which.max(freq$Freq),]$Var1   # get level with highest frequency
        indat[,ref]<-as.numeric(paste(indat[,ref]))     # insert new random effect value
    }
    ###### Predictor varying #####
    res.l<-vector("list",length(inpred))                # predictors
    names(res.l)<-inpred                                # store results, assign names of predictors
    for (i in 1:length(inpred)){
        indat1<-indat                                   # convert into tmp dataframe
        if(class(indat1[,inpred[i]])=="factor"){
            mean.l<-inpred[-c(i)] 
            predint<-sample(unique(indat1[,inpred[i]]),100,replace=TRUE)
            # if factor do...!!!!!!
        } else {
            mean.l<-inpred[-c(i)]                    # column names except predictor of interest!                   
            predint<-seq(min(indat1[,inpred[i]]),max(indat1[,inpred[i]]),
                         length.out = 100)  # if continous generate sequence of values
        }
        ###### Predictors held constant ######
        predsec1<-NULL                     # dataframe of results for factor
        predsec2<-NULL                     # dataframe of results for continous predictors
        for (x in 1:length(mean.l)){      # for each predictor
            if(class(indat1[,mean.l[x]])=="factor"){            # if factor 
                freqx<-as.data.frame(table(indat1[,mean.l[x]])) # compute table of frequences
                dff<-data.frame(name=mean.l[x],value=rep(freqx[which.max(freqx$Freq),]$Var1,100),ID=1:100) # choose the most frequent level
                predsec1<-rbind(dff,predsec1) # bind to data frame
            } else { # if continous 
                df<-data.frame(name=mean.l[x],value=rep(mean(indat1[,mean.l[x]]),100),ID=1:100) # replicate mean value 100 times
                predsec2<-rbind(df,predsec2) # bind to dataframe
            }
        }
        predsec1<-try(spread(predsec1,name,value),silent=TRUE) # spread data.frame for predictors with constant value (factors)
        if(class(predsec1)=="try-error"){ # if no factors
            predsec2<-spread(predsec2,name,value) # spread continous predictors
            predsec3<-predsec2 # and give another name
        } else { # if factors present                                 
            predsec2<-spread(predsec2,name,value) # spread continous predictors the normal way
            predsec3<-cbind(predsec1,predsec2) # bind factor stuff and continous
        }
        ###### Bind part held constant and continous part
        predsec4<-predsec3[, !(colnames(predsec3) %in% c("ID"))] # exclude variable ID from 
        predsec4$tmp<-predint  # insert in a temporary column with varying predictor (range of values)
        colnames(predsec4)[dim(predsec4)[2]]<-inpred[i] # rename column
        predsec4$tmp<-unique(indat[,ref]) # insert constant random effect (always constant)
        colnames(predsec4)[dim(predsec4)[2]]<-ref # and rename this column too!
        res.l[[i]]<-predsec4 # store data
    }
    return(res.l)  
}
