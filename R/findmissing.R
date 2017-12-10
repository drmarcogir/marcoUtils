#' Flag up non-matching rows when merging two data frames. 
#' 
#' @df1= dataframe containing variables of interest i.e. that need to matched to some other data
#' @df2= dataframe to be matched with (that may contain all matches with df1 or may not).
#' @col1= column from df1 that needs to match to data frame df2
#' @col2= column from df2 that needs to matched to dataframe df2
#' 

findmissing<-function(df1,df2,col1,col2){
    tmpdf1<-data.frame(tmp=df1[,col1])
    tmpdf2<-data.frame(tmp=df2[,col2])
    tmpdf2$flag<-tmpdf2[,1]
    tmpdf3<-merge(tmpdf1,tmpdf2,all.x=TRUE)
    colnames(tmpdf3)<-c(col1,"missing_check")
    return(tmpdf3)
}
