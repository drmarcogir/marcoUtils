#' Ordering labels of a categorical factor making reference to a numerical legend
#' factor has to be created with quantiles and cut!
#'
#'@ inv=input vector containing wrong factor
#' 

laborder<-function(inv){
# simplify vector created with quantiles/cut. here it takes the first number of the factor on the left hand side. ordering will be done using this number.
df<-data.frame(orig=levels(as.factor(inv)),small=as.numeric(stri_split_fixed(levels(as.factor(inv)),"-",simplify=TRUE)[,1])) 
df$oldid<-1:dim(df)[1] # insert an id for the order of the wrong factor
df1<-arrange(df,small) # arrange by the first number
# create a new lookup dataframe with correct order
newflookup<-data.frame(inv=factor(df1$orig,levels=as.character(df1$orig)),inv1=factor(df1$orig,levels=as.character(df1$orig)))
# take the big vector from the original input, convert it into dataframe and insert id
invdf<-data.frame(inv,id=1:length(inv))
# merge new correct lookup dataframe with big original vector containing an id
invdf1<-merge(invdf,newflookup)
# arrange by id of the big original vector
invdf2<-arrange(invdf1,id)
# create the final output vector
finalv<-invdf2$inv1
return(finalv)
}