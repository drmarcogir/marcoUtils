#'
#' Prepare a nice table of coefficients to be written on an excel spreadsheet
#' @incoef = input coefficients from INLA model
#'


coefstable<-function(incoef){
  coefs1<-incoef
  coefs1$predictor<-stri_split_fixed(coefs1$predictor,"ss",simplify = TRUE)[,1]
  coefs1$combined<-paste(round(coefs1$mean,digits=2),"[",round(coefs1$X0.025quant,digits=2),";",
  round(coefs1$X0.975quant ,digits=2),"]",sep="")
  df1<-spread(coefs1[,c("predictor","combined","response")],response,combined)
  return(df1)
}

