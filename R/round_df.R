#'Round numerical values in a data frame containing a mixture of categorical and quantitative variables 
#'
#' @df=input data frame
#' @digits=integer indicating the number of decimal places
#'

round_df <- function(df,digits){
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    return(df)
}


