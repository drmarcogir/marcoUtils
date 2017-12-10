#' Calculate deviance explained by GLM objects 
#'
#' @model = name of model object (GLM)
#'

d2 <- function(model){ 
    round(1-(model$deviance/model$null.deviance),4) 
}
