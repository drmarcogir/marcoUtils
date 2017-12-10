#' Function for downloading data from GBIF 
#'
#' library(rgbif) #
#' options(gbif_user="myusername", 
#' gbif_pwd="mypassword", gbif_email="myemailaddress") 
#' download.mg("Canis lupus") 
#'@sp=character vector indicating column containing species names
#'

downloadgbif<- function(sp) {
  results <- NULL
  for (i in 1:length(sp)) {
    # get species key
    key <- name_backbone(name = sp[i])$speciesKey
    if(is.null(key)){
      next
    } else { 
      count<-occ_count(taxonKey=key)
    }
    if(count < 2){
      next
    } else {
      key1 <- paste("taxonKey = ", key)
      did <- occ_download(key1, "hasCoordinate = TRUE", "hasGeospatialIssue = FALSE")
      id = did[1]
      res <- try(suppressMessages(occ_download_get(id, overwrite = TRUE)), silent = T)
      while (class(res) == "try-error") {
        res <- try(suppressMessages(occ_download_get(id, overwrite = TRUE)), silent = T)
      }
      options(warn = -1)
      res1 <- suppressMessages(occ_download_import(x = res))
      options(warn = -0)
      if(dim(res1)[1]==0){
        next
      } else { 
        spdat <- data.frame(species = sp[i], lat = res1$decimalLatitude, lon = res1$decimalLongitude,year=res1$year)
        results <- rbind(spdat, results)
        toremove <- list.files(pattern = ".zip")
        file.remove(toremove)
      }
    }
  }
  return(results)   
}

