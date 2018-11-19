#' Function for thinng species occurence data
#' @foo = dataframe with coordinates
#' @buffer = buffer size in meters
#' @reps = number of replicates (random selection within buffer)
#' 

bufferthin <- function(foo, buffer, reps){
    # Make list of suitable vectors
    suitable <- list()
    for(k in 1:reps){
        # Make the output vector
        outvec <- as.numeric(c())
        # Make the vector of dropped (buffered out) points
        dropvec <- c()
        for(i in 1:nrow(foo)){
            # Stop running when all points exhausted
            if(length(dropvec)<nrow(foo)){
                # Set the rows to sample from
                if(i>1){
                    rowsleft <- (1:nrow(foo))[-c(dropvec)]
                } else {
                    rowsleft <- 1:nrow(foo)
                }
                # Randomly select point
                outpoint <- as.numeric(sample(as.character(rowsleft),1))
                outvec[i] <- outpoint
                # Remove points within buffer
                outcoord <- foo[outpoint,c("x","y")]
                #dropvec <- c(dropvec, which(sqrt((foo$x-outcoord$x)^2 + (foo$y-outcoord$y)^2)<buffer))
                dropvec <- c(dropvec, which(pointDistance(outcoord,foo, lonlat=TRUE)<buffer))
                # Remove unnecessary duplicates in the buffered points
                dropvec <- dropvec[!duplicated(dropvec)]
            } 
        } 
        # Populate the suitable points list
        suitable[[k]] <- outvec
    }
    # Go through the iterations and pick a list with the most data
    best <- unlist(suitable[which.max(lapply(suitable,length))])
    foo[best,]
}