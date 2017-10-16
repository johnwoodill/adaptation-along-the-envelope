boot.strap <- function(x, rep = 1000, sample = length(x), cluster = NULL){
  if (is.null(cluster)){
    newdat.sum <- c()
    newdat.mean <- c()
    for (r in 1:rep){
      sampdat <- sample(x, size = sample, replace = TRUE)
      newdat.sum[r] <- sqrt(length(sampdat))*var(sampdat)
      newdat.mean[r] <- var(sampdat)/sqrt(length(sampdat))
    }
    retdat <- list(se.sum = mean(newdat.sum),
                se.mean = mean(newdat.mean))
    return(retdat)
  } 
}
  
boot_strap_rev <- function(x, rep = 1000, sample = length(x)){
    newdat.sum <- c()
    for (r in 1:rep){
      sampldat <- sample(x, size = sample, replace = TRUE)
      newdat.sum[r] <- sum(sampldat)
    }
    #retdat <- mean(newdat.sum)
    retdat <- var(sampldat)/sqrt(length(sampldat))
    return(retdat)
    }

