boot.strap <- function(x, rep = 1000, sample = length(x), cluster = NULL){
  if (is.null(cluster)){
    newdat.sum <- c()
    newdat.mean <- c()
    for (r in 1:rep){
      sampdat <- sample(x, size = sample, replace = TRUE)
      newdat.sum[r] <- sum(sampdat)
      newdat.mean[r] <- mean(sampdat)
    }
    retdat <- list(se.sum = sd(newdat.sum),
                se.mean = sd(newdat.mean))
    return(retdat)
  } 
}
  
boot_strap_rev <- function(x, rep = 1000, sample = length(x)){
    newdat.sum <- c()
    for (r in 1:rep){
      sampldat <- sample(x, size = sample, replace = TRUE)
      #newdat.sum[r] <- sum(sampldat)
      newdat.sum[r] <- sum(sampldat)
    }
    retdat <- sd(newdat.sum)
    #retdat <- sd(sampldat)*sqrt(length(sampldat))
    return(retdat)
    }

