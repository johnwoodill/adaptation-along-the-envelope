fitWeight <- function(x){
  
  #if(isTRUE(weight)){
  dat <- x
  retdat <- dat %>% 
    group_by(crop) %>% 
    mutate(ind = row_number()) %>% 
    spread(crop, prop) %>% 
    select(-ind)
  rowSums(retdat[, 2:6])
  #retdat[,2:6] <- retdat[,2:6]/rowSums(retdat[, c(2:6)])
  retdat[ ,2:6] <- retdat[ ,2:6]/pmax(rowSums(retdat[ ,2:6]), 1)
  rowSums(retdat[, 2:6])
  return(retdat)
}
  #}
  
#   if(isTRUE(LeaveOut)){
#     dat <- x
#     retdat <- dat %>% 
#       group_by(crop) %>% 
#       mutate(ind = row_number()) %>% 
#       spread(crop, prop) %>% 
#       select(-ind)
#     retdat[,6] <- (1 - rowSums(retdat,2:5))
#     rowSums(retdat)
#     retdat <- retdat/pmax(rowSums(retdat), 1)
#   }
# }
