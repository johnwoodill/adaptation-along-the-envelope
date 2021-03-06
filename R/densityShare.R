densityShare <- function(x, variable, weight, dens = NULL){ 
  cropdat <- x
  #cropdat$crop <- tolower(cropdat$crop)
  varn <- which(names(cropdat) == variable)
  wn <- which(names(cropdat) == weight)
  names(cropdat)[[varn]] <- "variable"
  names(cropdat)[[wn]] <- "weight"
  
  mint <- min(cropdat$variable)
  maxt <- max(cropdat$variable)  
  
  cropsum <- cropdat %>% 
    group_by(crop) %>% 
    summarise(sum = sum(weight)) %>% 
    arrange(-sum)
  cropsum
  
  # Order new dens data
  croporder <- cropsum$crop
  
  if(!is.null(dens)){
  dens %>% 
    slice(match(croporder, crop))
  }
  
    crop1 <- cropsum$crop[1];    dat1 <- filter(cropdat, crop == crop1)
    crop2 <- cropsum$crop[2];    dat2 <- filter(cropdat, crop == crop2)
    crop3 <- cropsum$crop[3];    dat3 <- filter(cropdat, crop == crop3)
    crop4 <- cropsum$crop[4];    dat4 <- filter(cropdat, crop == crop4)
    crop5 <- cropsum$crop[5];    dat5 <- filter(cropdat, crop == crop5)
    
    sum.corn <- sum(dat1$weight)
    sum.cotton <- sum(dat2$weight)
    sum.hay <- sum(dat3$weight)
    sum.wheat <- sum(dat4$weight)
    sum.soybean <- sum(dat5$weight)
    sum.all <- sum.corn + sum.cotton + sum.hay + sum.wheat + sum.soybean
    
    dens.dat1 <- density(dat1$variable, weight = dat1$weight/sum.all, from = 0, to = 30, n = 60)
    dens.dat2 <- density(dat2$variable, weight = dat2$weight/sum.all, from = 0, to = 30, n = 60)
    dens.dat3 <- density(dat3$variable, weight = dat3$weight/sum.all, from = 0, to = 30, n = 60)
    dens.dat4 <- density(dat4$variable, weight = dat4$weight/sum.all, from = 0, to = 30, n = 60)
    dens.dat5 <- density(dat5$variable, weight = dat5$weight/sum.all, from = 0, to = 30, n = 60)
    
    # Adjust density based on production/acre levels
    if(is.null(dens)){
      dens.dat1$y <- dens.dat1$y*cropsum$sum[1]
      dens.dat2$y <- dens.dat2$y*cropsum$sum[2]
      dens.dat3$y <- dens.dat3$y*cropsum$sum[3]
      dens.dat4$y <- dens.dat4$y*cropsum$sum[4]
      dens.dat5$y <- dens.dat5$y*cropsum$sum[5]
    }
    
    if(!is.null(dens)){
      dens.dat1$y <- dens.dat1$y*dens$value[1]
      dens.dat2$y <- dens.dat2$y*dens$value[2]
      dens.dat3$y <- dens.dat3$y*dens$value[3]
      dens.dat4$y <- dens.dat4$y*dens$value[4]
      dens.dat5$y <- dens.dat5$y*dens$value[5]
    }
    
    dens.dat5$y <- dens.dat5$y + dens.dat4$y
    dens.dat4$y <- dens.dat4$y + dens.dat5$y
    dens.dat3$y <- dens.dat3$y + dens.dat4$y
    dens.dat2$y <- dens.dat2$y + dens.dat3$y
    dens.dat1$y <- dens.dat1$y + dens.dat2$y
    
    ymax <- (max(dens.dat1$y, dens.dat2$y, dens.dat3$y, dens.dat4$y, dens.dat5$y))
    
    
    plot1 <- ggplot(NULL, aes(x = dens.dat1$x, y = dens.dat1$y)) + 
      geom_polygon(aes(x = dens.dat1$x, y = dens.dat1$y, fill = dat1$crop[1])) +
      geom_polygon(aes(x = dens.dat2$x, y = dens.dat2$y, fill = dat2$crop[1])) + 
      geom_polygon(aes(x = dens.dat3$x, y = dens.dat3$y, fill = dat3$crop[1])) + 
      geom_polygon(aes(x = dens.dat4$x, y = dens.dat4$y, fill = dat4$crop[1])) +
      geom_polygon(aes(x = dens.dat5$x, y = dens.dat5$y, fill = dat5$crop[1])) + 
      xlab(NULL) + ylab(NULL)+ ylim(0, ceiling(ymax/0.15)*.15) + theme_tufte(base_size = 14) +
      #scale_fill_discrete(breaks = c("Corn", "soybean", "hay", "wheat", "cotton")) + 
      theme(legend.position="top") + 
      theme(legend.title=element_blank()) +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey")
      
    retdat <- data.frame(x = c(dens.dat1$x, dens.dat2$x, dens.dat3$x, dens.dat4$x, dens.dat5$x),
               y = c(dens.dat1$y, dens.dat2$y, dens.dat3$y, dens.dat4$y, dens.dat5$y),
               crop = c(rep(cropsum$crop[1], length(dens.dat1$x)),
                        rep(cropsum$crop[2], length(dens.dat2$x)),
                        rep(cropsum$crop[3], length(dens.dat3$x)),
                        rep(cropsum$crop[4], length(dens.dat4$x)),
                        rep(cropsum$crop[5], length(dens.dat5$x))))
    retlist <- list(plot = plot1,
                    densdata = retdat)
    return(retlist)
}
