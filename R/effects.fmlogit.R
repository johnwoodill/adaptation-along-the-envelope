effects.fmlogit<-function(object,effect=c("marginal","discrete"),
                          marg.type="atmean",se=F,varlist = NULL,at=NULL,R=1000){
  j=length(object$estimates)+1; K=dim(object$estimates[[1]])[1]; N=dim(object$y)[1]
  betamat = object$coefficient
  R = R # for Krinsky-Robb sampling
  # determine variables
  Xnames = colnames(object$X); ynames = colnames(object$y)
  if(length(varlist)==0){
    varlist=Xnames[-K]
    var_colNo = c(1:(K-1))
    k = length(var_colNo)
  }else{
    var_colNo = which(varlist %in% Xnames)
    k = length(var_colNo)
  }
  
  xmarg = matrix(ncol=k,nrow=j)
  se_mat = matrix(ncol=k,nrow=j)
  
  if(effect == "marginal"){
    # calculate marginal effects
    yhat = predict(object); yhat = as.matrix(yhat)
    for(c in var_colNo){
      c1 = which(var_colNo == c)
      if(marg.type == "aveacr"){
        # this is the average marginal effect for all observations
        beta_bar = as.vector(yhat %*% betamat[,c])
        betak_long = matrix(rep(betamat[,c],N),nrow=N,byrow=T)
        marg_mat =  yhat * (betak_long-beta_bar)
        xmarg[,c1] = colMeans(marg_mat)
      }
      if(marg.type == "atmean"){
        # this is the marginal effect at the mean
        # mean calculation
        yhat_mean = predict(object,newdata=colMeans(object$X[,-K]))
        beta_bar = sum(yhat_mean * betamat[,c])
        betak = betamat[,c]
        marg_vec = yhat_mean * (betak - beta_bar)
        xmarg[,c1] = as.numeric(marg_vec) 
      }
      if(se==T){
        # se calculation, using atmean by default
        se_k = rep(0,j)
        for(i in 1:j){
          se_k[i] = sqrt(diag(object$vcov[[i]])[c])
          new_betak = rnorm(R,betamat[j,c],se_k[i])
          marg_matrix = matrix(nrow=R,ncol=j)
          for(r in 1:R){
            new_betamat = betamat; new_betamat[i,c] = new_betak[r]
            yhat_mean = predict(object,newdata=colMeans(object$X[,-K]),newbeta = new_betamat)
            beta_bar = sum(yhat_mean * new_betamat[,c])
            betak = new_betamat[,c]
            marg_vec = yhat_mean * (betak - beta_bar)
            marg_matrix[r,i] = as.numeric(marg_vec)[i]
          }
          se_mat[i,c1] = sd(marg_matrix[,i])
        }}}}
  
  if(effect=="discrete"){
    for(c in var_colNo){
      c1 = which(var_colNo == c)
      if(marg.type == "aveacr"){
        Xmin <- Xmax <- object$X[,-K]
        Xmin[,c] = min(object$X[,c])
        Xmax[,c] = max(object$X[,c])
        yhat_min = predict(object,newdata=Xmin)
        yhat_max = predict(object,newdata=Xmax)
        ydisc = yhat_max - yhat_min
        xmarg[,c1] = colMeans(ydisc)
      }
      if(marg.type == "atmean"){
        Xmin <- Xmax <- colMeans(object$X[,-K])
        Xmin[c] = min(object$X[,c])
        Xmax[c] = max(object$X[,c])
        yhat_min = predict(object,newdata=Xmin)
        yhat_max = predict(object,newdata=Xmax)
        ydisc = yhat_max - yhat_min
        xmarg[,c1] = as.numeric(ydisc)
      }
      if(se==T){
        # se calculation for discrete margins. using atmean by default
        se_k = rep(0,j)
        Xmin <- Xmax <- colMeans(object$X[,-K])
        Xmin[c] = min(object$X[,c])
        Xmax[c] = max(object$X[,c])
        marg_matrix = matrix(nrow=R,ncol=j)
        for(i in 1:j){
          se_k[i] = sqrt(diag(object$vcov[[i]])[c])
          new_betak = rnorm(R,betamat[j,c],se_k[i])      
          for(r in 1:R){
            new_betamat = betamat; new_betamat[i,c] = new_betak[r]
            yhat_min = predict(object,newdata=Xmin,newbeta = new_betamat)
            yhat_max = predict(object,newdata=Xmax,newbeta = new_betamat)
            ydisc = yhat_max - yhat_min
            marg_matrix[r,i] = as.numeric(ydisc)[i]
          }
          se_mat[i,c1] = sd(marg_matrix[,i])
        }}}}
  # generating hypothesis testing tables.
  listmat = list()
  for(i in 1:k){
    tabout = matrix(ncol=4,nrow=j)
    tabout[,1:2] = cbind(xmarg[,i],se_mat[,i])
    tabout[,3] = tabout[,1] / tabout[,2]
    tabout[,4] = 2*(1-pnorm(abs(tabout[,3])))
    colnames(tabout) = c("estimate","std","z","p-value")
    rownames(tabout) = ynames
    listmat[[i]] = tabout
  }
  names(listmat)=varlist
  
  colnames(xmarg) <- colnames(se_mat) <- varlist
  rownames(xmarg) <- rownames(se_mat) <-colnames(object$y)
  outlist=list()
  outlist$effects = xmarg
  if(se==T){outlist$se = se_mat; outlist$ztable = listmat}
  marg.type.out = ifelse(marg.type=="atmean","at the mean,","average across observations,")
  outlist$R = ifelse(se==T,R,NA)
  outlist$expl = paste(effect,"effect",marg.type.out,
                       ifelse(se==T,"Krinsky-Robb standard error calculated","standard error not computed"))
  return(structure(outlist,class="fmlogit.margins"))
}