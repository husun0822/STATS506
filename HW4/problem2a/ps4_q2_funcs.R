datagen = function(n,p,rho,beta){
  # First of all, we need to generate data matrix X from a multinormal distribution
  # And to settle down the distribution function,
  # we need to firstly specify the variance-covariance matrix
  
  var_cov = matrix(0,nrow=p,ncol=p)
  diag(var_cov)=1
  sigma = beta %o% beta # the outer product of the beta vector
  diag(sigma) = 0
  var_cov = var_cov+sigma # this gives us the specific variance-covariance matrix we need
  X = mnormt::rmnorm(n,mean=rep(0,p),varcov = var_cov)
  X
}



multicompare = function(X,beta,sigma,mc_rep){
  n = dim(X)[1]
  p = dim(X)[2]
  p_val = matrix(0,nrow = p,ncol = mc_rep)
  M = qr(t(X)%*%X) # QR decomposition of t(X)%*%X
  Y_mean = X%*%beta
  cM = chol(t(X)%*%X)
  M_inv = chol2inv(cM) # inverse of t(X)%*%X
  
  for (i in 1:mc_rep){
    set.seed(i)
    Y=Y_mean+rnorm(n,mean=0,sd = sigma) # new data of Y
    beta_est = solve(qr.R(M), t(qr.Q(M))%*% t(X) %*% Y)
    Y_est = X%*%beta_est
    error_est = sum((Y-Y_est)^2)/(n-p) # estimate error rate using RSS
    for (j in 1:p){
      var_betaj = error_est*M_inv[j,j]
      z_j = beta_est[j]/sqrt(var_betaj)
      p_val[j,i] = 2*(1-pnorm(abs(z_j)))
    }
  }
  
  p_val
}


# Function used to calculate FWER, FDR, Sensitivity and Specificity

evaluate = function(mat,beta){
  # Args:
  # mat is the output p-value matrix of the multicompare function above
  # beta is the true data-generating beta parameter
  
  m = dim(mat)[2]
  p = length(beta)
  alternative = which(beta!=0) # indices where beta is not zero
  
  # TP: True Positive cases
  # TN: True Negative cases
  # FP: False Positive cases
  # FN: False Negative cases
  
  if(m==1){
    TP = sum(mat[alternative,]<0.05)
    TN = sum(mat[-alternative,]>0.05)
    FP = sum(mat[-alternative,]<0.05)
    FN = sum(mat[alternative,]>0.05)
  }else{
    TP = sum(apply(mat[alternative,],1,function(x){x<0.05}))
    TN = sum(apply(mat[-alternative,],1,function(x){x>0.05}))
    FP = sum(apply(mat[-alternative,],1,function(x){x<0.05}))
    FN = sum(apply(mat[alternative,],1,function(x){x>0.05}))    
  }
  
  FWER = FP/(TP+TN+FP+FN)
  FDR = FP/(TP+FP)
  Sens = TP/(TP+FN)
  Spec = TN/(TN+FP)
  
  
  result = c("FWER"=FWER,"FDR"=FDR,"Sensitivity"=Sens,"Specificity"=Spec)
  result
}

