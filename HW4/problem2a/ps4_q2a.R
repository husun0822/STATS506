#### Multicore parallel computing for PS4 Q2a####
## The code aims at calculating FWER, FDR, Sensitivity and Specificity
## of a specific parametric model with Monte Carlo simulation for 
## calculating p-value and with severalmultiple-comparison methods
## for doing p-value adjustment. Both point estimation and standard
## errors are calculated for the four amounts of our interest.

source("ps4_q2_funcs.R")
library(parallel)
library(mnormt)

# Basic parameters to be used:
n = 1000 # Number of Observations per Monte Carlo simulation
p = 100
beta = c(rep(0.1,10),rep(0,90))
sigma = 1
mc_rep = 100

# parameter list where mclapply would operate on
rho = list(-0.75,-0.5,-0.25,0,0.25,0.5,0.75)

# the function doing Monte Carlo simulation with only rho, mc_rep and sigma being the input
simulation = function(rho,sigma,mc_rep){
  # The first step is to generate a data matrix to use:
  X = datagen(n,p,rho,beta)
  
  # the p-value matrix
  P = multicompare(X,beta,sigma,mc_rep)
}

# The list storing the uncorrected p-value matrix of each simulation
p_val = mclapply(rho,simulation,sigma=1,mc_rep=mc_rep,mc.cores = 32L)

# And to obtain the final estimation for each amount of interest, we 
# do parallel computing also on different p-value adjust method
multi_method = list("bonferroni","holm","BH","BY")

estimation = function(method,mat,beta){
  # We first do p-value adjustment for the p-value matrix
  A = apply(mat,2,p.adjust,method=method)
  
  # Get point estimation for FWER, FDR, Sensitivity and Specificity:
  point = evaluate(A,beta)
  
  n = dim(A)[2]
  B = NULL
  for (i in 1:n){
    m = as.matrix(A[,i])
    l = evaluate(m,beta)
    B = rbind(B,l)
  }
  se_est = apply(B,2,sd)
  result = rbind(point,se_est)
}

# The list storing the final result in a list format
results_q4a = NULL

for (i in 1:length(p_val)){
  M = mclapply(multi_method,estimation,mat=p_val[[i]],beta=beta,mc.cores = 32L)
  results_q4a[[i]] = M
}

save(results_q4a,file = "PS4_q2a.RData")


