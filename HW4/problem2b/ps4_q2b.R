#### doParallel computing for PS4 Q2b ####
## The code aims at calculating FWER, FDR, Sensitivity and Specificity
## of a specific parametric model with Monte Carlo simulation for 
## calculating p-value and with severalmultiple-comparison methods
## for doing p-value adjustment. Both point estimation and standard
## errors are calculated for the four amounts of our interest.

setwd("~/STATS506/HW4/problem2b")
source("ps4_q2_funcs.R")
library(doParallel)
library(foreach)
library(iterators)
library(mnormt)
library(data.table)
library(dplyr)


# Basic parameters to be used:
n = 1000 # Number of Observations per Monte Carlo simulation
p = 100
beta = c(rep(0.1,10),rep(0,90))
mc_rep = 10000
core = 32L

# parameter list where doPar would operate on
rho = list(-0.75,-0.5,-0.25,0,0.25,0.5,0.75)
sigma = list(0.25,0.5,1)
multi_method = list("bonferroni","holm","BH","BY")

# For debugging and testing purposes, we use a simpler parameter list:
#rho = list(0)
#sigma = list(1)
#multi_method = list("bonferroni")

# the function doing Monte Carlo simulation with only rho, mc_rep and sigma being the input
simulation = function(rho,sigma,mc_rep){
  # The first step is to generate a data matrix to use:
  X = datagen(n,p,rho,beta)
  
  # the p-value matrix
  P = multicompare(X,beta,sigma,mc_rep)
}

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

# Set up the cluster

nCores = 4 #use a 4 core cluster
cl = makeCluster(nCores)

registerDoParallel(cl)

# make every cluster accessible to the package lib
clusterEvalQ(cl, .libPaths('/home/husun/R/x86_64-pc-linux-gnu-library/3.3')) 

cat('Starting running ...\n')

results_q4b = foreach(i=rho,.combine = 'rbind',.packages=c("data.table","dplyr")) %:%
  foreach(j=sigma,.combine='rbind',.packages=c("data.table","dplyr")) %:%
  foreach(k=multi_method,.combine='rbind',.packages=c("data.table","dplyr")) %dopar%{
    p_val = simulation(rho = i,sigma = j,mc_rep = mc_rep)
    M = estimation(method=k,mat=p_val,beta=beta)
    M = t(M)
    M = as.data.frame(M)%>% 
        setDT(keep.rownames = TRUE)%>%
        cbind(rep(i,4),rep(j,4),rep(k,4))
    M
  }

stopCluster(cl)


names(results_q4b) = c("metric","est","se","rho","sigma","method")
results_q4b = results_q4b[,.(rho,sigma,metric,method,est,se)]

save(results_q4b,file = "./results_q4b.RData")



