#### Multicore parallel computing for PS4 Q2c####
## The code aims at calculating FWER, FDR, Sensitivity and Specificity
## of a specific parametric model with Monte Carlo simulation for 
## calculating p-value and with severalmultiple-comparison methods
## for doing p-value adjustment. Both point estimation and standard
## errors are calculated for the four amounts of our interest.

source("ps4_q2_funcs.R")
library(parallel)
library(futures)
library(mnormt)


# Default value for doing the Monte Carlo simulation:
n_cores=1
mcrep=1e2
n=1000
sigma=0.75
p = 100
beta = c(rep(0.1,10),rep(0,90))
rho = list(-0.75,-0.5,-0.25,0,0.25,0.5,0.75)

# args store the value of sigma, mc_rep and n_cores from the command line
args = commandArgs(trailingOnly = TRUE)
print(args)

# these code follows the in-class GammaMLE example:
args_to_list = function(args){
  ind = grep('=', args)  
  args_list = strsplit(args[ind], '=')
  names(args_list) = sapply(args_list, function(x) x[1])
  
  args_list = lapply(args_list, function(x) as.numeric(x[2]))
  args_list
}

args_list_in = args_to_list(args)

sigma = args_list_in[["sigma"]]
mc_rep = args_list_in[["mc_rep"]]
n_cores = args_list_in[["n_cores"]]


# the function doing Monte Carlo simulation with only rho, mc_rep and sigma being the input
simulation = function(rho,sigma,mc_rep){
  # The first step is to generate a data matrix to use:
  X = datagen(n,p,rho,beta)
  
  # the p-value matrix
  P = multicompare(X,beta,sigma,mc_rep)
}

# Use futures for parallelism

plan(multicore)

p_val = list()
for (i in 1:length(rho)){
  p_val[[i]] = future(simulation(rho[[i]],sigma,mc_rep))
}

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
results_q4c = NULL

for (j in 1:length(rho)){
  M = mclapply(multi_method,estimation,mat=p_val[[j]],beta=beta,mc.cores = n_cores)
  results_q4c[[j]] = M
}

file = paste("PS4_q2c-",as.character(sigma*4),".RData")

save(results_q4c,file = file)


