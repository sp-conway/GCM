# gcm predict function for multiple (>=2) categories
# Exemplars will be in a list for each category
# Stimuli in matrix
# Still assuming 2 dimensions
#
#
# Arguments:
#       1 - params - vector of parameters
#           params[1] - sensitvity
#           params[2] - w - attention weighting for dimension 1
#           params[3], params[4]...through number of categories minus 1 - response bias for category j. 
#           all but last category must be defined.
#           OPTIONAL PARAMETER - last value in params - gamma (determ/prob responding)
#       2 - stimuli - matrix of stimuli, x y coordinates
#       3 - exemplars - given j categories, list of j exemplar matrices w/ x y coords.
#       4 - determ - is a gamma parameter specified - logical argument - default is FALSE. 
#           If true, need to specify a gamma value in last value in params.
# 
# Output:
#       1 - prob_mat - matrix showing the probability of classifying each stimulus i into
#           each category j
# Sean Conway
# Sept. 2020


source('~/Box/GCM/R/similarity.R')
source('~/Box/GCM/R/distance.R')

gcm_pred <- function(params,stimuli, exemplars, determ = F){
  # number of categories 
  n_cats <- length(exemplars)
  
  # sensitivity
  c <- params[1]
  
  # attention weighting for dimension 1.
  w <- params[2]
  
  # number of bias parameters to estimate (1 - number of categories)
  n_bias <- n_cats - 1
  
  # response bias for cat 1, cat 2, etc...all must sum to 1
  b <- c(params[3:(3+n_bias-1)])
  b <- c(b, 1-sum(b))
  
  # optional gamma parameter - determines amount of deterministic v. probabilistic responding
  # should be last value in the params vector if used
  if (determ){
   g <- tail(params, n=1)
  }
  else {
   g <- 1
  }
  
  # number of test stimuli
  n_stim<-nrow(stimuli)
  
  # create a matrix of exemplar coordinates (to be filled in by for loop)
  exemplar_coords <- matrix(0,ncol=n_cats)
  
  # Similarity matrix to be filled in
  # last column is row similarity (i.e., summed similarity of stimulus i to all exemplars j.)
  sim_mat <- matrix(0,ncol=n_cats+1, nrow=n_stim)
  
  # iterate through each stimulus & each category.
  for(i in 1:n_stim){
    for(j in 1:n_cats){
      
      # all exemplar coordinates for exemplars in category j
      exemplar_coords <- exemplars[[j]]
      
      # number of exemplars in category j
      n_exemps<-nrow(exemplars[[j]])
      
      # stimulus to compare to exemplars in cat. j
      stim <- stimuli[i,]
      
      # for each exemplar in j, calculate distance between exemplar and a given stimulus i
      dist <- sapply(1:n_exemps, function(x) distance(exemplar_coords[x,],stim_coords_2=stim, w=w))
      
      # convert distance to similarity, given c (sensitivity)
      sim <- similarity(dist,c)
      
      # add up all similarities of a given stimulus to all exemplars of a particular category.
      sim_mat[i,j] <- sum(sim)
      
      # incorporate response bias and gamma parameter
      sim_mat[i,j] <- (sim_mat[i,j]*b[j])^g
    }
  }
  
  # last column is summed similarity of stimulus to all categories 
  # (i.e., denominator of gcm probability calculation)
  sim_mat[1:n_stim,n_cats+1] <- rowSums(sim_mat)
  
  # create probability matrix
  prob_mat <- matrix(0,nrow=n_stim, ncol=n_cats)
  
  # probability of categorizing each stimulus into each category. 
  # have to divide by summed similarity
  # of a particular stimulus to all exemplars of both categories
  prob_mat <- sim_mat[1:n_stim,1:n_cats]/sim_mat[1:n_stim, n_cats+1]
  return(prob_mat)
}
