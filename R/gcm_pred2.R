# gcm predict function for 2 categories
## Exemplars will be in a list for each category
## Stimuli in matrix
source('~/Box/GCM/R/similarity.R')
source('~/Box/GCM/R/distance.R')

gcm_pred2<-function(params,stimuli, exemplars){
  # sensitivity
  c<-params[1]
  
  # attention weighting for dimension 1.
  w<-params[2]
  
  # response bias for cat 1, cat 2
  b<-c(params[3],1-params[3])
  
  # number of categories (will always be 2)
  n_cats<-length(exemplars)
  
  # number of test stimuli
  n_stim<-nrow(stimuli)
  
  # create a matrix of exemplar coordinates (to be filled in in for loop)
  exemplar_coords<-matrix(0,ncol=n_cats)
  
  # Similarity matrix to be filled in
  # last column is row similarity (i.e., summed similarity of stimulus i to all exemplars j.)
  sim_mat<-matrix(0,ncol=n_cats+1, nrow=n_stim)
  
  # iterate through every stimulus & each category.
  for(i in 1:n_stim){
    for(j in 1:n_cats){
      
      # all exemplar coordinates for exemplars in category j
      exemplar_coords<-exemplars[[j]]
      
      # number of exemplars in category j
      n_exemps<-nrow(exemplars[[j]])
      
      # stimulus to compare to exemplars in cat. j
      stim<-stimuli[i,]
      
      # for each exemplar in j, calculate distance between exemplar and a given stimulus i
      dist<-unlist(lapply(1:n_exemps, function(x) distance(exemplar_coords[x,],stim_coords_2=stim, w=w)))
      
      # convert distance to similarity, given c (sensitivity)
      sim<-similarity(dist,c)
      
      # add up all similarities of a given stimulus to all exemplars of a particular category.
      sim_mat[i,j]<-sum(sim)
      sim_mat[i,j]<-sim_mat[i,j]*b[j]
    }
  }
  
  # last column is summed similarity of stimulus to all categories (i.e., denominator of gcm)
  sim_mat[1:n_stim,n_cats+1]<-rowSums(sim_mat)
  
  # create probability matrix
  prob_mat<-matrix(0,nrow=n_stim, ncol=n_cats)
  
  # probability of categorizing each stimulus into each category. have to divide by summed similarity
  # of a particular stimulus to all exemplars of both categories
  prob_mat<-sim_mat[1:n_stim,1:n_cats]/sim_mat[1:n_stim, n_cats+1]
  return(prob_mat)
}
