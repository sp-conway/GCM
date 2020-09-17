# gcm predict function for 2 categories
## Exemplars will be in a list for each category
## Stimuli in matrix
source('~/Box/GCM/R/similarity.R')
source('~/Box/GCM/R/distance.R')

gcm_pred2<-function(params,stimuli, exemplars){
  c<-params[1]
  w<-params[2]
  b<-c(params[3],1-params[3])
  n_cats<-length(exemplars)
  n_stim<-nrow(stimuli)
  exemplar_coords<-matrix(0,ncol=n_cats)
  sim_mat<-matrix(0,ncol=n_cats+1, nrow=n_stim)
  
  for(i in 1:n_stim){
    for(j in 1:n_cats){
      exemplar_coords<-exemplars[[j]]
      n_exemps<-nrow(exemplars[[j]])
      stim<-stimuli[i,]
      dist<-unlist(lapply(1:n_exemps, function(x) distance(exemplar_coords[x,],stim_coords_2=stim, w=w)))
      sim<-similarity(dist,c)
      sim_mat[i,j]<-sum(sim)
      sim_mat[i,j]<-sim_mat[i,j]*b[j]
    }
  }
  
  sim_mat[1:n_stim,n_cats+1]<-rowSums(sim_mat)
  prob_mat<-matrix(0,nrow=n_stim, ncol=n_cats)
  prob_mat<-sim_mat[1:n_stim,1:n_cats]/sim_mat[1:n_stim, n_cats+1]
  return(prob_mat)
}
