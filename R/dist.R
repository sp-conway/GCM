# Function for calculating distance between two matrices of stimuli per Nosofsky (1986)
# only for 2 dimensional stimuli
# ASSUMES DIMENSIONS ARE ORDERED IN THE SAME ORDER FOR EACH STIMULUS MATRIX
# uses compmat function to compare all stimuli of one matrix to all of the other
# then calculates distance
#
#
# Arguments
# stim 1 - matrix of first set of stimuli
# stim 2 - matrix of second set of stimuli
# c - Nosofsky sensitivity parameter
# w - Nosofsky attention weighting parameter
# n_dims - # of dimensions stimuli vary along
#
# Returns
# Matrix of distance between each stimulus of stim1 to each stimulus of stim2
# Sean Conway
# August 2020
source('~/Box/RSean/comp_mat.R')
library(stringr)
dist<-function(stim1, stim2,c,w,n_dims){
  r<-2 # Euclidean distance. May need to change to 1 (city-block)
  # using compmat function
  # Because compmat will create column names in the form 'matrix_name'+'dimension_name',
  # Can find which are the dimension names (and rule out observation names), by searching columns for matrix name
  names<-c(deparse(substitute(stim1)),
           deparse(substitute(stim2)))
  mat_1<-compmat(stim1,stim2)[[1]]
  mat_2<-compmat(stim1,stim2)[[2]]
  stim_numbers_1<-mat_1[,'Observation_Number_1']
  stim_numbers_2<-mat_2[,'Observation_Number_2']
  mat_1_dim_idx<-str_which(colnames(mat_1),names[[1]])
  mat_2_dim_idx<-str_which(colnames(mat_2),names[[2]])
  
  mat_1<-mat_1[,mat_1_dim_idx]
  mat_2<-mat_2[,mat_1_dim_idx]
  
  dist_mat<-sapply(1:n_dims, function(i) mat_1[,i]-mat_2[,i])
  dist_mat<-sapply(1:n_dims, function(i) (dist_mat[,i]*w[i])^r)
  dist_mat<-cbind(dist_mat, 'dist'=rowSums(dist_mat))
  dist_mat[,'dist']<-c*(dist_mat[,'dist']^(1/r))
  
  dist_mat<-cbind(dist_mat[,'dist'],stim_numbers_1, stim_numbers_2)
  colnames(dist_mat)<-c('dist',names)
  return(dist_mat)
}
