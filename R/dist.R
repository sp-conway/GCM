# Function for calculating distance between two matrices of stimuli per Nosofsky (1986)
# ASSUMES DIMENSIONS ARE ORDERED IN THE SAME ORDER FOR EACH STIMULUS MATRIX
# uses compmats function to compare all stimuli of one matrix to all of the other
# then calculates distance
#
#
# Arguments
# stim 1 - matrix of first set of stimuli
# stim 2 - matrix of second set of stimuli
# c - Nosofsky sensitivity parameter - default is 1 
# w - Nosofsky attention weighting parameter 
# n_dims - # of dimensions stimuli vary along - default is ncol(stim1)
# r - 1 = city-block distance, 2=Euclidean
#
# Returns
# Matrix of distance between each stimulus of stim1 to each stimulus of stim2
# Sean Conway
# August 2020
source('~/Box/RSean/compmats.R')
library(stringr)
dist<-function(stim1, stim2,c=1,w,n_dims,r){
  names<-c(deparse(substitute(stim1)),
           deparse(substitute(stim2)))
  
  # using compmats function to get matrices ready to compare
  mat_1<-compmats(stim1,stim2)[[1]]
  mat_2<-compmats(stim1,stim2)[[2]]
  
  # get observation numbers, column names
  mat_1_dim_idx<-str_which(colnames(mat_1),names[[1]])
  stim_1_obs_num<-mat_1[,str_which(colnames(mat_1),names[[1]],negate = TRUE)]
  mat_2_dim_idx<-str_which(colnames(mat_2),names[[2]])
  stim_2_obs_num<-mat_2[,str_which(colnames(mat_2),names[[2]],negate = TRUE)]
  
  # matrices with just dimension values
  mat_1<-mat_1[,mat_1_dim_idx]
  mat_2<-mat_2[,mat_1_dim_idx]
  
  # perform distance calculations
  dist_mat<-matrix(data=NA,ncol = n_dims,nrow=nrow(mat_1))
  
  # Had to use a for loop here - hopefully looping over columns isn't too slow
  for(i in 1:n_dims){
    dist_mat[,i]<-(w[i]*(mat_1[,i]-mat_2[,i]))^r
  }
  
  dist_mat<-cbind(dist_mat, 'dist'=rowSums(dist_mat))
  dist_mat[,'dist']<-c*(dist_mat[,'dist']^(1/r))
  
  # get distance vals & observation numbers together
  dist_mat<-cbind(dist_mat[,'dist'],stim_1_obs_num, stim_2_obs_num)
  colnames(dist_mat)<-c('dist',names)
  return(dist_mat)
}
