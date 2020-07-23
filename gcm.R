# Function for implementation of Nosofsky's (1986) Generalized Context Model
# Sean Conway, July 2020
require(dplyr)
gcm<-function(params, stim, categories){
  c<-params[1]
  # If using two dimensions, just need one w (the other is 1-w)
  w<-c(params[2], 1-params[2])
  
  r<-2 # May need to change to  1 (city-block)
  
  # Change column names just to be safe
  colnames(stim)<-c('x','y')
  colnames(categories)<-c('x','y')
  
  # Calculate distance from each stimulus to every category in MDS space
  # Do so for x and y separately for simplicity
  x_dist<-lapply(1:nrow(stim),FUN=function(i) categories[,'x'] - - stim[i,'x'])
  y_dist<-lapply(1:nrow(stim),FUN=function(i) categories[,'y'] - - stim[i,'y'])
  x_dist<-as.matrix(unlist(x_dist))
  
  # Preserving row names ensures we know for which category the row refers to
  x_dist<-cbind(x_dist,rownames(x_dist))
  x_dist<-cbind(rep(1:nrow(stim),each=nrow(categories)),x_dist[,1])
  
  y_dist<-matrix(unlist(y_dist))
  y_dist<-cbind(rep(1:nrow(stim),each=nrow(categories)),y_dist[,1])
  
  # Get X and Y distances together in a data frame
  dist_mat<-cbind(x_dist,y_dist[,1],row.names(x_dist))
  dist_mat<-as.data.frame(dist_mat)
  colnames(dist_mat)<-c('Stim','X_Diff','Y_Diff','Cat')
  rownames(dist_mat)<-NULL
  dist_mat$X_Diff<-as.numeric(dist_mat$X_Diff)
  dist_mat$Y_Diff<-as.numeric(dist_mat$Y_Diff)
  
  # Now finish calculating distances per Nosofsky
  dist_mat$X_Diff<-params[1]*abs(dist_mat$X_Diff)^r
  dist_mat$Y_Diff<-params[2]*abs(dist_mat$Y_Diff)^r
  dist_mat$Summed_D<-(dist_mat$X_Diff+dist_mat$Y_Diff)^(1/r)
  dist_mat$Summed_D <-dist_mat$Summed_D*c
  
  # Exponential decay function to convert distance to similarity
  dist_mat$Sim<-exp(-dist_mat$Summed_D)
  
  # For each stimulus, need total sum for all categories
  # e.g. for stimulus 1, need to sum similarity to A, to, B, etc...
  All_Sums<-dist_mat %>%
    group_by(Stim) %>%
    summarise(Sum_Sim=sum(Sim))
  dist_mat<-left_join(dist_mat,All_Sums)
  
  # Calculate probability per GCM rule
  dist_mat$Prob<-dist_mat$Sim/dist_mat$Sum_Sim
  cat_probs<-dist_mat
  return(cat_probs)
}

