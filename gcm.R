# Function for implementation of Nosofsky's (1986) Generalized Context Model
# Sean Conway, July 2020

# FUNCTION INPUT
# params - model parameters. paraams[1]<- c - the sensitivity parameter
# params[2] <- w - dimension weighting parameter
# stim - matrix of x y MDS coordinates for classification stimuli - have row names be stimulus names
# categories - matrix of x y MDS coordinates for category stimuli - have row names be category names

# FUNCTION RETURNS
# cat_probs - data frames with probability of classifying each unique stimulus into each unique category

library(dplyr)
gcm<-function(params, stim, categories){
  # c - sensitivity parameter
  c<-params[1]
  
  # w - dimension weighting parameter. Number of w values = Number of dimensions
  w<-c(params[2], 1-params[2])
  
  #Euclidean distance (For now) - May need to change to  1 (city block distance)
  r<-2 
  
  # Calculate distance from each stimulus to every category in MDS space
  # Do for x and y separately for simplicity
  x_dist<-lapply(1:nrow(stim),FUN=function(i) categories[,1] - stim[i,1])
  y_dist<-lapply(1:nrow(stim),FUN=function(i) categories[,2] - stim[i,2])
  
  # X distances
  x_dist<-as.matrix(unlist(x_dist))
  
  # Preserving row names ensures we know for which category the row refers to
  x_dist<-cbind(x_dist,rownames(x_dist))
  x_dist<-cbind(rep(rownames(stim),each=nrow(categories)),x_dist[,1])
  
  # Y distances
  y_dist<-matrix(unlist(y_dist))
  y_dist<-cbind(rep(rownames(stim),each=nrow(categories)),y_dist[,1])
  
  # Get X and Y distances together in a data frame
  cat_probs<-as.data.frame(cbind(x_dist,y_dist[,1],row.names(x_dist)))
  colnames(cat_probs)<-c('Stim','X_Diff','Y_Diff','Cat')
  cat_probs$X_Diff<-as.numeric(cat_probs$X_Diff)
  cat_probs$Y_Diff<-as.numeric(cat_probs$Y_Diff)
  
  # Now finish calculating distances per Nosofsky
  cat_probs$X_Diff<-params[1]*abs(cat_probs$X_Diff)^r
  cat_probs$Y_Diff<-params[2]*abs(cat_probs$Y_Diff)^r
  cat_probs$Summed_D<-(cat_probs$X_Diff+cat_probs$Y_Diff)^(1/r)
  cat_probs$Summed_D <-cat_probs$Summed_D*c
  
  # Exponential decay function to convert distance to similarity
  cat_probs$Sim<-exp(-cat_probs$Summed_D)
  
  # For each stimulus, need total sum for all categories
  # e.g. for stimulus 1, need to sum similarity to A, to, B, etc...
  All_Sums<-cat_probs %>%
    group_by(Stim) %>%
    summarise(Sum_Sim=sum(Sim))
  cat_probs<-inner_join(cat_probs,All_Sums)
  
  # Calculate probability per GCM rule
  cat_probs$Prob<-cat_probs$Sim/cat_probs$Sum_Sim
  return(cat_probs)
}

