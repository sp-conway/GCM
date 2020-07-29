# Function for obtaining predictions from Nosofsky's (1986) Generalized Context Model
# Sean Conway, July 2020

# FUNCTION INPUT
# params - model parameters. paraams[1]<- c - the sensitivity parameter
# params[2] <- w - dimension weighting parameter
# stim - matrix of x y MDS coordinates for classification stimuli - have row names be stimulus names
# categories - matrix of x y MDS coordinates for category stimuli - have row names be category names

# FUNCTION RETURNS
# cat_probs - data frame with probability of classifying each unique stimulus into each unique category

library(dplyr)
library(data.table)
gcm_pred<-function(params, stim, categories,stim_names,exemplar_names){
  # c - sensitivity parameter
  c<-params[1]
  
  # w - dimension weighting parameter(s). Number of dimensions = Number of w values
  # w[1] = w_x = dimension weighting for x
  # w[2] = w_y = dimension weighting for y
  w<-c(params[2], params[3])
  
  #Euclidean distance (For now) - May need to change to  1 (city block distance)
  r<-1
  
  rownames(categories)<-exemplar_names
  
  # Calculate distance from each stimulus to every category in MDS space
  # Do for x and y separately for simplicity
  x_dist<-lapply(1:nrow(stim),FUN=function(i) stim[i,]-categories[,1])
  y_dist<-lapply(1:nrow(stim),FUN=function(i) stim[i,2]-categories[,2])
  
  # This gets stimuli/category names in a way that ensures each is matched correctly to their comparisons
  exemps_var<-lapply(1:nrow(stim),FUN=function(i) paste(names(x_dist[[i]])))
  exemps_var<-as.vector(unlist(exemps_var))
  stim_names_var<-lapply(1:nrow(stim),FUN=function(i) rep(stim_names[i],each=nrow(categories)))
  stim_names_var<-as.vector(unlist(stim_names_var))
  
  # X distances
  x_dist<-as.matrix(unlist(x_dist))
  
  # Y distances
  y_dist<-matrix(unlist(y_dist))

  # Get X and Y distances together in a data frame
  dist_df<-as.data.frame(cbind(stim_names_var, exemps_var,x_dist[,1],y_dist[,1]))
  colnames(dist_df)<-c('Stim','Exemp','X_Diff','Y_Diff')
  rownames(dist_df)<-NULL
  dist_df$Cat<-substr(dist_df$Exemp,start=1,stop=1)
  dist_df$Cat<-as.factor(dist_df$Cat)
  dist_df$X_Diff<-as.numeric(dist_df$X_Diff)
  dist_df$X_Diff<-abs(dist_df$X_Diff)
  dist_df$Y_Diff<-as.numeric(dist_df$Y_Diff)
  dist_df$Y_Diff<-abs(dist_df$Y_Diff)
                      
  # Now finish calculating distances per Nosofsky
  dist_df$X_Y_Sum<-lapply(1:nrow(dist_df),function(i) w[1]*(dist_df$X_Diff[i]^r)+w[2]*(dist_df$Y_Diff[i])^r)
  ##dist_df$X_Y_Sum<-dist_df$X_Diff+dist_df$Y_Diff
  dist_df$Stim<-as.factor(dist_df$Stim)
  dist_df$Cat<-as.factor(dist_df$Cat)
  cat_probs <-dist_df %>%
    group_by(Stim,Cat)%>%
    summarise(Sum = sum(X_Y_Sum))
  cat_probs$Summed_D<-cat_probs$Sum^(1/r)
  cat_probs$Summed_D <-cat_probs$Summed_D*c
  cat_probs$Sim<-exp(-cat_probs$Summed_D)
  All_Sums <- cat_probs %>%
    group_by(Stim) %>%
    summarise(T_Sim=sum(Sim))
  cat_probs<-inner_join(cat_probs,All_Sums,by="Stim")

  # Calculate probability per GCM rule
  cat_probs$Prob<-cat_probs$Sim/cat_probs$T_Sim
  return(cat_probs)
}
