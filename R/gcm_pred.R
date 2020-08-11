# Function for obtaining predictions from Nosofsky's (1986) Generalized Context Model
#
# ARGUMENTS
#   params - model parameters. 
#     params[1]<- c - the sensitivity parameter
#     params[2] <- w - dimension weighting parameter
#       -LIST IN ORDER OF DIMENSIONS
#       -sum of all w = 1
#     params[3] <- b - category response bias
#       -IN ORDER OF CATEGORIES 
#     params[4] (OPTIONAL) <- gamma - deterministic responding
#       -bound from 0:inf
#   stim - matrix of MDS coordinates for classification stimuli
#     *Note: Present code assumes two dimensions. Needs to be modified if >2
#   categories - matrix of x y MDS coordinates for category stimuli 
#     *Note: Present code assumes two dimensions. Needs to be modified if >2
#   stim_names - stimulus names - in order of matrix rows
#   exemplar names - in order of matrix rows
#     e.g. In two category structure, 3 exemps per cat, = c(1,1,1,2,2,2) 
#
# FUNCTION RETURNS
# cat_probs - data frame with probability of classifying each unique stimulus into each unique category
# cat_probs_w_sim <- same as cat_probs but includes similarity values as well
library(dplyr)

gcm_pred<-function(params, stim, categories,stim_names,exemplar_names){
  
  # c - sensitivity parameter
  c<-params[1]
  
  # w - dimension weighting parameter(s). Number of dimensions = Number of w values
  # w[1] = w_x = dimension weighting for x
  # w[2] = w_y = dimension weighting for y
  # Need to change if > 2 dimensions
  w<-c(params[2],1-params[2])
  
  # b - category response bias
  b<-c(params[3],1-params[3])
  
  # gamma - from Ashby & Maddox (1993) - optional parameter
  if (length(params)==4){
    gamma<-params[4]
  } else{
    gamma<-1
  }
  
  # Name category rows by the exemplar names
  if(is.null(rownames(categories))){
    rownames(categories)<-exemplar_names
  }
  
  if(is.null(rownames(stim))){
    rownames(stim)<-paste(seq(1:nrow(all_test_coords)))
  }
  
  n_stim<-nrow(stim)
  n_cats<-unique(exemplar_names)
  
  # Repeat each stimulus X & Y value by the number of exemplars it needs to be compared to
  stim_x<-unlist(lapply(1:nrow(stim),FUN=function(i) rep(stim[i,1],each=nrow(categories))))
  stim_y<-unlist(lapply(1:nrow(stim),FUN=function(i) rep(stim[i,2],each=nrow(categories))))
  
  # Get stimulus names in a vector that matches the structure of stim_x, stim_y
  # Keeps track of which stimulus is compared to which exemplar
  stim_names_var<-unlist(lapply(1:nrow(stim),FUN=function(i) rep(stim_names[i],each=nrow(categories))))
  
  # Get stimulus in a matrix, where each stimulus name, x val, y val, repeated by n exemplars
  stim<-cbind(stim_names_var,stim_x,stim_y)
  colnames(stim)<-c("Stim","Stim_X","Stim_Y")
  
  # Category exemplars in a matrix repeated by n stimuli to be compared to
  categories<-matrix(data=rep(categories,3),ncol=ncol(categories))
  categories<-cbind(rep(exemplar_names,3),categories)
  colnames(categories)<-c("Cat","Cat_X","Cat_Y")
  
  # all in DF
  comb<-as.data.frame(cbind(stim,categories))
  comb$Stim<-as.factor(comb$Stim)
  comb$Cat<-as.factor(comb$Cat)
  
  # Using dist function
  source('~/Box/GCM/R/dist.R')
  comb$Dist<-dist(comb$Stim_X,comb$Stim_Y,comb$Cat_X,comb$Cat_Y,c=c,w=w)
  
  # Convert distances to similarities per Nosofsky (1986)
  comb$sim<-exp(-comb$Dist)
  
  # Get b values ready to be added to df
  bias<-rep(b,each=n_stim)
  
  # For each category x stim combo, sum similarity values
  cat_probs <- comb %>%
    group_by(Stim,Cat) %>%
    summarise(sim=sum(sim))
  
  # Order by category name
  cat_probs<-cat_probs %>% arrange(Cat)
  
  # Put bias in
  cat_probs$Bias<-bias
  
  # Include gamma
  cat_probs$sim<-cat_probs$sim^gamma
  
  # Multiply similarity to a category by that category's response bias
  cat_probs$sim_bias<-cat_probs$sim*cat_probs$Bias
  
  # Sum all similarities for each stimulus
  All_Sums <- cat_probs %>%
    group_by(Stim) %>%
    summarise(T_Sim=sum(sim_bias))
  
  # Join summed vals
  cat_probs<-inner_join(cat_probs,All_Sums,by="Stim")
  
  # Probability calculation
  cat_probs$prob<-cat_probs$sim_bias/cat_probs$T_Sim
  
  return(cat_probs)
}
