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
#   categories - list of matrices of x y MDS coordinates for category stimuli 
#     *Note: Present code assumes two dimensions. Needs to be modified if >2
#   stim_names - stimulus names - in order of matrix rows
#   exemplar names - in order of matrix rows
#     e.g. In two category structure, 3 exemps per cat, = c(1,1,1,2,2,2) 
#
# FUNCTION RETURNS
# cat_probs - data frame with probability of classifying each unique stimulus into each unique category
# cat_probs_w_sim <- same as cat_probs but includes similarity values as well
#
# Assume dimensions ordered the same for stimuli & categories
# Sean Conway
# August 2020
# Change History:
#
#
### Currently ONLY WORKS FOR TWO CATEGORIES W/ TWO DIMENSIONS
library(dplyr)
source('~/Box/GCM/R/dist.R')

gcm_pred<-function(params, stim, categories,stim_names,exemplar_names){
  
  # c - sensitivity parameter
  c<-params[1]
  
  # w - dimension weighting parameter(s). Number of dimensions = Number of w values
  # w[1] = w_x = dimension weighting for x
  # w[2] = w_y = dimension weighting for y
  # Need to change if > 2 dimensions
  w<-c(params[2],1-params[2])
  
  # b - category response bias - b is bias for first category.
  n_cats<-unique(exemplar_names)
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
    rownames(stim)<-paste(seq(1:nrow(stim)))
  }
  
  dist_mat<-dist(stim1=stim, stim2=categories[1], w=w,c=c,n_dims=2)
  dist_mat<-cbind(dist_mat,'sim'=exp(-dist_mat[,'dist']))
  
  # For each category x stim combo, sum similarity values
  cat_probs <- comb %>%
    group_by(Stim,Cat) %>%
    summarise(sim=sum(sim))
  
  # Order by category name
  cat_probs<-cat_probs %>% arrange(Cat)
  
  # Get b values ready to be added to df
  bias<-rep(b,each=n_stim)
  
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
