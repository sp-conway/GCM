# Generate predictions for set of test stimuli & exemplars
source('~/Box/GCM/R/gcm_pred2.R')
# Test stimulus coordinates
all_test_coords <- matrix(c(1,1,2,1,3,1,4,1,1,2,2,2,3,2,4,2,1,3,2,3,3,3,4,3,1,4,2,4,3,4,4,4),                          
                          ncol=2, byrow=TRUE)

# A matrix of exemplar coordinates for Category A
exemplar_A_coords <- matrix(c(1,4,1,3,2,2,3,1),
                            ncol=2, byrow=TRUE)

# A matrix of exemplar coordinates for Category B
exemplar_B_coords <- matrix(c(2,4,3,3,4,2,4,1),
                            ncol=2, byrow=TRUE)

# Put them together in a list
all_exemplar_coords <- list(exemplar_A_coords, exemplar_B_coords)

# Plot the exemplars and test stimuli.
# Red circle = Category A, Blue triangle = Category B, Green numbers = Stimuli
par(pty='s',
    mar=c(5.1, 5.1, 4.1, 2.1))

plot(NA, ty='n',
     xlim=c(1,4), ylim=c(1,4),
     cex.axis=1.5, cex.lab=1.5,
     xlab='Dimension 1', ylab='Dimension 2')

points(exemplar_A_coords,
       pch=1, lwd=2,
       col='red',
       cex=4)

points(exemplar_B_coords,
       pch=2, lwd=2,
       col='blue',
       cex=4)

for (i in 1:nrow(all_test_coords)) {
  text(x=all_test_coords[i,1], 
       y=all_test_coords[i,2],
       labels=as.character(i),
       cex=2, col='green')
}

# Simulated data
freq_cat_A_data <- c(24, 24, 25,  3, 28, 24, 13,  1, 29, 14,  7,  1, 26,  9,  8,  3)
N <- 30

# Some reasonable parameters
params <- c(2, .5, .5)

# gcm_fit_1
# Arguments:
#   params: a vector of GCM parameters, [sens_param, dim_1_weight, cat_A_bias].
#   all_test_coords: a matrix of all test stimulus coordinates.
#   freq_cat_A_data: the frequency that the participant put each of the test 
#     stimuli into Category A. In the same stimulus order as all_test_coords.
#   N: how many trials per stimulus were there in the experiment? 
#   all_exemplar_coords: a list of exemplar coordinates.
#     The first element [[1]] is the coordinate for Category A exemplars.
#     The second element [[2]] is the coordinate for Category B exemplars.
#   cost_type: the type of discrepancy function to use:
#     'rmsd' (default), 'chi-square', 'g-squared'.
#   plot: whether or not (default) to plot the data vs model.

gcm_rms<-function(params, stimuli, freq_cat_A_data, N, exemplars){
  # How many stimuli are there?
  n_stim <- nrow(all_test_coords)
  
  # Get the gcm prediction using a function you've already created.
  prob_cat_A_model <- gcm_pred2(params, stimuli, exemplars)[,1]
  #prob_model <- 1 - prob_model # more efficient, given two category experiment
  
  prob_cat_A_data <- freq_cat_A_data/N
  #freq_cat_B_data <- N - freq_cat_A_data
  #prob_cat_B_data <- freq_cat_B_data/N
  #prob_data<-cbind(prob_cat_A_data, prob_cat_B_data)
  
  rms<-sqrt(sum((prob_cat_A_model-prob_cat_A_data)^2)/n_stim)
  
  rms
  #plot(prob_model[,1], prob_data[,1])
}

# Simulated data
freq_cat_A_data <- c(24, 24, 25,  3, 28, 24, 13,  1, 29, 14,  7,  1, 26,  9,  8,  3)
N <- 30

# Some reasonable parameters
params <- c(2, .5, .5)

# rmsd
# You should get 0.07263109.
gcm_rms(params=params, stimuli=all_test_coords, freq_cat_A_data=freq_cat_A_data,N= N, exemplars=all_exemplar_coords)  

c<-seq(0,5,by=.20)
w<-seq(0,1,by=.05)
b<-seq(0,1,by=.05)
n_c<-length(c)
n_w<-length(w)
n_b<-length(b)
rms_vals<-array(NA, c(n_c,n_w,n_b))

# Go through all combinations of all 3 parameters.
# This might take a while.
for (i in 1:n_c) {
  
  cat(i, ' of ', n_c, '\n')
  
  # Set the sens_param
  sens_param <- c[i]
  
  for (j in 1:n_w) {
    
    # Set the dim_1_weight
    dim_1_weight <- w[j]
    
    for (k in 1:n_b) {
      
      # Set the cat_A_bias
      cat_A_bias <- b[k]
      
      # Combine the parameters into a vector
      params <- c(sens_param, dim_1_weight, cat_A_bias)
      
      # Get and store the cost value
      rms_vals[i, j, k] <- gcm_rms(params, 
                                           all_test_coords, 
                                           freq_cat_A_data, 
                                           N, 
                                           all_exemplar_coords)
    }
  }
}
lower_bounds<-c(0,0,0)
upper_bounds<-c(10,1,1)
library(optimx)
start_params<-c(3,.1,.5)
optimx(start_params,
       fn=gcm_rms, 
       method='L-BFGS-B',
       lower=lower_bounds,
       upper=upper_bounds,
       stimuli=all_test_coords, 
       freq_cat_A_data=freq_cat_A_data,
       N= N, 
       exemplars=all_exemplar_coords)
       








