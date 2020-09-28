## Testing out gcm_pred function, which takes more than two categories.

set.seed(150)
stimuli <- matrix(data=rnorm(20,m=3,sd=10), ncol=2)
cat_a <- matrix(data=runif(10,0,10), ncol=2)
cat_b <- matrix(data=runif(10,5,10),ncol=2)
cat_c <- matrix(data=runif(10,1,4),ncol=2)
exemplars <- list(cat_a, cat_b, cat_c)

c <- 4
w <- .4
bias <- c(.2,.3)
params <- c(c, w, bias)
source('~/Box/GCM/R/gcm_pred.R')
gcm_pred(params=params, stimuli=stimuli, exemplar=exemplars)

# From andrew's script from modeling seminar F20 -------------------------------------------------------------
# Put them together into a matrix
all_test_coords <- rbind(test_coords_1, test_coords_2, test_coords_3)
all_test_coords

# A matrix of exemplar coordinates for Category A
exemplar_A_coords <- matrix(c(2, 3,
                              3, 3,
                              3, 2),
                            ncol=2, byrow=TRUE)

# A matrix of exemplar coordinates for Category B
exemplar_B_coords <- matrix(c(1, 2,
                              1, 1,
                              2, 1),
                            ncol=2, byrow=TRUE)

# Put them together in a list
all_exemplar_coords <- list(exemplar_A_coords, exemplar_B_coords)
params <- c(5, .5, .5) 
model_predictions <- gcm_pred(params, all_test_coords, all_exemplar_coords)
model_predictions
# You should get 0.5000000 0.5000000 0.9684522

all_test_coords <- rbind(test_coords_1, test_coords_2, test_coords_3)
all_test_coords

# From andrew's script from modeling seminar F20 -------------------------------------------------------------
# trying it with a third category of my own
# A matrix of exemplar coordinates for Category A
exemplar_A_coords <- matrix(c(2, 3,
                              3, 3,
                              3, 2),
                            ncol=2, byrow=TRUE)

# A matrix of exemplar coordinates for Category B
exemplar_B_coords <- matrix(c(1, 2,
                              1, 1,
                              2, 1),
                            ncol=2, byrow=TRUE)
# A matrix of exemplar coordinates for Category B
exemplar_C_coords <- matrix(c(1,4,
                            4,1,
                            3,6),
                            ncol = 2,
                            byrow = T)
# Put them together in a list
all_exemplar_coords <- list(exemplar_A_coords, exemplar_B_coords,exemplar_C_coords)
params <- c(5, .5, .5, .3) # add in second r. bias param
model_predictions <- gcm_pred(params, all_test_coords, all_exemplar_coords)
model_predictions
# appears to be working
