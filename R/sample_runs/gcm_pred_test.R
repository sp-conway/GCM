# Testing out GCM on sample data, generating predicted probabilities
source('./gcm_pred.R')

# Andrew Sample Data for GCM
all_test_coords <- matrix(c(3, 1,
                            2, 2,
                            2.5, 2.5),
                          ncol=2, byrow=TRUE)
stim_names<-seq(1:nrow(all_test_coords))

# Category exemplars
exemplar_coords <- matrix(c(2,3,
                            3,3,
                            3,2,
                            1,2,
                            1,1,
                            2,1),
                          ncol=2, byrow=TRUE)
# Exemplar categories
exemplar_cats <- c(1, 1, 1, 2, 2, 2)
# Parameters
params <- c(1, .5, .5) #,100)  # c, w, b, [gamma] [optional]

pred_dat<-gcm_pred(params = params, stim=all_test_coords, categories = exemplar_coords, 
         stim_names = stim_names, exemplar_names = exemplar_cats)[[1]]
