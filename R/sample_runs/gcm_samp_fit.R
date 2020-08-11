# Getting the 'actual' data
source('./sample_runs/gen_data.R')

# Function for calling gcm_pred, getting predictions, and comparing to data 
# Returns predictions & sse
source('./gcm_compare_preds.R')

library(optimx)

# Making up starting values for parameters
start_params<-c(10,.01,.99)

gcm_compare_preds(params=start_params,
                  stim=all_test_coords,
                  categories=exemplar_coords,
                  stim_names=stim_names,
                  exemplar_names = exemplar_cats,
                  data=gen_data)

fit<-optimx(par=start_params,
            fn=gcm_compare_preds,
            method = 'L-BFGS-B',
            lower = c(0,0,0),
            upper = c(10, 1,1),
            stim=all_test_coords,
            categories=exemplar_coords,
            stim_names=stim_names,
            exemplar_names = exemplar_cats,
            data=gen_data)
