# Getting the 'actual' data
setwd('/Users/seanconway/Box/GCM/R')
source('./sample_runs/gen_data.R')

# Function for calling gcm_pred, getting predictions, and comparing to data 
# Returns predictions & sse
source('gcm_compare_preds.R')

# Actual gcm predict
source('gcm_pred.R')

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

fit_params<-c(fit$p1,fit$p2,fit$p3)
predictions<-gcm_pred(fit_params, all_test_coords, exemplar_coords,stim_names,exemplar_cats)

### Try it again with new data
source('./sample_runs/gen_data.R')

# Making up starting values for parameters
start_params<-c(10,.01,.99)
fit2<-optimx(par=start_params,
            fn=gcm_compare_preds,
            method = 'L-BFGS-B',
            lower = c(0,0,0),
            upper = c(10, 1,1),
            stim=stim2,
            categories=exemplars2,
            stim_names=stim_names2,
            exemplar_names = exemplar_cats2,
            data=gen_data2)
fit2
