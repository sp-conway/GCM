# Using this script to get predicted gcm data, given parameters
# Attempting to recover parameters from the model later on

source('/Users/seanconway/Box/GCM/R/gcm_pred.R')

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
params <- c(1, .5, .5)  # c, w, b

gen_data<-gcm_pred(params = params, stim=all_test_coords, categories = exemplar_coords, 
                   stim_names = stim_names, exemplar_names = exemplar_cats)

#### Try again with new data
stim2<-matrix(data=runif(40,min = -40,max=40),ncol=2)
exemplars2<-matrix(data=runif(1000,min=-40,max=40),ncol=2)
stim_names2<-seq(1:nrow(stim2))
exemplar_cats2<-c(rep(1,times=250),rep(2,times=250))
params2<-c(6.5,.5,.5)
gen_data2<-gcm_pred(params=params2,stim=stim2,categories=exemplars2,
                    stim_names=stim_names2,exemplar_names = exemplar_cats2)



