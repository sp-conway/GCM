# Wrapper function to generate gcm predictions and compare to data
# Returns predicted data, sse
gcm_compare_preds<-function(params, stim, categories,stim_names,exemplar_names,data){
  source('./gcm_pred.R')
  pred_dat<-gcm_pred(params, stim, categories,stim_names,exemplar_names)[[1]]
  sse<-sum((pred_dat$prob - data$prob)^2)
  return(sse)#list(pred_dat,sse))
}

  
