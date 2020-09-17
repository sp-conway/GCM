# function for similarity in GCM
# x is psychological distance, c is sensitivity 

similarity<-function(x,c){
  s <- exp(-c*x)
  return(s)
}