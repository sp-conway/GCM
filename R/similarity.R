# similarity.R 
# Function for converting psychological distance to similarity for Nosofsky's GCM (1986)
#
#
# Arguments:
# x - a distance values
# f - 1=gaussian, 2=exponential decay
#
# Sean Conway
# 09/05/2020
#
# Change history:
#
#
similarity<-function(x,f){
  if (f==1){
    s<-exp(-x^2)
  }
  else{
    s<-exp(-x)
  }
  return(s)
}


