# Function for implementation of Nosofsky's (1986) Generalized Context Model
# Sean Conway, July 2020

gcm<-function(params, stim, categories){
  c<-params[1]
  w<-c(params[2], 1-params[2])
  r<-2 # May need to change to  1 (city-block)
  apply(categories, 1, FUN = stim-categories)
  dist_mat<-mapply(distance,c,w,r,stim,categories)
  dist_mat<-matrix(dist_mat,nrow=nrow(categories),ncol=ncol(categories))
  sim_mat<-exp(-dist_mat^r)
}

a<-c(1,3)
b<-c(2,46,6,5)
d<-c(3,6)

test<-function(a,b,d){
  a+b+d
}
mapply(test,a,b,d)
c<-2
r<-2
w<-c(.3,.7) 
stim<-c(4,9)
categories<-matrix(data=c(5,4,3,2,5,2,5,2),nrow=4,ncol = 2)
mapply(distance,c,w,r,stim,categories)
