# Function for calculating distance between stimuli per Nosofsky (1986)
# only for 2 dimensional stimuli
dist<-function(X1,Y1,X2,Y2,c,w){
  r<-2 # Euclidean distance. May need to change to 1 (city-block)
  x<-w[1]*(abs(X1-X2)^r)
  y<-w[2]*(abs(Y1-Y2)^r)
  xy<-x+y
  xy<-c*(xy^(1/r))
  return(xy)
}
