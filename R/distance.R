distance <- function(stim_coords_1, stim_coords_2, w) {
  
  # This value determines whether a city-block (1) or Euclidean (2) metric is 
  # used. We'll use Euclidean for now, so r=2. Remember that x^(1/2) = sqrt(x).
  r <- 2
  
  w<-c(w,1-w[1])
  
  # Compute the distance (dij)  
  # **********************************
  x <- w[1]*(abs(stim_coords_1[1]-stim_coords_2[1])^r)
  y <- w[2]*(abs(stim_coords_1[2]-stim_coords_2[2])^r)
  xy <- x+y
  d <- (xy^(1/r))
  # **********************************
  
  return(d)
  
}