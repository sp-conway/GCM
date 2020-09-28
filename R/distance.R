distance <- function(stim_coords_1, stim_coords_2, w, r=2) {
  # may have to change r to other values (e.g., r=1 if city-block)
  
  w<-c(w,1-w[1])
  
  # Compute the distance (dij)  
  # **********************************
  x <- w[1]*(abs(stim_coords_1[1]-stim_coords_2[1])^r)
  y <- w[2]*(abs(stim_coords_1[2]-stim_coords_2[2])^r)
  xy <- x+y
  d <- (xy^(1/r))
  return(d)
  
}