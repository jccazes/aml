
# projection for a vector v with non-negative coordinates on the simplex of radius z

pisimplex <- function(v,z=1){
  n <- length(v)
  u <- sort(v, TRUE)
  
  su <- cumsum(u)
  
  
  rho <- max(which(u > (su-z) / (1:n)))
  
  
  theta <- (su[rho] -z) / rho
  
  
  w <- pmax(v - theta, 0)
  return(w) 
}

pib1 <- function(x,z=1){
  v <- abs(x)
  if (sum(v)>z){
  w <- pisimplex(v,z)
  x<-sign(x)*w
  }
  return(x)
}

pib1w <- function(x,w,z=1){
  v <- abs(x* w)
  u <- order(-v)
  
  sx <- cumsum(abs(x)[u])
  
  
  sw <- cumsum(1/w[u])
  
  rho <- max(which(v[u] > (sx-z) / sw))
  
  
  theta <- (sx[rho] -z) / sw[rho]
  
  
 if (sum(abs(x))>z)
  x<-sign(x)*pmax(abs(x) - theta/w, 0)
  return(x)
}
