######################################### Model Building & Evaluation ######################################


# Explanatory variables and intercept in the design matrix. 
a <- data.frame(intercept= rep(1,100),
                a = rnorm(n = 100) * 5,
                b = rnorm(n = 100) * 3 + 1,
                c = rnorm(n = 100) * 2 + 2,
                d = rnorm(n = 100))

# Setting up a perfect relationship (linear problem) with +1 or -1 for classes with some sparsity 
b <- 2 * as.integer(((a[, 2] * 2) + (a[, 3] * 3) + (a[, 4] * 4) + (a[, 5] * 0) ) > 20) - 1



# Add column names and intercept
colnames(a) <- c("intercept", "a", "b", "c","d")
a <- as.matrix(a)

# Stochastic ONS diagonal version with projection


ONSproj <- function(a, b, init, iters = length(b), cost,  instgrad, lambda,z ) {
  
  ind<-sample(1:length(b),iters,replace=TRUE)
  a<-a[ind,]
  b<-b[ind]
  
  param <- data.frame(matrix(nrow = iters, ncol = length(init) + 1))
  colnames(param) <- c(colnames(a), "Loss")
  param[1, ] <- c(c( init), cost( init, a[1,], b[1],lambda))
  gamm <- 1/2 *min(1/(8*z),1)
  s <- rep(1/(gamm^2*(2*z)^2),length(init))
  
  for (i in 2:iters) { 
    x <- as.numeric(param[i-1, 1:length(init)])
    s <- s + (instgrad(x, a[i,], b[i], lambda)+x)^2
    y <- as.numeric(param[i-1, 1:length(init)]) - 1/gamm * 1/s * (instgrad(x, a[i,], b[i], lambda)+x)
    param[i, 1:length(init)] <- pib1w(y,s,z)
    param[i, length(init) + 1] <-  cost( colMeans(as.matrix(param[1:i, 1:length(init)])), a[i,], b[i],lambda)
  }
  
  cat("Final cost: ", sprintf("%10.07f", param[nrow(param), ncol(param)]), "\n", sep = "")
  cat("Parameters:", as.numeric(param[nrow(param), 1:length(init) ]), sep = " ")
  
  param <- cbind(Iteration = 1:nrow(param), param)
  
  return(param)
  
}


Adaproj <- function(a, b, init, iters = length(b), cost,  instgrad, lambda,z ) {
  
  ind<-sample(1:length(b),iters,replace=TRUE)
  a<-a[ind,]
  b<-b[ind]
  
  param <- data.frame(matrix(nrow = iters, ncol = length(init) + 1))
  colnames(param) <- c(colnames(a), "Loss")
  param[1, ] <- c(c( init), cost( init, a[1,], b[1],lambda))
  s <- rep(1/(4*length(init)* z),length(init))
  eta <- 2*z
  
  for (i in 2:iters) { 
    s <- s + instgrad(as.numeric(param[i-1, 1:length(init)]), a[i,], b[i], lambda)^2
    y <- as.numeric(param[i-1, 1:length(init)]) - eta * 1/sqrt(s) * instgrad(as.numeric(param[i-1, 1:length(init)]), a[i,], b[i], lambda)
    param[i, 1:length(init)] <- pib1w(y,sqrt(s),z)
    param[i, length(init) + 1] <-  cost( colMeans(as.matrix(param[1:i, 1:length(init)])), a[i,], b[i],lambda)
  }
  
  cat("Final cost: ", sprintf("%10.07f", param[nrow(param), ncol(param)]), "\n", sep = "")
  cat("Parameters:", as.numeric(param[nrow(param), 1:length(init) ]), sep = " ")
  
  param <- cbind(Iteration = 1:nrow(param), param)
  
  return(param)
  
}

# Comparaison between ONS and Ada projected

SMDproj <- function(a, b, init, iters = length(b), cost,  instgrad, lambda,z) {
  
  ind<-sample(1:length(b),iters,replace=TRUE)
  a<-a[ind,]
  b<-b[ind]
  
  param <- data.frame(matrix(nrow = iters, ncol = length(init) + 1))
  colnames(param) <- c(colnames(a), "Loss")
  param[1, ] <- c(c(init), cost( init, a[1,], b[1],lambda))
  theta <- c(init)
  
  for (i in 2:iters) {
    eta <- 1/sqrt(i)  # 1/i play with the learning rate
    theta <- theta - eta * instgrad(theta, a[i,], b[i], lambda)
    param[i, 1:length(init)] <- pib1(theta,z)
    param[i, length(init) + 1] <-  cost( colMeans(as.matrix(param[1:i, 1:length(init)])), a[i,], b[i],lambda)
  }
  
  cat("Final cost: ", sprintf("%10.07f", param[nrow(param), ncol(param)]), "\n", sep = "")
  cat("Parameters:", as.numeric(param[nrow(param), 1:length(init) ]), sep = " ")
  
  param <- cbind(Iteration = 1:nrow(param), param)
  
  return(param)
  
}



start_time <- Sys.time()

set.seed(1)   


paramONSproj <- ONSproj(a = a,
                        b = b,
                        init = rep(0, 5),
                        iters = 1000,  # Play with the number of iterations
                        cost = hingereg,
                        instgrad = instgradreg,
                        lambda = 1,  # Play with the regularization parameter
                        z=.5) 
set.seed(1)   # Same seed!

paramAdaproj <- Adaproj(a = a,
                        b = b,
                        init = rep(0, 5),
                        iters = 1000,  # Play with the number of iterations
                        cost = hingereg,
                        instgrad = instgradreg,
                        lambda = 1,  # Play with the regularization parameter
                        z=.5) 
end_time <- Sys.time()

# Different trajectories 


plot.ts(paramONSproj[, 2:(ncol(paramONSproj) - 1)])
plot.ts(paramAdaproj[, 2:(ncol(paramAdaproj) - 1)])

# Comparison ONS proj and Ada proj on 1000 iterations

plot.ts(paramONSproj$Iteration, paramONSproj$Loss-paramAdaproj$Loss )

mean(paramONSproj$Loss-paramAdaproj$Loss)


