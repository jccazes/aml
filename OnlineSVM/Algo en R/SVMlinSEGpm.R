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

# Stochastic Exponentiated Gradient +_

SEGpm <- function(a, b, iters = length(b), cost,  instgrad, lambda,z) {
  
  ind<-sample(1:length(b),iters,replace=TRUE)
  a<-a[ind,]
  b<-b[ind]
  
  param <- data.frame(matrix(nrow = iters, ncol = ncol(a) + 1))
  colnames(param) <- c(colnames(a), "Loss")
  w <- rep(1/(2*ncol(a)),2*ncol(a))
  param[1, 1:ncol(a)] <- z*c(w[1:ncol(a)]-w[ncol(a)+1:ncol(a)])
  param[1, ncol(a)+1] <-  cost(as.numeric(param[1, 1:ncol(a)]), a[1,], b[1],lambda)
  eta <- z*sqrt(log(ncol(a))/iters)  #   play with the learning rate
  
  for (i in 2:iters) {
    x <- as.numeric(param[i-1, 1:ncol(a)])
    instg <- instgrad( x, a[i,], b[i], lambda)+x
    W <- c(exp( - eta * instg ),exp( eta *instg))*w
    w <- W/sum(W)
    param[i, 1:ncol(a)] <- z*c(w[1:ncol(a)]-w[ncol(a)+1:ncol(a)])
    param[i, ncol(a) + 1] <-  cost( colMeans(as.matrix(param[1:i, 1:ncol(a)])), a[i,], b[i],lambda)
  }
  
  cat("Final cost: ", sprintf("%10.07f", param[nrow(a), ncol(a)]), "\n", sep = "")
  cat("Parameters:", as.numeric(param[nrow(a), 1:ncol(a) ]), sep = " ")
  
  param <- cbind(Iteration = 1:iters, param)
  
  return(param)
  
}


# Comparaison with the SMD projected on the same l1 ball and same seed

start_time <- Sys.time()

set.seed(100)


paramSMDproj <- SMDproj(a = a,
                        b = b,
                        init = rep(0, 5),
                        iters = 1000,  # Play with the number of iterations
                        cost = hingereg,
                        instgrad = instgradreg,
                        lambda = 1,  # Play with the regularization parameter
                        z=.5) 
set.seed(100)

paramSEGpm <- SEGpm(a = a,
                  b = b,
                  iters = 1000,  # Play with the number of iterations
                  cost = hingereg,
                  instgrad = instgradreg,
                  lambda = 1,  # Play with the regularization parameter
                  z=.5) 
end_time <- Sys.time()

# Different trajectories 

plot.ts(paramSEGpm[, 2:(ncol(paramSEGpm) - 1)])

# Comparison SGD proj and SEGpm on 1000 iterations

plot(paramSMDproj$Iteration, paramSEGpm$Loss-paramSMDproj$Loss )


mean(paramSEGpm$Loss-paramSMDproj$Loss)
