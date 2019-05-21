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

SREGpm <- function(a, b, iters = length(b), cost,  instgrad, lambda,z) {
  
  ind<-sample(1:length(b),iters,replace=TRUE)
  a<-a[ind,]
  b<-b[ind]
  
  param <- data.frame(matrix(nrow = iters, ncol = ncol(a) + 1))
  colnames(param) <- c(colnames(a), "Loss")
  w <- rep(1/(2*ncol(a)),2*ncol(a))
  param[1, 1:ncol(a)] <- z*c(w[1:ncol(a)]-w[ncol(a)+1:ncol(a)])
  param[1, ncol(a)+1] <-  cost(as.numeric(param[1, 1:ncol(a)]), a[1,], b[1],lambda)
  eta <-  z*sqrt(log(ncol(a))/(iters*ncol(a)))  #   play with the learning rate
  
  for (i in 2:iters) {
    J <- sample(1:(2*ncol(a)),1,prob=w)-1
    j <- J %% ncol(a) + 1
    pm <- 2* (J<ncol(a)) -1
    x <- as.numeric(param[i-1, j])
    instgj <- instgrad(x, a[i,j], b[i], lambda) + x
    gradest <-c(rep(0,J), pm *instgj  / w[J+1], rep(0,2*ncol(a)-J-1))
    W <-  pmax(pmin(exp(700),exp(-eta * gradest)),exp(-700))*w
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

set.seed(10)

paramSEGpm <- SEGpm(a = a,
                    b = b,
                    iters = 1000,  # Play with the number of iterations
                    cost = hingereg,
                    instgrad = instgradreg,
                    lambda = 1,  # Play with the regularization parameter
                    z=.5) 
set.seed(10)

paramSREGpm <- SREGpm(a = a,
                    b = b,
                    iters = 1000,  # Play with the number of iterations
                    cost = hingereg,
                    instgrad = instgradreg,
                    lambda = 1,  # Play with the regularization parameter
                    z=.5) 
end_time <- Sys.time()

# Unstable trajectories 

plot.ts(paramSREGpm[, 2:(ncol(paramSMDproj) - 1)])
plot.ts(cumsum(paramSREGpm$Loss)/1:length(paramSREGpm$Loss))

# Comparison SGD proj and SEGpm on 1000 iterations

plot(paramSMDproj$Iteration,paramSREGpm$Loss-paramSEGpm$Loss)

mean(paramSREGpm$Loss-paramSEGpm$Loss)

# Comparison GD and SGD on 200 gradients evaluations

plot(paramSEGpm$Iteration,paramSREGpm$Loss-rep(paramSEGpm$Loss[1:200],each =5))

mean(paramSREGpm$Loss-rep(paramSEGpm$Loss[1:200],each =5))

