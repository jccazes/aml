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

# AdaGrad with projection

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
    x <- as.numeric(param[i-1, 1:length(init)])
    s <- s + (instgrad(x, a[i,], b[i], lambda)+x)^2
    y <- x - eta * 1/sqrt(s) * (instgrad(x, a[i,], b[i], lambda) + x) 
    param[i, 1:length(init)] <- pib1w(y,sqrt(s),z)
    param[i, length(init) + 1] <-  cost( colMeans(as.matrix(param[1:i, 1:length(init)])), a[i,], b[i],lambda)
  }
  
  cat("Final cost: ", sprintf("%10.07f", param[nrow(param), ncol(param)]), "\n", sep = "")
  cat("Parameters:", as.numeric(param[nrow(param), 1:length(init) ]), sep = " ")
  
  param <- cbind(Iteration = 1:nrow(param), param)
  
  return(param)
  
}





# Comparaison between AdaGrad and SMD projected

start_time <- Sys.time()

set.seed(1)   


paramAdaproj <- Adaproj(a = a,
                        b = b,
                        init = rep(0, 5),
                        iters = 1000,  # Play with the number of iterations
                        cost = hingereg,
                        instgrad = instgradreg,
                        lambda = 1,  # Play with the regularization parameter
                        z=.5) 
set.seed(1)   # Same seed!

paramSGDproj <- SGDproj(a = a,
                        b = b,
                        init = rep(0, 5),
                        iters = 1000,  # Play with the number of iterations
                        cost = hingereg,
                        instgrad = instgradreg,
                        lambda = 1,  # Play with the regularization parameter
                        z=.5) 
end_time <- Sys.time()

# Different trajectories 

dev.off()
plot.ts(paramAdaproj[, 2:(ncol(paramAdaproj) - 1)])
plot.ts(paramSGDproj[, 2:(ncol(paramSGDproj) - 1)])

# Comparison AdaGrad proj and SGD proj on 1000 iterations
dev.off()
plot.ts(paramAdaproj$Iteration, paramAdaproj$Loss-paramSGDproj$Loss )

mean(paramAdaproj$Loss-paramSGDproj$Loss)


