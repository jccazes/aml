

#--------------------------------------------- Linear SVM ----------------------------------------------#

# Cost function: regularized hinge loss

insthingereg <- function(x, a, b,lambda){
  threshold <- (a %*% x) * b
  cost <- 1- (a  %*% x) * b
  cost[threshold >= 1] <- 0
  return(lambda*cost+t(x)%*%x/2)
}

# The instatenuous gradient

instgradreg <- function(x, a, b,lambda) {
  
  threshold <- b * (a %*% x) # define hard-margin SVM
  gradient <- - b* a
  gradient[threshold >= 1] <- 0
  return(as.numeric(lambda*gradient+x))
}  

 
# Batch Gradient Descent with learning rate 2/t

SGD <- function(a, b, init, iters = length(b), cost,  instgrad, lambda) {
  
  ind<-sample(1:length(b),iters,replace=TRUE)
  a<-a[ind,]
  b<-b[ind]
  
  param <- data.frame(matrix(nrow = iters, ncol = length(init) + 1))
  colnames(param) <- c(colnames(a), "Loss")
  param[1, ] <- c(c( init), cost( init, a[1,], b[1],lambda))

  
  for (i in 2:iters) {
    eta <- 1/sqrt(i)  # 1/i play with the learning rate
    x <- as.numeric(param[i-1, 1:length(init)])
    param[i, 1:length(init)] <- x - eta *( instgrad(x, a[i,], b[i], lambda)+x)
    param[i, length(init) + 1] <-  cost( colMeans(as.matrix(param[1:i, 1:length(init)])), a[i,], b[i],lambda)
  }
  
  cat("Final cost: ", sprintf("%10.07f", param[nrow(param), ncol(param)]), "\n", sep = "")
  cat("Parameters:", as.numeric(param[nrow(param), 1:length(init) ]), sep = " ")
  
  param <- cbind(Iteration = 1:nrow(param), param)
  
  return(param)
  
}



######################################### Model Building & Evaluation ######################################


# Explanatory variables and intercept in the design matrix
a <- data.frame(intercept= rep(1,100),
                a = rnorm(n = 100) * 5,
                b = rnorm(n = 100) * 3 + 1,
                c = rnorm(n = 100) * 2 + 2)

# Setting up a perfect relationship (linear problem) with +1 or -1 for classes
b <- 2 * as.integer(((a[, 2] * 2) + (a[, 3] * 3) + (a[, 4] * 4)) > 20) - 1



# Add column names and intercept
colnames(a) <- c("intercept", "a", "b", "c")
a <- as.matrix(a)

# Gradient Descent applied with inital value 0
start_time <- Sys.time()



paramSGD <- SGD(a = a,
            b = b,
            init = rep(0, 4),
            iters = 1000,  # Play with the number of iterations
            cost = hingereg,
            instgrad = instgradreg,
            lambda = 1) # Play with the regularization parameter

end_time <- Sys.time()
end_time - start_time


start_time <- Sys.time()
paramGD <- GD(a = a,
              b = b,
              init = rep(0, 4),
              iters = 1000,  # Play with the number of iterations
              cost = hingereg,
              grad = gradreg,
              lambda = 1) # Play with the regularization parameter
end_time <- Sys.time()
end_time - start_time

# Convergence of the algorithm

plot.ts(paramSGD[, 2:(ncol(paramSGD) - 1)])

# Rate of convergence in a log-log plot


regret <- paramSGD$Loss  # Normalized to be compared with GD rate
logregret <- log( regret ) 
plot(log(paramSGD$Iteration),log(paramGD$Loss)[-length(paramGD$Loss)],type="l")
lines(log(paramSGD$Iteration),logregret,col=4)
# Estimation of the rate of convergence

reg<-lm(logregret ~  log(paramSGD$Iteration))
abline(reg, col= "red")
legend("topright",  legend =as.numeric(reg$coefficients[2]), title= "rate")  

# Comparison GD and SGD on 1000 iterations

plot.ts(paramSGD$Iteration, paramSGD$Loss-paramGD$Loss[-length(paramGD$Loss)] )

mean(paramSGD$Loss-paramGD$Loss[-length(paramGD$Loss)])

# Comparison GD and SGD on 1000 gradients evaluations

plot.ts(paramSGD$Iteration,paramSGD$Loss-rep(paramGD$Loss[1:10],each =100))

mean(paramSGD$Loss-rep(paramGD$Loss[1:10],each =100))


