

#--------------------------------------------- Linear SVM ----------------------------------------------#

# Cost function: regularized hinge loss

hingereg <- function(x, a, b,lambda){
  threshold <- (a %*% x) * b
  cost <- 1- (a  %*% x) * b
  cost[threshold >= 1] <- 0
  return(lambda*mean(cost)+t(x)%*%x/2)
}

# The corresponding gradient

gradreg <- function(x, a, b,lambda) {
  
  threshold <- b * (a %*% x) # define hard-margin SVM
  gradient <- - b* a
  gradient[threshold >= 1] <- 0
  return(as.numeric(lambda*colMeans(gradient)+x))
}  

average <- function(x){
  2*colSums(x*(1:nrow(x)))/(nrow(x)*(nrow(x)+1))
}   
# Batch Gradient Descent with learning rate 2/t

GD <- function(a, b, init,iters, cost,  grad, lambda) {
  
  param <- data.frame(matrix(nrow = iters + 1, ncol = length(init) + 1))
  colnames(param) <- c(colnames(a), "Loss")
  param[1, ] <- c(c( init), cost( init, a, b,lambda))
  
  for (i in 1:iters) {
    eta <- 2/(i+1)   # 2/sqrt(i+1) play with the learning rate
    param[i + 1, 1:length(init)] <- as.numeric(param[i, 1:length(init)]) - eta * grad(as.numeric(param[i, 1:length(init)]), as.matrix(a), b, lambda)
    param[i + 1, length(init) + 1] <- cost(as.numeric(param[i+1, 1:length(init)]), as.matrix(a), b,lambda)
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
paramGD <- GD(a = a,
          b = b,
          init = rep(0, 4),
          iters = 100,  # Play with the number of iterations
          cost = hingereg,
          grad = gradreg,
          lambda = 1) # Play with the regularization parameter
end_time <- Sys.time()
end_time - start_time
# Convergence of the algorithm

plot.ts(paramGD[, 2:(ncol(paramGD) - 1)])

# Rate of convergence in a log-log plot

regret <- paramGD$Loss-min(paramGD$Loss)
logregret <- log(pmax(regret, min(regret[regret>0]))) 
plot(log(paramGD$Iteration),logregret,type="l")

# Estimation of the rate of convergence

reg<-lm(logregret~  log(paramGD$Iteration))
abline(reg, col= "red")
legend("topright",  legend =as.numeric(reg$coefficients[2]), title= "rate")

