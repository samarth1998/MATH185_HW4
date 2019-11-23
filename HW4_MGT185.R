# Homework 4

#q1
standardize_X <- function(x) {
  mX <- matrix(colMeans(x), nr=dim(x)[1], nc=dim(x)[2], byrow = TRUE)
  x0 <- x-mX
  s2 <- sqrt(colMeans(x0^2))
  x0 <- as.matrix(x0) %*% diag(1/s2)
  output <- list(x0 = x0, diag = s2)
  return(output)
}

lad_gd <- function(x, y, eta=0.1) {
  n <- dim(x)[1]
  d <- dim(x)[2]
  standX <- standardize_X(x)
  x0 <- cbind(rep(1,n), standX$x0)
  b0 <- numeric((d+1))
  res0 <- y
  b1 <- rep(1, d+1)
  iter <- 0
  repeat {
    if(max(abs(b1-b0)) < 10^(-5) | iter > 20000)
      break
    b1 <- b0
    subgrad <- sign(res0)
    grad <- -t(x0)%*%as.matrix(subgrad/n)
    b0 <- b0 - eta *grad
    res0 <- y-x0%*%b0
    iter <- iter + 1
  }
    b0[-1] <- b0[-1] / standX$diag
    b0[1] <- b0[1] - sum(colMeans(x) * b0[-1])
    output <- list(beta=b0, res = res0, iter = iter)
    return(output)
}

#2
y__education = education$Y
x__education = cbind(education$X1_1,education$X2,education$X3)
lift = lad_gd(x__education,y__education)

