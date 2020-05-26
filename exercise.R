x <- (1:20)
y = c(1:20,19:1)
assign('tmp', c(4,6,3))
rep(tmp, 10)
rep(c(4,6,3), c(10,20,30))

tmp = seq(3,6, by = 0.1)
exp(tmp)*cos(tmp)

tmp = seq(10,100, by = 1)
sum((tmp)^3+4*(tmp)^2)

paste("label ", c(1:30))
paste("label", c(1:30))

paste("fn", c(1:30), sep = "")


set.seed (50)
xVec  <- sample (0:999 , 250,  replace=T)
yVec  <- sample (0:999 , 250,  replace=T)

yselect = yVec[yVec>600]

which(yVec >= 600)

A = matrix(c(-1,1,3,5,2,6,2,-1,-3),3,3)

A %*% A %*% A == matrix(0, 3,3)
A[,3] <- A[,2]+A[,3]

B = matrix(c(10,-10,-10),b = T, nc = 3, nr = 15 )
?matrix

t(B) %*% B

tmpfn = function(xVec){
  xVec ^ (1:length(xVec))
}

tmpfn2 = function(xVec){
  xVec^(1:length(xVec))/(1:length(xVec))
}

tmpfn = function(xvec){
  n = length(xvec)
  (xvec[1:(n-2)]+xvec[2:(n-1)]+xvec[3:n])/3
}