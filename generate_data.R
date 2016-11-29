#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------

library(rgl)
library(plot3D)
observationsNum <- 5000
noise <- 0.05

#### Swiss ROll ####
swissroll <- function(n,noise){
  p <- sort(runif(n))
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  X <- cbind(t*cos(t), t*sin(t), 30*q) + noise * rnorm(n)

  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  scatter3D(X[,1],X[,2],X[,3], phi= -60, theta=90, colvar = t, colkey=F, pch=20, cex=0.75)
  scatter3D(X[,1],X[,2],X[,3], phi= 40, theta=90, colvar = t, colkey=F, pch=20, cex=0.75)  
  mtext("Simulation of a Swiss Roll dataset", outer = TRUE, cex = 1.5)
  plot3d(X[,1],X[,2],X[,3], type="p",aspect =TRUE, col=jet.col(n), main = "Simulation of a Swiss Roll dataset")
  return(X)
}
#swiss <- swissroll(observationsNum,noise)


#### Broken Swiss Roll ####
brokenSwissroll <- function(n, noise, n.break){
  p <- sort(runif(n+n.break))
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  colors <- jet.col(n+n.break)
  
  begin.break <- which(t>8)[1]
  t <- t[-c(begin.break:(begin.break+n.break-1))]
  colors <- colors[-c(begin.break:(begin.break+n.break-1))]
  X <- cbind(t*cos(t), t*sin(t), 30*q) + noise * rnorm(n)
  
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  scatter3D(X[,1],X[,2],X[,3], phi= -60, theta=90, colvar = t, colkey=F, pch=20, cex=0.75, col=colors)
  scatter3D(X[,1],X[,2],X[,3], phi= 40, theta=90, colvar = t, colkey=F, pch=20, cex=0.75, col=colors) 
  mtext("Simulation of a broken Swiss Roll dataset", outer = TRUE, cex = 1.5)
  plot3d(X[,1],X[,2],X[,3], type="p",aspect =TRUE, col=colors , main = "Simulation of a broken Swiss Roll dataset")
  return(X)
}
#broken.swiss <- brokenSwissroll(observationsNum,noise, 500)

#### Helix ####
#colors is correcly broken too
helix <- function(n, noise){
  t <- (1:n)/n
  t <- t * 2 * pi
  X <- cbind((2+cos(8*t))*cos(t), (2+cos(8*t))*sin(t), sin(8*t)) + noise * rnorm(n)
  
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  scatter3D(X[,1],X[,2],X[,3], phi= -60, theta=90, colvar = t, colkey=F, pch=20, cex=0.75)
  scatter3D(X[,1],X[,2],X[,3], phi= 40, theta=90, colvar = t, colkey=F, pch=20, cex=0.75)  
  mtext("Simulation of a helix dataset", outer = TRUE, cex = 1.5)
  plot3d(X[,1],X[,2],X[,3], type="p",aspect =TRUE, col=jet.col(n), main = "Simulation of a helix dataset")
  return(X)
}
#helix.data <- helix(observationsNum,noise)

#### Twinpeaks ####
twinpeaks <- function(n, noise){
  p <- runif(n)
  q <- runif(n)
  X <- cbind(1-2*p, 1-2*q, sin(pi - 2*pi*p) * tanh(3 - 6*q)) + noise * rnorm(n)
  X<- X[order(X[,3]),]
  
  par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  scatter3D(X[,1],X[,2],X[,3], phi= 30, theta=-80, colvar = X[,3], colkey=F, pch=20, cex=0.65)
  scatter3D(X[,1],X[,2],X[,3], phi= -30, theta=45, colvar = X[,3], colkey=F, pch=20, cex=0.65) 
  mtext("Simulation of a twinpeaks dataset", outer = TRUE, cex = 1.5)
  plot3d(X[,1],X[,2],X[,3], type="p",aspect =TRUE, col=jet.col(n), main = "Simulation of a twinpeaks dataset")
  return(X)
}
#twins <- twinpeaks(observationsNum,noise)

#### HD ####

HD <-function(n){
  ndims = 5
  nb_per_dim = round(n^(1/ndims))
  l <- list(seq(0,1,length=nb_per_dim))
  temp <- rep(t(l),ndims)
  t <- expand.grid(temp)
  X <- cbind(cos(t[,1]), tanh(3 * t[,2]), t[,1] + t[,3], t[,4] * sin(t[,2]), sin(t[,1] + t[,5]), t[,5] * cos(t[,2]), t[,5] + t[,4], t[,2], t[,3] * t[,4], t[,1])
  return(X)
}

newHD <-function(n){
  ndims = 5
  t <- matrix(runif(n*ndims), ncol=ndims)
  # ou trouver les fonctions de combinaison? 
  X <- cbind(cos(t[,1]), tanh(3 * t[,2]), t[,1] + t[,3], t[,4] * sin(t[,2]), sin(t[,1] + t[,5]), t[,5] * cos(t[,2]), t[,5] + t[,4], t[,2], t[,3] * t[,4], t[,1])
  return(X)
}

#hd <- HD(observationsNum)

