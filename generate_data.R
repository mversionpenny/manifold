#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
# Functions to generate different types of datasets
list.of.packages <- c("rgl", "plot3D")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rgl)
library(plot3D)
plot.path = "plots\\artificial_data"

#### Swiss ROll ####
swissroll <- function(n = 5000, noise = 0.05, plot.3D = T, plot.file = NULL){
  p <- sort(runif(n))
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  X <- cbind(t*cos(t), t*sin(t), 30*q) + noise * rnorm(n)
  
  if (!is.null(plot.file))
    png(file = plot.file,  bg = "transparent")
  scatter3D(X[,3],X[,1],X[,2], phi= 15, theta=60, colvar = t, colkey=F, pch=16, 
            bty = "b2", ticktype = "detailed", scale = F,
            xlim = c(-5,35), ylim = c(-15,15), zlim = c(-10,15), xlab= "", ylab="", zlab="")
  dev.off()
  
  if (plot.3D)
    plot3d(X[,3],X[,1],X[,2], type="p",aspect =TRUE, col=jet.col(n),
           xlab="", ylab="", zlab="", expand = 1.5, box=F, bty="b2", size = 7)
  
  return(X)
}
#swiss <- swissroll(plot.3D = F, plot.file = file.path(plot.path, "swiss.png"))
#write.table(swiss,file = "data\\swissRoll.txt", sep = "\t", col.names = F, row.names = F)


#### Broken Swiss Roll ####
#colors is correcly broken too
brokenSwissroll <- function(n = 5000, noise = 0.05, n.break = 500, plot.3D = T, plot.file = NULL){
  p <- sort(runif(n+n.break))
  q <- runif(n)
  t <- 3*pi/2 * (1 + 2*p)
  colors <- jet.col(n+n.break)
  
  begin.break <- which(t>8)[1]
  t <- t[-c(begin.break:(begin.break+n.break-1))]
  colors <- colors[-c(begin.break:(begin.break+n.break-1))]
  X <- cbind(t*cos(t), t*sin(t), 30*q) + noise * rnorm(n)
  
  if (!is.null(plot.file))
    png(file = plot.file,  bg = "transparent")
  scatter3D(X[,3],X[,1],X[,2], phi= 15, theta=60, colvar = t, colkey=F, pch=16, 
            bty = "b2", ticktype = "detailed", scale = F,
            xlim = c(-5,35), ylim = c(-15,15), zlim = c(-10,15), xlab= "", ylab="", zlab="")
  dev.off()
  
  if (plot.3D)
    plot3d(X[,3],X[,1],X[,2], type="p",aspect =TRUE, col=jet.col(n),
           xlab="", ylab="", zlab="", expand = 1.5, box=F, bty="b2", size = 7)
  
  return(X)
}
#brokenswiss <- brokenSwissroll(plot.3D = F,
#                               plot.file = file.path(plot.path,
#                                                     "brokenSwissRoll.png"))
#write.table(brokenswiss,file = "data\\brokenSwissRoll.txt", sep = "\t",
#            col.names = F, row.names = F)

#### Helix ####
helix <- function(n = 5000, noise = 0.05, plot.3D=T, plot.file = NULL){
  t <- (1:n)/n
  t <- t * 2 * pi
  X <- cbind((2+cos(8*t))*cos(t), (2+cos(8*t))*sin(t), sin(8*t)) + noise * rnorm(n)
  
  if (!is.null(plot.file))
    png(file = plot.file,  bg = "transparent")
  scatter3D(X[,1],X[,2],X[,3], phi= 15, theta=60, colvar = t, colkey=F, pch=20, 
            bty = "b2", ticktype = "detailed", scale = F,
            xlim = c(-3, 3), ylim = c(-3,3), zlim = c(-1.5,1.5), xlab= "", ylab="", zlab="")
  dev.off()
  
  if (plot.3D)
    plot3d(X[,1],X[,2],X[,3], type="p",aspect=c(2,1,1), col=jet.col(n),
           xlab="", ylab="", zlab="", expand = 1.5, box=F, bty="b2", size = 7)

  return(X)
}
# helix.data <- helix(plot.3D = F,
#                               plot.file = file.path(plot.path,
#                                                     "helix.png"))
# write.table(helix.data,file = "data\\helix.txt", sep = "\t",
#            col.names = F, row.names = F)

#### Twinpeaks ####
twinpeaks <- function(n = 5000, noise= 0.02, plot.3D=T, plot.file = NULL){
  p <- runif(n)
  q <- runif(n)
  X <- cbind(1-2*p, 1-2*q, sin(pi - 2*pi*p) * tanh(3 - 6*q)) + noise * rnorm(n)
  X<- X[order(X[,3]),]
  
  if (!is.null(plot.file))
    png(file = plot.file,  bg = "transparent")
  scatter3D(X[,1],X[,2],X[,3], phi= 35, theta= -35, colvar = X[,3], colkey=F, pch=16, 
            bty = "b2", ticktype = "detailed", scale = F,
            xlim = c(-1.2,1.2), ylim = c(-1.2,1.2), zlim = c(-1.2,1.2), xlab= "", ylab="", zlab="")
  dev.off()
  
  if (plot.3D)
    plot3d(X[,1],X[,2],X[,3], type="p",aspect=c(1,1,1), col=jet.col(n),
           xlab="", ylab="", zlab="", expand = 1.5, box=F, bty="b2", size = 7)
  
  return(X)
}
# twins <- twinpeaks(plot.3D = F,
#                     plot.file = file.path(plot.path,
#                                           "twinpeaks.png"))
# write.table(twins,file = "data\\twinpeaks.txt", sep = "\t",
#             col.names = F, row.names = F)

#### HD ####

HD <-function(n = 5000){
  ndims = 5
  nb_per_dim = round(n^(1/ndims))
  l <- list(seq(0,1,length=nb_per_dim))
  temp <- rep(t(l),ndims)
  t <- expand.grid(temp)
  X <- cbind(cos(t[,1]), tanh(3 * t[,2]), t[,1] + t[,3], t[,4] * sin(t[,2]), sin(t[,1] + t[,5]), t[,5] * cos(t[,2]), t[,5] + t[,4], t[,2], t[,3] * t[,4], t[,1])
  return(X)
}

#hd <- HD()

#### Open box ####

openbox <- function(n = 5000, noise = 0.02, plot.3D = T, plot.file = NULL){
  nb_per_side = n %/% 6
  nb_in_base = n - nb_per_side*5
  #base
  base <- replicate(2, runif(nb_in_base,-5,5))
  base <- cbind(base,rep(0,nb_in_base))
  #top
  top <- cbind(runif(nb_per_side,5,15), runif(nb_per_side,-5,5), rep(10,nb_per_side))
  #sides
  s1 <- cbind(runif(nb_per_side,-5,5), rep(-5, nb_per_side), runif(nb_per_side, 0, 10))
  s2 <- cbind(runif(nb_per_side,-5,5), rep(5, nb_per_side), runif(nb_per_side, 0, 10))
  s3 <- cbind(rep(-5, nb_per_side), runif(nb_per_side,-5,5),  runif(nb_per_side, 0, 10))
  s4 <-  cbind(rep(5, nb_per_side), runif(nb_per_side,-5,5),  runif(nb_per_side, 0, 10))
  X <- rbind(base,s1,s2,s3,s4,top) + noise*rnorm(n)
  X <- X[order(X[,3]), ]
  
  if (!is.null(plot.file))
    png(file = plot.file,  bg = "transparent")
  scatter3D(X[,1],X[,2],X[,3], phi= 35, theta= -35, colvar = X[,3], colkey=F, pch=16, 
            bty = "b2", ticktype = "detailed", scale = F,
            xlim = c(-7, 17), ylim = c(-7,7), zlim = c(-2,12), xlab= "", ylab="", zlab="")
  dev.off()
  
  if (plot.3D)
    plot3d(X[,1],X[,2],X[,3], type="p",aspect=c(2,1,1), col=jet.col(n),
           xlab="", ylab="", zlab="", expand = 1.5, box=F, bty="b2", pch = 16, size = 7)
   
  return(X)
}
# open <- openbox(plot.3D = F,
#                     plot.file = file.path(plot.path,
#                                           "openBox.png"))
# write.table(open,file = "data\\openBox.txt", sep = "\t",
#             col.names = F, row.names = F)
