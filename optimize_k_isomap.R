# using 4 cores :
library(doParallel)
library(foreach)
cl <- makeCluster(4)
clusterExport(cl, )
# IMPORTANT!

test_k = c(13,20,30,50)
registerDoParallel(cl)
foreachLoopPar <- function(){
  foreach(i= test_k) %dopar% 
    x_iso <- isomap(distances, ndim=2, k=i, fragmentedOK =T)
    png(file = file.path("plot_test_iso", paste("k", i, ".png", sep="")),  bg = "transparent")
    barplot(x_iso$eig[1:10], main = sum(x_iso$eig[1:2])/sum(x_iso$eig))
    dev.off()
}