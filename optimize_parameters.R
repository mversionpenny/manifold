optimize_k_isomap <- function(ndim, test_k){
    foreach(i= test_k) %dopar% {
      x_iso <- isomap(distances, ndim=2, k=i)
      png(file = file.path("plots", "plot_test_isomap", paste("k", i, ".png", sep="")),  bg = "transparent")
      barplot(x_iso$eig[1:10], main = paste("Inertie = ",sum(x_iso$eig[1:2])/sum(x_iso$eig), sep=""))
      dev.off()
    }
}

# using 4 cores :
require(doParallel)
require(foreach)
cl <- makeCluster(4)
clusterExport(cl)

test1 = c(10, 15, 25, 50) #margot
test2 = c(5, 20, 30, 40) #thu

optimize_k_isomap(2, test2)
stopCluster(cl)
