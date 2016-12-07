optimize_k_isomap <- function(distances, ndim, test_k){
    foreach(i= test_k) %dopar% {
      x_iso <- isomap(distances, ndim=2, k=i)
      png(file = file.path("plots", "test_isomap", paste("k", i, ".png", sep="")),  bg = "transparent")
      barplot(x_iso$eig[1:10], main = paste("Inertie = ", sum(x_iso$eig[1:2])/sum(x_iso$eig), sep=""))
      dev.off()
    }
}
