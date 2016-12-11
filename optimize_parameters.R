optimize_k_isomap <- function(distances, ndim = 2, test_k, dataname){
  dir.create(file.path("plots", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
    
  foreach(i= test_k) %dopar% {
      require(vegan)
      x_iso <- isomap(distances, ndim=2, k=i, fragmentedOK = T)
      png(file = file.path("plots", paste("test_isomap_", dataname, sep=''), paste("k", i, ".png", sep="")),  bg = "transparent")
      barplot(x_iso$eig[1:10], main = paste("Inertie = ", sum(abs(x_iso$eig[1:2]))/sum(abs(x_iso$eig)), sep=""))
      dev.off()
    }
}

optimize_k_lle <- function(data, ndim = 2, test_k, dataname){
  dir.create(file.path("plots", paste("test_lle_", dataname, sep='')), showWarnings = FALSE)
    
  foreach(i= test_k) %dopar% {
      require(lle)
      lle <- lle(data, ndim=2, k=i)
      png(file = file.path("plots", paste("test_lle_", dataname, sep=''), paste("k", i, ".png", sep="")),  bg = "transparent")
      # TODO : find a way to know what is the differenes between the ks. Maybe (plot_lle(lle.k$Y, lle.k$X))
      dev.off()
    }
}

