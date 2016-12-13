#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
# Optimize parameters for dimension reduction methods. Result objects after each
# test is saved in a Rdata file to save time of recalculation later. 
# The function calling lines are put in comment to prevent the long calculation
# to be repeated

list.of.packages <- c("doParallel", "foreach", "vegan", "lle" , "MASS", "parallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(lle)
library(doParallel)
library(foreach)
library(MASS)
library(parallel)

#### Optimize parameter for Isomap ####
## Parallel:
optimize_k_isomap <- function(distances, ndim = 2, test_k, dataname){
  dir.create(file.path("plots", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
  dir.create(file.path("data", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
    
  foreach(i= test_k) %dopar% {
      require(vegan)
      x_iso <- isomap(distances, ndim=2, k=i)
      save(x_iso, file = file.path("data", paste("test_isomap_", dataname, sep=''), paste("k", i, ".RData", sep="")))
      png(file = file.path("plots", paste("test_isomap_", dataname, sep=''), paste("k", i, ".png", sep="")),  bg = "transparent")
      barplot(x_iso$eig[1:10], main = paste("Inertie = ", sum(abs(x_iso$eig[1:2]))/sum(abs(x_iso$eig)), sep=""))
      dev.off()
    }
}

# test <- c(5,10,15,20,25,30,35)
# test <- c(6,7,8,9,11,12,13,14)
# nb_cores <- 7
# cl <- makeCluster(nb_cores)
# clusterExport(cl, list())
# registerDoParallel(nb_cores)
# 
# optimize_k_isomap(d_swiss, 2, test, "swissRoll")
# optimize_k_isomap(d_helix, 2, test, "helix")
# optimize_k_isomap(d_twins, 2, test, "twinpeaks")
# optimize_k_isomap(d_open, 2, test, "openBox")
# 
# stopCluster(cl)

## Sequential:
# for higher k
optimize_k_isomap_seq <- function(distances, ndim = 2, test_k, dataname){
  dir.create(file.path("plots", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
  dir.create(file.path("data", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
  
  for (i in test_k){
    require(vegan)
    print(i)
    x_iso <- isomap(distances, ndim=2, k=i)
    save(x_iso, file = file.path("data", paste("test_isomap_", dataname, sep=''), paste("k", i, ".RData", sep="")))
    png(file = file.path("plots", paste("test_isomap_", dataname, sep=''), paste("k", i, ".png", sep="")),  bg = "transparent")
    barplot(x_iso$eig[1:10], main = paste("Inertie = ", sum(abs(x_iso$eig[1:2]))/sum(abs(x_iso$eig)), sep=""))
    dev.off()
  }
}

# optimize_k_isomap_seq(d_broken, 2, seq(65,85,10) , "brokenSwissRoll")
# test <- c(35,seq(45,80,5))
# optimize_k_isomap_seq(d_swiss, 2, test , "swissRoll")
# optimize_k_isomap_seq(d_helix, 2, test , "helix")
# optimize_k_isomap_seq(d_twins, 2, test, "twinpeaks")
# optimize_k_isomap_seq(d_open, 2, test , "openBox")

#### Optimize parameter for LLE ####
## Parallel:
optimize_k_lle <- function(data, ndim = 2, kmin, kmax, step=1, dataname, nb_cores){
  if (step == 1) {
    res <- calc_k(data, ndim, kmin = kmin, kmax = kmax, plotres = F, parallel = T, cpus = nb_cores)
  }
  else{
    test_k <- seq(kmin,kmax,step)
    cl <- makeCluster(nb_cores)
    clusterExport(cl, list())
    registerDoParallel(nb_cores)
    res <- foreach(i= test_k,  .combine='rbind') %dopar% {
      require(lle)
      calc_k(data, ndim, kmin = i, kmax = i, plotres = F)
    }
    stopCluster(cl)
  }
  write.table(res, file = file.path("data", "test_lle", paste(dataname, ".txt", sep="")), quote = F, col.names = F, row.names = F, append = T)
}

## Firstly, we test 5<=k<=15 as in the article:
# dir.create(file.path("data", "test_lle"), showWarnings = FALSE)
nb_cores <- 7
# optimize_k_lle(swissRoll, kmin = 5, kmax = 15, step = 1, dataname = "swissRoll", nb_cores = nb_cores)
# optimize_k_lle(helix, ndim=1, kmin = 5, kmax = 15, step = 1, dataname = "helix", nb_cores = nb_cores)
# optimize_k_lle(twinpeaks, ndim=1, kmin = 5, kmax = 15, step = 1, dataname = "twinpeaks", nb_cores = nb_cores)
# optimize_k_lle(openBox, ndim=1, kmin = 5, kmax = 15, step = 1, dataname = "openBox", nb_cores = nb_cores)
# optimize_k_lle(brokenSwissRoll, kmin = 5, kmax = 15, step = 1, dataname = "brokenSwissRoll", nb_cores = nb_cores)

nb_cores <- 4
# optimize_k_lle(swissRoll, kmin = 20, kmax = 35, step = 5, dataname = "swissRoll", nb_cores = nb_cores)
# optimize_k_lle(helix, ndim=1, kmin = 20, kmax = 35, step = 5, dataname = "helix", nb_cores = nb_cores)
# optimize_k_lle(twinpeaks, ndim=1, kmin = 20, kmax = 35, step = 5, dataname = "twinpeaks", nb_cores = nb_cores)
# optimize_k_lle(openBox, ndim=1, kmin = 20, kmax = 35, step = 5, dataname = "openBox", nb_cores = nb_cores)
# optimize_k_lle(brokenSwissRoll, kmin = 20, kmax = 35, step = 5, dataname = "brokenSwissRoll", nb_cores = nb_cores)

nb_cores <- 3
# optimize_k_lle(swissRoll, kmin = 40, kmax = 80, step = 5, dataname = "swissRoll", nb_cores = nb_cores)
# optimize_k_lle(helix, ndim=1, kmin = 40, kmax = 80, step = 5, dataname = "helix", nb_cores = nb_cores)
# optimize_k_lle(twinpeaks, ndim=1, kmin = 40, kmax = 80, step = 5, dataname = "twinpeaks", nb_cores = nb_cores)
# optimize_k_lle(openBox, ndim=1, kmin = 40, kmax = 80, step = 5, dataname = "openBox", nb_cores = nb_cores)
# optimize_k_lle(brokenSwissRoll, kmin = 40, kmax = 80, step = 5, dataname = "brokenSwissRoll", nb_cores = nb_cores)

## Run LLE with optimal k (in general) and optimal k in [5,15]
dir.create(file.path("data", "lle"), showWarnings = FALSE)
data_name <- c("swissRoll", "helix", "twinpeaks", "brokenSwissRoll", "openBox")
for (name in data_name){
  test_k <- read.table(file.path("data", "test_lle", paste(name, ".txt", sep="")), header = T)
  k_opt <- test_k$k[which(test_k$rho == min(test_k$rho))]
  if (name == "helix")  m = 1
  else m = 2
  x_lle <- lle(get(name), m, k_opt)
  save(x_lle, file = file.path("data", "lle", paste(dataname, "_k", k_opt, ".RData", sep="")))
  if (k_opt>15){
    k_opt <- test_k$k[which(test_k$rho == min(test_k$rho[1:11]))]
    x_lle <- lle(get(name), m, k_opt)
    save(x_lle, file = file.path("data", "lle", paste(dataname, "_k", k_opt, ".RData", sep="")))
  }
}


#### Optimize parameter for Sammon ####
## No parameter to optimize, simply use the method for each data set and save the result
# dir.create(file.path("data", "sammon"), showWarnings = FALSE)
# sammon_swiss <- sammon(d_swiss)
# save(sammon_swiss, file = file.path("data", "sammon", "swissRoll.RData"))
# # 20
# sammon_broken <- sammon(d_broken)
# save(sammon_broken, file = file.path("data", "sammon", "brokenSwissRoll.RData"))
# #17
# sammon_helix <- sammon(d_helix)
# save(sammon_helix, file = file.path("data", "sammon", "helix.RData"))
# #0
# sammon_twins <- sammon(d_twins)
# save(sammon_twins, file = file.path("data", "sammon", "twinpeaks.RData"))
# #0
# sammon_open <- sammon(d_open)
# save(sammon_open, file = file.path("data", "sammon", "openBox.RData"))
# #2