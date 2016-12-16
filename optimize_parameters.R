#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
#                                                        *
#     *                                                          *
#                                  *                  *        .--.
#      \/ \/  \/  \/                                        ./   /=*
#        \/     \/      *            *                ...  (_____)
#         \ ^ ^/                                       \ \_((^o^))-.    *
#         (o)(O)--)--------\.                           \   (   ) \ \._.
#         |    |  ||================((~~~~~~~~~~~~~~~~~))|   ( )   |    \
#          \__/             ,|        \. * * * * * * ./  (~~~~~~~~~~)    \
#   *        ||^||\.____./|| |          \___________/     ~||~~~~|~'\____/ *
#            || ||     || || A            ||    ||         ||    |   
#     *      <> <>     <> <>          (___||____||_____)  ((~~~~~|   *

# Optimize parameters for dimension reduction methods. Result objects after each
# test is saved in a Rdata file to save time of recalculation later. 
# The function calling lines are put in comment to prevent the long calculation
# to be repeated
library(lle)
library(doParallel)
library(foreach)
library(MASS)
library(parallel)


# +-------------------------------------------------------------------------+
# | *Function : optimize_k_isomap                                           |
# | *Description: perform the isomap algorithm for every k neighbourhood    |
# |  size. Parallel computation is used for smaller calculation time. If    |
# |  the testing k is large, optimize_k_isomap_seq is recommended.          |
# |  The isomap result objects for all the k tested are saved in            |
# |  data/test_isomap_[dataname]. For each k, a barplot of eigenvalues      |
# |  is also saved in plots/test_isomap_[dataname]                          |
# | *Inputs: - distance: distance structure of the form returned by dist    |
# |          - ndim: intrinsic dimension of the data                        |
# |          - test_k: vector containts all the k that need to be tested    |
# |          - dataname: name of the data (to save). Should be the same as  |
# |            the name of the matrix of input data that is used to compute |
# |            the 'distance'.                                              |
# |          - nb_cores: number of cpus will be used for parallel           |
# |            computation                                                  |
# | *Outputs: no output                                                     |
# +-------------------------------------------------------------------------+
optimize_k_isomap <- function(distances, ndim = 2, test_k, dataname, nb_cores){
  dir.create(file.path("plots", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
  dir.create(file.path("data", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)

  cl <- makeCluster(nb_cores)
  clusterExport(cl, list())
  registerDoParallel(nb_cores)
  foreach(i= test_k) %dopar% {
      require(vegan)
      x_iso <- isomap(distances, ndim=ndim, k=i)
      save(x_iso, file = file.path("data", paste("test_isomap_", dataname, sep=''), paste("k", i, ".RData", sep="")))
      png(file = file.path("plots", paste("test_isomap_", dataname, sep=''), paste("k", i, ".png", sep="")),  bg = "transparent")
      barplot(x_iso$eig[1:10], main = paste("Inertie = ", sum(abs(x_iso$eig[1:i]))/sum(abs(x_iso$eig)), sep=""))
      dev.off()
      
  stopCluster(cl)
    }
}

# *Function : optimize_k_isomap_seq                              
# *Description: perform the isomap algorithm for every k neighbourhood size. This function should be used for larger k 
# or when the machine has low memory. The isomap result objects for all the k tested are saved in data/test_isomap_[dataname].
# For each k, a barplot of eigenvalues is also saved in plots/test_isomap_[dataname]
# *Inputs: 
# distance: distance structure of the form returned by dist
# ndim: intrinsic dimension of the data
# test_k: vector containts all the k that need to be tested
# dataname: name of the data (to save). Should be the same as the name of the matrix of input data that is used to compute the 'distance'
# *Outputs: no output  
optimize_k_isomap_seq <- function(distances, ndim = 2, test_k, dataname){
  dir.create(file.path("plots", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
  dir.create(file.path("data", paste("test_isomap_", dataname, sep='')), showWarnings = FALSE)
  
  for (i in test_k){
    require(vegan)
    print(i)
    x_iso <- isomap(distances, ndim=ndim, k=i)
    save(x_iso, file = file.path("data", paste("test_isomap_", dataname, sep=''), paste("k", i, ".RData", sep="")))
    png(file = file.path("plots", paste("test_isomap_", dataname, sep=''), paste("k", i, ".png", sep="")),  bg = "transparent")
    barplot(x_iso$eig[1:10], main = paste("Inertie = ", sum(abs(x_iso$eig[1:i]))/sum(abs(x_iso$eig)), sep=""))
    dev.off()
  }
}



# *Function : optimize_k_lle                              
# *Description: perform the LLE algorithm for every k neighbourhood size using calc_k. Parallel computation is used for smaller 
# calculation time. For each k , a value rho will be computed. The k with the lowest rho is the optimal. 
# The table with all k and rho correspondings is saved as 'data/test_lle/[dataname].txt'

# *Inputs: 
# data: matrix object containing the input data
# ndim: intrinsic dimension of the data
# kmin: minimum value of k
# kmax: maximum value of k
# step: increment of the sequence of k
# dataname: name of the data (to save). Should be the same as the name of the matrix of input data.
# nb_cores: number of cpus to be used in parallel computation
# *Outputs: no output  
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
  write.table(res, file = file.path("data", "test_lle", paste(dataname, ".txt", sep="")), 
              quote = F, col.names = F, row.names = F, append = T)
}

# *Function : lle_k_opt_artificial                             
# *Description: perform the LLE algorithm on the artificial datasets with the global optimal k and the optimal k between 5 and 15 based on the result of optimize_k_lle.
# ATTENTION: This function will fail if the dataname used in optimize_k_lle is not the same as the name of input data. The result objects
# are saved as 'data/lle/[dataname].RData'
# *Inputs: 
# dataname: vector containing all the names of input datasets
# ndims: vector containing the intrinsic dimensions of the datasets
# *Outputs: no output  
lle_k_opt_artificial <- function(dataname, ndims){
  dir.create(file.path("data", "lle"), showWarnings = FALSE)
  for (i in 1:length(dataname)){
    name = dataname[i]
    m = ndims[i]
    test_k <- read.table(file.path("data", "test_lle", paste(name, ".txt", sep="")), header = F)
    k_opt <- test_k[,1][which(test_k[,2] == min(test_k[,2]))]
    x_lle <- lle(get(name), m, k_opt)
    save(x_lle, file = file.path("data", "lle", paste(name, "_k", k_opt, ".RData", sep="")))
    if (k_opt>15){
      k_opt <- test_k[,1][which(test_k[,2] == min(test_k[1:11,2]))]
      x_lle <- lle(get(name), m, k_opt)
      save(x_lle, file = file.path("data", "lle", paste(name, "_k", k_opt, ".RData", sep="")))
    }
  }
}

# *Function : lle_k_opt_real                             
# *Description: perform the LLE algorithm on the real datasets with the optimal k.
# ATTENTION: This function will fail if the dataname used in optimize_k_lle is not the same as the name of input data. The result objects
# are saved as 'data/lle/[dataname].RData'
# *Inputs: 
# dataname: vector containing all the names of input datasets
# ndims: vector containing the intrinsic dimensions of the datasets
# *Outputs: no output  
lle_k_opt_real <- function(dataname, ndims){
  dir.create(file.path("data", "lle"), showWarnings = FALSE)
  for (i in 1:length(dataname)){
    name = dataname[i]
    m = ndims[i]
    test_k <- read.table(file.path("data", "test_lle", paste(name, ".txt", sep="")), header = F)
    k_opt <- test_k[,1][which(test_k[,2] == min(test_k[,2]))]
    x_lle <- lle(get(name), m, k_opt)
    save(x_lle, file = file.path("data", "lle", paste(name, "_k", k_opt, ".RData", sep="")))
  }
}

#####################################################

######################################################

#### Optimize parameter for Sammon ####
## No parameter to optimize, simply use the method for each data set and save the result
# dir.create(file.path("data", "sammon"), showWarnings = FALSE)
# sammon_swiss <- sammon(d_swiss)
# save(sammon_swiss, file = file.path("data", "sammon", "swissRoll.RData"))

# sammon_broken <- sammon(d_broken)
# save(sammon_broken, file = file.path("data", "sammon", "brokenSwissRoll.RData"))

# sammon_helix <- sammon(d_helix, k= 1)
# save(sammon_helix, file = file.path("data", "sammon", "helix.RData"))

# sammon_twins <- sammon(d_twins)
# save(sammon_twins, file = file.path("data", "sammon", "twinpeaks.RData"))

# sammon_open <- sammon(d_open)
# save(sammon_open, file = file.path("data", "sammon", "openBox.RData"))

# sammon_real_seg <- sammon(d_seg)
# save(sammon_real_seg, file = file.path("data", "sammon", "real_seg.RData"))

# sammon_real_color<- sammon(d_color)
# save(sammon_real_color, file = file.path("data", "sammon", "real_color.RData"))

# sammon_real_grey <- sammon(d_grey)
# save(sammon_real_grey, file = file.path("data", "sammon", "real_grey.RData"))

#### Optimize parameter for Isomap ####

### Artificial data
# test <- c(seq(5,15),20,25,30,35)
# nb_cores <- 7
# optimize_k_isomap(d_swiss, 2, test, "swissRoll", nb_cores)
# optimize_k_isomap(d_helix, 1, test, "helix", nb_cores)
# optimize_k_isomap(d_twins, 2, test, "twinpeaks", nb_cores)
# optimize_k_isomap(d_open, 2, test, "openBox", nb_cores)

## Use the sequential computation for large k
# optimize_k_isomap_seq(d_broken, 2, seq(65,85,10) , "brokenSwissRoll")
# test <- seq(40,80,5)
# optimize_k_isomap_seq(d_swiss, 2, test , "swissRoll")
# optimize_k_isomap_seq(d_helix, 1, test , "helix")
# optimize_k_isomap_seq(d_twins, 2, test, "twinpeaks")
# optimize_k_isomap_seq(d_open, 2, test , "openBox")

### Real data
# test_real <- c(5:25)
# optimize_k_isomap(d_seg, 2, test_real, "real_seg", nb_cores)
# optimize_k_isomap(d_grey, 2, test_real, "real_grey", nb_cores)
# optimize_k_isomap(d_color, 2, test_real, "real_color", nb_cores)

#### Optimize parameter for LLE ####

### Artificial data
## Firstly, we test 5<=k<=15 as in the article:
# dir.create(file.path("data", "test_lle"), showWarnings = FALSE)
# nb_cores <- 7
# optimize_k_lle(swissRoll, kmin = 5, kmax = 15, step = 1, dataname = "swissRoll", nb_cores = nb_cores)
# optimize_k_lle(helix, ndim=1, kmin = 5, kmax = 15, step = 1, dataname = "helix", nb_cores = nb_cores)
# optimize_k_lle(twinpeaks, kmin = 5, kmax = 15, step = 1, dataname = "twinpeaks", nb_cores = nb_cores)
# optimize_k_lle(openBox, kmin = 5, kmax = 15, step = 1, dataname = "openBox", nb_cores = nb_cores)
# optimize_k_lle(brokenSwissRoll, kmin = 5, kmax = 15, step = 1, dataname = "brokenSwissRoll", nb_cores = nb_cores)

## Reduce the number of cpus used when k increases
# nb_cores <- 5
# optimize_k_lle(swissRoll, kmin = 20, kmax = 35, step = 5, dataname = "swissRoll", nb_cores = nb_cores)
# optimize_k_lle(helix, ndim=1, kmin = 20, kmax = 35, step = 5, dataname = "helix", nb_cores = nb_cores)
# optimize_k_lle(twinpeaks, kmin = 20, kmax = 35, step = 5, dataname = "twinpeaks", nb_cores = nb_cores)
# optimize_k_lle(openBox, kmin = 20, kmax = 35, step = 5, dataname = "openBox", nb_cores = nb_cores)
# optimize_k_lle(brokenSwissRoll, kmin = 20, kmax = 35, step = 5, dataname = "brokenSwissRoll", nb_cores = nb_cores)

# nb_cores <- 2
# optimize_k_lle(swissRoll, kmin = 40, kmax = 80, step = 5, dataname = "swissRoll", nb_cores = nb_cores)
# optimize_k_lle(helix, ndim=1, kmin = 40, kmax = 80, step = 5, dataname = "helix", nb_cores = nb_cores)
# optimize_k_lle(twinpeaks, kmin = 40, kmax = 80, step = 5, dataname = "twinpeaks", nb_cores = nb_cores)
# optimize_k_lle(openBox, kmin = 40, kmax = 80, step = 5, dataname = "openBox", nb_cores = nb_cores)
# optimize_k_lle(brokenSwissRoll, kmin = 40, kmax = 80, step = 5, dataname = "brokenSwissRoll", nb_cores = nb_cores)

## Compute LLE for optimal k
# data_name <- c("swissRoll", "helix", "twinpeaks", "brokenSwissRoll", "openBox")
# ndims <- c(2,1,2,2,2)
# lle_k_opt_artificial(data_name, ndims)


### Real data
# nb_cores <- 5
# optimize_k_lle(real_seg, kmin = 5, kmax = 25, step = 1, dataname = "real_seg", nb_cores = nb_cores)
# optimize_k_lle(real_grey, kmin = 5, kmax = 25, step = 1, dataname = "real_grey", nb_cores = nb_cores)
# optimize_k_lle(real_color, kmin = 5, kmax = 25, step = 1, dataname = "real_color", nb_cores = nb_cores)

## Compute LLE for optimal k
# data_name <- c("real_seg", "real_color", "real_grey")
# lle_k_opt_real(data_name, rep(1,3))