#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#----------------------- Manifold Learning Project----------------------
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


# install.packages("RANN") 
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
library(RANN) 


# +-------------------------------------------------------------------------+
# | *Function : trustworthiness                                             |
# | *Description: computes the trustworthiness of a low dimensionnal        |
# |  dataset with respect to its high-dimensionnal original dataset.        |
# | *Inputs: - k: number of neighborhoods compared locally                  |
# |          - hd.ngh: k-nearest-neighbors for each point of high-          |
# |            dimensionnal dataset                                         |
# |          - ld.data: points from low-dimensionnal dataset                |
# | *Outputs:- trustworthiness (float)                                      |
# |                                                                         |
# | cf. report for details on computations of trustworthiness.              |
# +-------------------------------------------------------------------------+


trustworthiness <- function(k, hd.ngh, ld.data){ 
  
  n <- nrow(hd.ngh$nn.idx) 
  
  print("getting ld neighborhoods") 
  ld.ngh <- nn2(ld.data, k=k) 
  
  sum <- 0 
  for (i in 1:n){ 
    
    sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){ 
      if(is.na(match(ld.ngh$nn.idx[i,m],hd.ngh$nn.idx[i,1:k]))){ 
        rank <- which(hd.ngh$nn.idx[i,] == ld.ngh$nn.idx[i,m]) 
        rank-k 
      }  
      else 0 
    })) 
    
  } 
  result <- 1 - 2/(n*k*(2*n - 3*k -1)) * sum 
  return(result) 
} 

# +-------------------------------------------------------------------------+
# | *Function : continuity                                                  |
# | *Description: computes the continuity of a low dimensionnal             |
# |  dataset with respect to its high-dimensionnal original dataset.        |
# | *Inputs: - k: number of neighborhoods compared locally                  |
# |          - hd.data: points from high-dimensionnal dataset               |
# |          - ld.data: points from low-dimensionnal dataset                |
# | *Outputs:- continuity (float)                                           |
# |                                                                         |
# | cf. report for details on computations of continuity                    |
# +-------------------------------------------------------------------------+

continuity <- function(k, hd.data, ld.data){ 
  n <- nrow(hd.data) 
  print("getting hd neighborhoods")
  hd.ngh <- nn2(hd.data, k=k)
  print("getting ld neighborhoods") 
  ld.ngh <- nn2(ld.data, k=n) 
  
  sum <- 0 
  for (i in 1:n){ 
    
    sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){ 
      if(is.na(match(hd.ngh$nn.idx[i,m],ld.ngh$nn.idx[i,1:k]))){ 
        rank <- which(ld.ngh$nn.idx[i,] == hd.ngh$nn.idx[i,m]) 
        rank-k 
      }  
      else 0 
    })) 
    
  } 
  result <- 1 - 2/(n*k*(2*n - 3*k -1)) * sum 
  return(result) 
} 

#### Artificial ####

#### Loading HD data and computing HD neighborhoods ####
K = 10
n=5000
swissRoll <- read.table("data/swissRoll.txt", sep = "\t")
# hd.ngh.swissRoll <- nn2(swissRoll, k=n)

brokenSwissRoll <- read.table("data/brokenSwissRoll.txt", sep = "\t")
# hd.ngh.brokenSwissRoll <- nn2(brokenSwissRoll, k=n)

helix <- read.table("data/helix.txt", sep = "\t")
# hd.ngh.helix <- nn2(helix, k=n)

twinpeaks <- read.table("data/twinpeaks.txt", sep = "\t")
# hd.ngh.twinpeaks <- nn2(twinpeaks, k=n)


openBox <- read.table("data/openBox.txt", sep = "\t")
# hd.ngh.openBox <- nn2(openBox, k=n)

# #### Sammon Mapping ####
# 
# swissRoll.sammon <- get(load("data/sammon/swissRoll.RData"))
# brokenSwissRoll.sammon <- get(load("data/sammon/brokenSwissRoll.RData"))
# helix.sammon <- get(load("data/sammon/helix.RData"))
# twinpeaks.sammon <- get(load("data/sammon/twinpeaks.RData"))
# openBox.sammon <- get(load("data/sammon/openBox.RData"))
# 
# 
# # trustworthiness
# trust.sammon.swissRoll <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.sammon$points)
# trust.sammon.brokenSwissRoll <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.sammon$points)
# trust.sammon.helix <- trustworthiness(K, hd.ngh.helix, helix.sammon$points[,1])
# trust.sammon.twinpeaks <- trustworthiness(K, hd.ngh.twinpeaks, twinpeaks.sammon$points)
# trust.sammon.openBox <- trustworthiness(K, hd.ngh.openBox, openBox.sammon$points)
# 
# # continuity
# cont.sammon.swissRoll <- continuity(K, swissRoll, swissRoll.sammon$points)
# cont.sammon.brokenSwissRoll <- continuity(K, brokenSwissRoll, brokenSwissRoll.sammon$points)
# cont.sammon.helix <- continuity(K, helix, helix.sammon$points[,1])
# cont.sammon.twinpeaks <- continuity(K, twinpeaks, twinpeaks.sammon$points)
# cont.sammon.openBox <- continuity(K, openBox, openBox.sammon$points)
# 
# #### ISOMAP ####
# 
# swissRoll.isomap <- get(load("./data/test_isomap_swissRoll/k40.RData"))
# brokenSwissRoll.isomap <- get(load("./data/test_isomap_brokenSwissRoll/k65.RData"))
# helix.isomap <- get(load("./data/test_isomap_helix/k10.RData"))
# twinpeaks.isomap <- get(load("./data/test_isomap_twinpeaks/k65.RData"))
# openBox.isomap <- get(load("./data/test_isomap_openBox/k5.RData"))
# 
# #trustworthiness
# trust.isomap.swissRoll <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.isomap$points)
# trust.isomap.brokenSwissRoll <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.isomap$points)
# trust.isomap.helix <- trustworthiness(K, hd.ngh.helix, helix.isomap$points[,1])
# trust.isomap.twinpeaks <- trustworthiness(K, hd.ngh.twinpeaks, twinpeaks.isomap$points)
# trust.isomap.openBox <- trustworthiness(K, hd.ngh.openBox, openBox.isomap$points)
# 
# #continuity
# cont.isomap.swissRoll <- continuity(K, swissRoll, swissRoll.isomap$points)
# cont.isomap.brokenSwissRoll <- continuity(K, brokenSwissRoll, brokenSwissRoll.isomap$points)
# cont.isomap.helix <- continuity(K, helix, helix.isomap$points[,1])
# cont.isomap.twinpeaks <- continuity(K, twinpeaks, twinpeaks.isomap$points)
# cont.isomap.openBox <- continuity(K, openBox, openBox.isomap$points)
# 
# #### Local Linear Embedding ####
# 
# swissRoll.lle<- get(load("./data/lle/swissRoll_k13.RData"))
# brokenSwissRoll.lle <- get(load("./data/lle/brokenSwissRoll_k65.RData"))
# helix.lle <- get(load("./data/lle/helix_k8.RData"))
# twinpeaks.lle <- get(load("./data/lle/twinpeaks_k12.RData"))
# openBox.lle <- get(load("./data/lle/openBox_k8.RData"))
# 
# # trustworthiness
# trust.lle.swissroll <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.lle$Y)
# trust.lle.brokenSwissroll <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.lle$Y)
# trust.lle.helix <- trustworthiness(K, hd.ngh.helix, helix.lle$Y)
# trust.lle.twinpeaks <- trustworthiness(K, hd.ngh.twinpeaks, twinpeaks.lle$Y)
# trust.lle.openBox <- trustworthiness(K, hd.ngh.openBox, openBox.lle$Y)
# 
# # continuity
# 
# cont.lle.swissroll <- continuity(K, swissRoll, swissRoll.lle$Y)
# cont.lle.brokenSwissroll <- continuity(K, brokenSwissRoll, brokenSwissRoll.lle$Y)
# cont.lle.helix <- continuity(K, helix, helix.lle$Y)
# cont.lle.twinpeaks <- continuity(K, twinpeaks, twinpeaks.lle$Y)
# cont.lle.openBox <- continuity(K, openBox, openBox.lle$Y)
