#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#----------------------- Manifold Learning Project----------------------
#------------------------------ 2016/2017 ------------------------------

# install.packages("RANN") 
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
library(RANN) 

# trustworthiness take hd.ngh as a argument so that we compute it once
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

continuity <- function(k, hd.data, ld.data){ 
  n <- nrow(hd.data) 
  print("getting hd neighborhoods") 
  c
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
# hd.ngh.openBox<- nn2(openBox, k=n)

#### Sammon Mapping ####

swissRoll.sammon <- load("data/sammon/swissRoll.RData")
swissRoll.sammon <- get(swissRoll.sammon)
brokenSwissRoll.sammon <- load("data/sammon/brokenSwissRoll.RData")
brokenSwissRoll.sammon <- get(brokenSwissRoll.sammon)
helix.sammon <- load("data/sammon/helix.RData")
helix.sammon <- get(helix.sammon)
twinpeaks.sammon <- load("data/sammon/twinpeaks.RData")
twinpeaks.sammon <- get(twinpeaks.sammon)
openBox.sammon <- load("data/sammon/openBox.RData")
openBox.sammon <- get(openBox.sammon)

# trustworthiness
trust.sammon.swissRoll <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.sammon$points)
trust.sammon.brokenSwissRoll <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.sammon$points)
trust.sammon.helix <- trustworthiness(K, hd.ngh.helix, helix.sammon$points[,1])
trust.sammon.twinpeaks <- trustworthiness(K, hd.ngh.twinpeaks, twinpeaks.sammon$points)
trust.sammon.openBox <- trustworthiness(K, hd.ngh.openBox, openBox.sammon$points)

# continuity
cont.sammon.swissRoll <- continuity(K, swissRoll, swissRoll.sammon$points)
cont.sammon.brokenSwissRoll <- continuity(K, brokenSwissRoll, brokenSwissRoll.sammon$points)
cont.sammon.helix <- continuity(K, helix, helix.sammon$points[,1])
cont.sammon.twinpeaks <- continuity(K, twinpeaks, twinpeaks.sammon$points)
cont.sammon.openBox <- continuity(K, openBox, openBox.sammon$points)

#### ISOMAP ####

swissRoll.isomap <- get(load("./data/test_isomap_swissRoll/k40.RData"))
brokenSwissRoll.isomap <- get(load("./data/test_isomap_brokenSwissRoll/k65.RData"))
helix.isomap <- get(load("./data/test_isomap_helix/k5.RData"))
twinpeaks.isomap <- get(load("./data/test_isomap_twinpeaks/k65.RData"))

#trustworthiness
trust.isomap.swissRoll <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.isomap$points)
trust.isomap.brokenSwissRoll <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.isomap$points)
trust.isomap.helix <- trustworthiness(K, hd.ngh.helix, helix.isomap$points[,1])
trust.isomap.twinpeaks <- trustworthiness(K, hd.ngh.twinpeaks, twinpeaks.isomap$points)

#continuity

#### Local Linear Embedding ####

swissRoll.lle.k13 <- load("./data/lle/swissRoll_k13.RData")
swissRoll.lle.k13 <- get(swissRoll.lle.k13)
swissRoll.lle.k80 <- load("./data/lle/swissRoll_k80.RData")
swissRoll.lle.k80 <- get(swissRoll.lle.k80)

brokenSwissRoll.lle.k11 <- load("./data/lle/brokenSwissRoll_k11.RData")
brokenSwissRoll.lle.k11 <- get(brokenSwissRoll.lle.k11)
brokenSwissRoll.lle.k65 <- load("./data/lle/brokenSwissRoll_k65.RData")
brokenSwissRoll.lle.k65 <- get(brokenSwissRoll.lle.k65)

helix.lle <- get(load("./data/lle/helix_k7.RData"))

twinpeaks.lle <- get(load("./data/lle/twinpeaks_k11.RData"))

openBox.lle <- get(load("./data/lle/openBox_k11.RData"))

# trustworthiness
trust.lle.swissroll.k13 <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.lle.k13$Y)
trust.lle.swissroll.k80 <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.lle.k80$Y)
trust.lle.brokenSwissroll.k11 <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.lle.k11$Y)
trust.lle.brokenSwissroll.k65 <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.lle.k65$Y)
trust.lle.helix <- trustworthiness(K, hd.ngh.helix, helix.lle$Y)
trust.lle.twinpeaks <- trustworthiness(K, hd.ngh.twinpeaks, twinpeaks.lle$Y)
trust.lle.openBox <- trustworthiness(K, hd.ngh.openBox, openBox.lle$Y)

# continuity



