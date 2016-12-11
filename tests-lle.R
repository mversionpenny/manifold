#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------

# /!\ NOT A DELIVERABLE

rm(list=ls())
dev.off()

## Set working directory to the location of this file
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

#### Use of lle package ####
# install.packages("lle")
library(lle)
swissRoll <- read.table("data/swissRoll.txt", sep = "\t")
helix <- read.table("data/helix.txt", sep = "\t")
# with k :
lle.k.swissroll <- lle(swissRoll, m=2, k=20)
lle.k.helix <- lle(helix, m=2, k=20)
write.table(lle.k.swissroll$Y,file = "swissroll-lle.txt", sep = "\t", col.names = F, row.names = F)
write.table(lle.k.helix$Y,file = "helix-lle.txt", sep = "\t", col.names = F, row.names = F)

# with epsilon /!\ be careful, nnk must be FALSE
lle.eps <- lle(swissRoll, m=2, nnk=FALSE, eps=0.1)

# not too bad function :
plot_lle(lle.k$Y, lle.k$X)

#### Use of rdrtools package ####
source("https://bioconductor.org/biocLite.R")
biocLite("RDRToolbox")
library(RDRToolbox)
lle.rdr.k <- LLE(data=as.matrix(swissRoll), dim=2, k=10)
