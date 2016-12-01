#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
list.of.packages <- c("rstudioapi", "kernlab")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rstudioapi)
library(kernlab)

rm(list=ls())
dev.off()

# Load simulation data functions
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
source("generate_data.R")

roll <- swissroll(plot.3D = F) # no 3D plot
realDist <- dist(roll)

kpc <- kpca(~. , data = as.data.frame(roll), kernel="rbfdot", kpar=list(sigma=0.1), features=2)
test <- rotated(kpc)
plot(test)
sum(realDist - dist(test))
#with(text(test[,1], test[,2], labels = colnames(as.matrix(data))))
