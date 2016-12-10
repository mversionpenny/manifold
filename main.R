#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
list.of.packages <- c("rstudioapi", "doParallel", "foreach", "vegan", "parallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rstudioapi)
library(doParallel)
library(foreach)
library(vegan)
library(parallel)

rm(list=ls())

## Set working directory to the location of this file
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

#### Generate data ####
## ATTENTION: Run only once
# source("generate_data.R")

#### Read the artificial data ####
swissRoll <- read.table("data/swissRoll.txt", sep = "\t")
d_swiss <- dist(swissRoll)

brokenSwissRoll <- read.table("data/brokenSwissRoll.txt", sep = "\t")
d_broken <- dist(brokenSwissRoll)

helix <- read.table("data/helix.txt", sep = "\t")
d_helix <- dist(helix)

twinpeaks <- read.table("data/twinpeaks.txt", sep = "\t")
d_twins <- dist(twinpeaks) 

openBox <- read.table("data/openBox.txt", sep = "\t")
d_open <- dist(openBox)

#### Optimize the parameters ####
## ATTENTION: Take a lot of time. Run only once.

source("optimize_parameters.R")

test = c(5,10,15,20,25,30,40)

## Create cluster :
nb_cores <- detectCores() - 1 
cl <- makeCluster(nb_cores)
clusterExport(cl, list())
registerDoParallel(nb_cores)

## Swiss Roll
optimize_k_isomap(d_broken, 2, test, "brokenSwissRoll")


stopCluster(cl)


