#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
list.of.packages <- c("rstudioapi", "doParallel", "foreach")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rstudioapi)
library(doParallel)
library(foreach)

rm(list=ls())
dev.off()

## Set working directory to the location of this file
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

#### Generate data ####
## ATTENTION: Run only once
# source("generate_data.R")

#### Read the artificial data ####
swissRoll <- read.table("data/swissRoll.txt", sep = "\t")
brokenSwissRoll <- read.table("data/brokenSwissRoll.txt", sep = "\t")
helix <- read.table("data/helix.txt", sep = "\t")
twinpeaks <- read.table("data/twinpeaks.txt", sep = "\t")
openBox <- read.table("data/openBox.txt", sep = "\t")

#### Optimize the parameters ####
## ATTENTION: Take a lot of time. Run only once.

source("optimize_parameters.R")

## using 4 cores :
cl <- makeCluster(4)
clusterExport(cl)

## Run on 2 seperated computers
test1 = c(10, 15, 25, 50) #margot
test2 = c(5, 20, 30, 40) #thu

## Swiss Roll
d_swiss <- dist(swissRoll)
# optimize_k_isomap(d_swiss, test1) 
optimize_k_isomap(d_swiss, test2)

stopCluster(cl)


