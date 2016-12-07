#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
list.of.packages <- c("rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rstudioapi)

rm(list=ls())
dev.off()

# Set working directory to the location of this file
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

# Generate data ####
# ATTENTION: Run only once
# source("generate_data.R")

# Optimize the parameters ####


