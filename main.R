#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
list.of.packages <- c("rstudioapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rstudioapi)

# Load simulation data functions
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
source("generate_data.R")
