#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Project----------------------
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

list.of.packages <- c("rstudioapi", "rgl", "plot3D", "RColorBrewer", "doParallel", "foreach", "png", "parallel", "vegan", "lle" , "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rstudioapi)

rm(list=ls())

## Set working directory to the location of this file
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

#### Generate data ####
## ATTENTION: Run only once
# data are already saved in 'data' as .txt files. Rerun the line the line below might overwrite the data files.
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

#### Read the real data ####
## The line below shouldn't be rerun (took a lot of time)
source("read_real_data.R")
# add the pixels of images into a large matrix => saved as .RData files in data/real_data_matrix

## load the saved matrix
load(file.path("data","real_data_matrix", "segmentation.RData")) # load real_seg
load(file.path("data","real_data_matrix", "dist_segmentation.RData")) #load d_seg
load(file.path("data","real_data_matrix", "grey.RData")) #load real_grey
load(file.path("data","real_data_matrix", "dist_grey.RData")) #load d_grey
load(file.path("data","real_data_matrix", "color.RData")) #load real_color
load(file.path("data","real_data_matrix", "dist_color.RData")) # load d_color

#### Optimize the parameters ####
## ATTENTION: Take a lot of time. Run only once.
## Result objects are all saved in order to save time of calculation later
## Results are saved in 'plots' or 'data' (details in optimize_parameters.R)
source("optimize_parameters.R")

#### Calculate the trustworthiness and the continuity ####
source("trustworthiness_continuity.R")

#### Plot the results and save into plots/results_... ####
source("plot_results.R")
