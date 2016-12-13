#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
list.of.packages <- c("rstudioapi", "png")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
library(rstudioapi)
library(png)

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

#### Read the real data ####
data_path <- file.path("data", "real_data_image")
list_folder <- list.files(data_path)
row_names <- c()
for (folder in list_folder){
  list_files <- list.files(file.path(data_path,folder))
  for (i in 1:length(list_files)){
    row_names <- c(row_names, paste(folder, "_", i, sep=""))
    img <- readPNG(file.path(data_path,folder,list_files[i]))
    img <- as.vector(img)
    if (!exists("real")){
      real <- img
    }
    else{
      real <- rbind(real, img)
    }  
  }
}
real <- as.data.frame(real)
rownames(real) <- row_names

d <- dist(real)
test <- sammon(d)
plot(test$points, type ="n")
text(test$points, labels = row_names)
#### Optimize the parameters ####
## ATTENTION: Take a lot of time. Run only once.
## Result objects are all saved in order to save time of calculation later
source("optimize_parameters.R")

