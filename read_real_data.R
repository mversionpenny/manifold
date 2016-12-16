#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
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

# Read all ETH-80 data set and write the data to a matrix to facilitate the charging de data afterwards.
# This file has been run and the data matrix are already saved in data/real_data_matrix. 
# Rerun this file will take a lot of time and will overwrite all the files in real_data_matrix folder.
library(png)
library(doParallel)
library(foreach)
library(parallel)

# *Function : read_folder                             
# *Description: read png images in a folder, transform them to a vector of pixel values and  finally concatenate all the vector to a matrix.
# This function is meant to be called by read_real_data
# *Inputs: 
# f: the index in the folder in list_folder
# list_folder: list of folder of data
# data_path: path to data
# step: number of images between 2 read images (1: read all images)
# *Outputs: matrix of pixel values of images
read_folder <- function(f, list_folder, data_path, step){
  require(png)
  folder = list_folder[f]
  
  list_files <- list.files(file.path(data_path,folder))
  for (i in seq(1,length(list_files), step)){
    img <- readPNG(file.path(data_path,folder,list_files[i]))
    img <- as.vector(img)
    if (i==1){
      real <- img
    }
    else{
      real <- rbind(real, img)
    } 
  }
  return(real)
}

# *Function : read_real_data                             
# *Description: read png images in a folder containing many subfolders, transform them to a vector of pixel values and  finally concatenate all the vector to a matrix.
# This function use parallel computation to read many subfolders at once. 
# *Inputs: 
# data_path: path to folder containing the images. The folder can only contain subfolders and the subfolders can only contain png images in order to this function to work.
# step: number of images between 2 read images (1: read all images)
# nb_cores: number of cpus to be used in parallel computation
# *Outputs: matrix of pixel values of images
read_real_data <- function(data_path, nb_cores, step=1) {
  list_folder <- list.files(data_path)
  
  cl <- makeCluster(nb_cores)
  clusterExport(cl, list())
  registerDoParallel(nb_cores)
  res <- foreach(i= seq(1,length(list_folder)),  .combine='rbind', .packages = "png", .export = "read_folder") %dopar% {
    require(png)
    read_folder(i, list_folder, data_path, step)
  }
  stopCluster(cl)
  rownames(res) <- NULL
  return(res)
}



dir.create(file.path("data", "real_data_matrix"), showWarnings = FALSE)
nb_cores = 7

##!!!!!! Save and load a .RData is !!!!A LOT FASTER!!!! than save and load a .txt
# real_seg <- read_real_data(file.path("data", "real_data_seg"),nb_cores, 5)
# save(real_seg, file = file.path("data", "real_data_matrix", "segmentation.RData"))
# d_seg <- dist(real_seg)
# save(d_seg,file = file.path("data", "real_data_matrix", "dist_segmentation.RData"))
# 
# real_grey <- read_real_data(file.path("data", "real_data_grey"),nb_cores, 5)
# save(real_grey, file = file.path("data", "real_data_matrix", "grey.RData"))
# d_grey <- dist(real_grey)
# save(d_grey,file = file.path("data", "real_data_matrix", "dist_grey.RData"))
# 
# real_color <- read_real_data(file.path("data", "real_data_color"), nb_cores, 5)
# save(real_color, file = file.path("data", "real_data_matrix", "color.RData"))
# d_color <- dist(real_color)
# save(d_color,file = file.path("data", "real_data_matrix", "dist_color.RData"))
