#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
# Read all ETH-80 data set and write the data to a matrix to facilitate the
# charging de data afterwards
library(png)
library(doParallel)
library(foreach)
library(parallel)

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
real_seg <- read_real_data(file.path("data", "real_data_seg"),nb_cores, 5)
save(real_seg, file = file.path("data", "real_data_matrix", "segmentation.RData"))
d_seg <- dist(real_seg)
save(d_seg,file = file.path("data", "real_data_matrix", "dist_segmentation.RData"))

real_grey <- read_real_data(file.path("data", "real_data_grey"),nb_cores, 5)
save(real_grey, file = file.path("data", "real_data_matrix", "grey.RData"))
d_grey <- dist(real_grey)
save(d_grey,file = file.path("data", "real_data_matrix", "dist_grey.RData"))

real_color <- read_real_data(file.path("data", "real_data_color"), nb_cores, 5)
save(real_color, file = file.path("data", "real_data_matrix", "color.RData"))
d_color <- dist(real_color)
save(d_color,file = file.path("data", "real_data_matrix", "dist_color.RData"))
