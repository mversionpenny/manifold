#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Project---------------------
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

# Plot the final results and save as .png. Rerun this file will overwrite all the plot saved in plots/results_[sammon/isomap/lle]
library(RColorBrewer)
library(ggplot2)
library(plot3D)

dir.create(file.path("plots", "results_sammon"), showWarnings = FALSE) 
dir.create(file.path("plots", "results_isomap"), showWarnings = FALSE)
dir.create(file.path("plots", "results_lle"), showWarnings = FALSE)

#### Artificial data ####
best_iso <- c(10, 65, 40, 65, 5)
best_lle <- c(35, 65, 13 , 12 , 75)
data_name <- c("helix", "brokenSwissRoll", "swissRoll", "twinpeaks", "openBox")
short_name <- c("helix", "broken", "swiss", "twins", "open")

## Sammon
for (i in 1:length(data_name)){
  load(file.path("data", "sammon", paste(data_name[i],".RData", sep=""))) #load sammon_[short_name]
  if (i == 1) points <- get(paste("sammon",short_name[i], sep="_"))$points[,1]
  else points <- get(paste("sammon",short_name[i], sep="_"))$points
  png(filename = file.path("plots", "results_sammon", paste(data_name[i], ".png", sep="")))
  plot(points, col = jet.col(5000), xlab="", ylab="", cex.axis=1.5)
  dev.off()
}

## isomap
for (i in 1:length(data_name)){
  load(file.path("data", paste("test_isomap_", data_name[i], sep=""), paste("k", best_iso[i], ".RData", sep=""))) #load x_iso
  if (i == 1) points <- x_iso$points[,1]
  else points <- x_iso$points
  png(filename = file.path("plots", "results_isomap", paste(data_name[i], ".png", sep="")))
  plot(points, col = jet.col(5000), xlab="", ylab="", cex.axis=1.5)
  dev.off()
}

## lle
for (i in 1:length(data_name)){
  load(file.path("data", "lle", paste(data_name[i], '_k', best_lle[i], ".RData", sep=""))) #load x_lle
  points <- x_lle$Y
  png(filename = file.path("plots", "results_lle", paste(data_name[i], ".png", sep="")))
  plot(points, col = jet.col(5000), xlab="", ylab="", cex.axis=1.5)
  dev.off()
}

#### Real data ####
classes <- c("apple", "car", "cow", "cup", "dog", "horse", "pear", "tomato")
n = nrow(real_seg)
factor_class <- as.factor(rep(classes, each = n/8))
col.classes <- brewer.pal(8,"Dark2")
cols.classes <- rep(col.classes, each=n/8)
data_name <- c("real_seg", "real_grey", "real_color")
best_iso <- c(9, 5, 5)
best_lle <- c(18,24,15)

## Sammon
for (name in data_name){
  load(file.path("data", "sammon", paste(name,".RData", sep=""))) # load sammon_real_...(seg, grey or color)
  points <- as.data.frame(get(paste("sammon", name, sep="_"))$points)
  p <- ggplot(points, aes(x=points[,1], y=points[,2])) + 
    geom_point(aes(colour = factor_class)) + 
    xlab("") + ylab("") + scale_color_manual(values=brewer.pal(8, "Dark2")) + 
    theme(legend.title=element_blank())
  ggsave(file.path("plots", "results_sammon", paste(name, ".png", sep="")), plot = p, width = 7, height = 6)
}

## Isomap
for (i in 1:length(data_name)){
  name = data_name[i]
  load(file.path("data", paste("test_isomap_", name , sep=""), paste("k", best_iso[i], ".RData", sep=""))) #load x_iso
  points <- as.data.frame(x_iso$points)
  p <- ggplot(points, aes(x=points[,1], y=points[,2])) + 
    geom_point(aes(colour = factor_class)) + guides(fill=guide_legend(title=NULL)) +
    xlab("") + ylab("") + scale_color_manual(values=brewer.pal(8, "Dark2")) + 
    theme(legend.title=element_blank())
  ggsave(file.path("plots", "results_isomap", paste(name, ".png", sep="")), plot = p, width = 7, height = 6)
}

## LLE
for (i in 1:length(data_name)){
  name = data_name[i]
  load(file.path("data", "lle", paste(name, "_k", best_lle[i],".RData", sep=""))) # load x_lle
  points <- as.data.frame(x_lle$Y)
  p <- ggplot(points, aes(x=points[,1], y=points[,2])) + 
    geom_point(aes(colour = factor_class)) + 
    xlab("") + ylab("") + scale_color_manual(values=brewer.pal(8, "Dark2")) + 
    theme(legend.title=element_blank())
  ggsave(file.path("plots", "results_lle", paste(name, ".png", sep="")), plot = p, width = 7, height = 6)
}
