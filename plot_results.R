#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
library(RColorBrewer)
library(ggplot2)

dir.create(file.path("plots", "results_sammon"), showWarnings = FALSE)
dir.create(file.path("plots", "results_isomap"), showWarnings = FALSE)
dir.create(file.path("plots", "results_lle"), showWarnings = FALSE)
#### Real data ####
classes <- c("apple", "car", "cow", "cup", "dog", "horse", "pear", "tomato")
n = nrow(real_seg)
col.classes <- brewer.pal(8,"Dark2")
cols.classes <- rep(col, each=n/8)
data_name <- c("real_seg", "real_grey", "real_color")

par(xpd=TRUE)
for (name in data_name){
  load(file.path("data", "sammon", "real_seg.RData")) #load sammon_real_seg
  png(filename = file.path("plots", "results_sammon", paste(name, ".png", sep="")))
  points <- get(paste("sammon", name, sep="_"))
  p <- ggplot(data = scores, aes(x = scores[,i], y =scores[,j], colour = class, label=time)) + geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, colour = "gray65") + xlab(xname) + ylab(yname) + geom_point() + geom_path(aes(group = class)) + theme(legend.title=element_blank()) #+ ggtitle("PCA plot of trajectories")
  legend("right", legend = classes, pch =16, col = col)
  dev.off()
}


load(file.path("data", "lle", "real_seg_k18.RData")) #load sammon_real_seg
# png(filename = file.path("plots", "results_sammon", paste(name, ".png", sep="")))
plot(x_lle$Y, col = cols, pch = 16, xlab = "", ylab="")
legend(3, 1, legend = classes, pch =16, col = col, xpd=TRUE)
