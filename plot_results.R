#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------
library(RColorBrewer)
library(ggplot2)
library(plot3D)
dir.create(file.path("plots", "results_sammon"), showWarnings = FALSE)
dir.create(file.path("plots", "results_isomap"), showWarnings = FALSE)
dir.create(file.path("plots", "results_lle"), showWarnings = FALSE)

#### Artificial data ####
best_iso <- c(65, 65, 40, 65, 5)
best_lle <- c(7, 65, 13 , 11 , 11)
data_name <- c("helix", "brokenSwissRoll", "swissRoll", "twinpeaks", "openBox")
short_name <- c("helix", "broken", "swiss", "twins", "open")
for (i in 3:length(data_name)){
  load(file.path("data", "sammon", paste(data_name[i],".RData", sep=""))) #load sammon_[short_name]
  png(filename = file.path("plots", "results_sammon", paste(data_name[i], ".png", sep="")))
  plot(get(paste("sammon",short_name[i], sep="_"))$points, col = jet.col(5000),
       xlab="", ylab="", cex.axis=1.5)
  dev.off()
}

load(file.path("data", "sammon", "helix.RData")) #load sammon_helix
png(filename = file.path("plots", "results_sammon", "helix.png"))
plot(sammon_helix$points[,1], col = jet.col(5000),
     xlab="", ylab="", cex.axis=1.5)
dev.off()


load(file.path("data", "sammon", "brokenSwissRoll.RData")) #load sammon_broken
png(filename = file.path("plots", "results_sammon", "brokenSwissRoll.png"))
plot(sammon_broken$points, col = jet.col(5000),
     xlab="", ylab="", cex.axis=1.5)
dev.off()



#### Real data ####
classes <- c("apple", "car", "cow", "cup", "dog", "horse", "pear", "tomato")
factor_class <- as.factor(rep(classes, each = n/8))
n = nrow(real_seg)
col.classes <- brewer.pal(8,"Dark2")
cols.classes <- rep(col.classes, each=n/8)
data_name <- c("real_seg", "real_grey", "real_color")

for (name in data_name){
  load(file.path("data", "sammon", paste(name,".RData", sep=""))) # load sammon_real_...(seg, grey or color)
  points <- as.data.frame(get(paste("sammon", name, sep="_"))$points)
  p <- ggplot(points, aes(x=points[,1], y=points[,2])) + 
    geom_point(aes(colour = factor_class)) + 
    xlab("") + ylab("") + scale_color_manual(values=brewer.pal(8, "Dark2"))
  ggsave(file.path("plots", "results_sammon", paste(name, ".png", sep="")), plot = p, width = 7, height = 6)
}


load(file.path("data", "lle", "real_seg_k18.RData")) #load sammon_real_seg
# png(filename = file.path("plots", "results_sammon", paste(name, ".png", sep="")))
plot(x_lle$Y, col = cols, pch = 16, xlab = "", ylab="")
legend(3, 1, legend = classes, pch =16, col = col, xpd=TRUE)
