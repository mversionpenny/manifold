#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#------------------------ Manifold Learning Prject----------------------
#------------------------------ 2016/2017 ------------------------------

this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
source("trustworthiness_continuity.R")

#### Loading HD data and computing HD neighborhoods ####
K = 10
n=5000
swissRoll <- read.table("data/swissRoll.txt", sep = "\t")
hd.ngh.swissRoll <- nn2(swissRoll, k=n)
  
brokenSwissRoll <- read.table("data/brokenSwissRoll.txt", sep = "\t")
hd.ngh.brokenSwissRoll <- nn2(brokenSwissRoll, k=n)

helix <- read.table("data/helix.txt", sep = "\t")
hd.ngh.helix <- nn2(helix, k=n)

twinpeaks <- read.table("data/twinpeaks.txt", sep = "\t")
hd.ngh.twinpeaks <- nn2(twinpeaks, k=n)


openBox <- read.table("data/openBox.txt", sep = "\t")
hd.ngh.openBox<- nn2(openBox, k=n)

#### Sammon Mapping ####

swissRoll.sammon <- load("data/sammon/swissRoll.RData")
swissRoll.sammon <- get(swissRoll.sammon)
brokenSwissRoll.sammon <- load("data/sammon/brokenSwissRoll.RData")
brokenSwissRoll.sammon <- get(brokenSwissRoll.sammon)
helix.sammon <- load("data/sammon/helix.RData")
helix.sammon <- get(helix.sammon)
twinpeaks.sammon <- load("data/sammon/twinpeaks.RData")
twinpeaks.sammon <- get(twinpeaks.sammon)
openBox.sammon <- load("data/sammon/openBox.RData")
openBox.sammon <- get(openBox.sammon)

# trustworthiness
trust.sammon.swissRoll <- trustworthiness(K, hd.ngh.swissRoll, swissRoll.sammon$points)
trust.sammon.brokenSwissRoll <- trustworthiness(K, hd.ngh.brokenSwissRoll, brokenSwissRoll.sammon$points)
trust.sammon.helix <- trustworthiness(K, hd.ngh.helix, helix.sammon$points[,1])
trust.sammon.twinpeaks <- trustworthiness(K, hd.ngh.twinpeaks, twinpeaks.sammon$points)
trust.sammon.openBox <- trustworthiness(K, hd.ngh.openBox, openBox.sammon$points)

# continuity
cont.sammon.swissRoll <- continuity(K, swissRoll, swissRoll.sammon$points)
cont.sammon.brokenSwissRoll <- continuity(K, brokenSwissRoll, brokenSwissRoll.sammon$points)
cont.sammon.helix <- continuity(K, helix, helix.sammon$points[,1])
cont.sammon.twinpeaks <- continuity(K, twinpeaks, twinpeaks.sammon$points)
cont.sammon.openBox <- continuity(K, openBox, openBox.sammon$points)

#### ISOMAP ####

#### Local Linear Embedding ####

