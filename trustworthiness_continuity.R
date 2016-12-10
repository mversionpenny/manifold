#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#----------------------- Manifold Learning Project----------------------
#------------------------------ 2016/2017 ------------------------------

trustworthiness <- function(k, HD_data, LD_data){
  totalSum <-0
  for (i in 1:nrow(HD_data)){
    hd.ngh <- getHDNeighborhood(i, k, HD_data)
    ld.ngh <- getSortedLDNeighborhood(i, k, HD_data)
    sumi <- 0
    for(m in 1:length(ld.ngh)){
      if(!match(ld.ngh[m],hd.ngh)){
        sumi <- sumi + m
      }
    }
    totalSum <- totalSum +sumi
  }
  
}

continuity <- function(){
  
}