#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#----------------------- Manifold Learning Project----------------------
#------------------------------ 2016/2017 ------------------------------

# install.packages("RANN")
  
library(RANN)

# trustworthiness take hd.ngh as a argument so that we compute it once
trustworthiness <- function(k, hd.ngh, ld.data){
  
  n <- nrow(hd.ngh$nn.idx)
  
  print("getting ld neighborhoods")
  ld.ngh <- nn2(ld.data, k=k)
  
  sum <- 0
  for (i in 1:n){

    sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){
      if(is.na(match(ld.ngh$nn.idx[i,m],hd.ngh$nn.idx[i,1:k]))){
        rank <- which(hd.ngh$nn.idx[i,] == ld.ngh$nn.idx[i,m])
        rank-k
      } 
      else 0
    }))
    
  }
  result <- 1 - 2/(n*k*(2*n - 3*k -1)) * sum
  return(result)
}

continuity <- function(k, hd.data, ld.data){
  n <- nrow(hd.data)
  print("getting hd neighborhoods")
  hd.ngh <- nn2(hd.data, k=k)
  print("getting ld neighborhoods")
  ld.ngh <- nn2(ld.data, k=n)
  
  sum <- 0
  for (i in 1:n){
    
    sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){
      if(is.na(match(hd.ngh$nn.idx[i,m],ld.ngh$nn.idx[i,1:k]))){
        rank <- which(ld.ngh$nn.idx[i,] == hd.ngh$nn.idx[i,m])
        rank-k
      } 
      else 0
    }))
    
  }
  result <- 1 - 2/(n*k*(2*n - 3*k -1)) * sum
  return(result) 
}