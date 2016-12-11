#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#----------------------- Manifold Learning Project----------------------
#------------------------------ 2016/2017 ------------------------------

# install.packages("RANN")
# install.packages("dendextend")
library(RANN)
trustworthiness <- function(k, HD_data, LD_data){
  
  n <- nrow(HD_data)
  totalSum <-0
  print("getting hd neighborhoods")
  hd.ngh <- nn2(HD_data, k=n)
  print("getting ld neighborhoods")
  ld.ngh <- nn2(LD_data, k=k)
  distances <- dist(LD_data)
  
  sum <- 0
  for (i in 1:n){
    
    # sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){
    #           if(is.na(match(ld.ngh$nn.idx[i,m],hd.ngh$nn.idx[i,]))) (m-k)
    #           else 0
    #         }))
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

continuity <- function(){
  
}