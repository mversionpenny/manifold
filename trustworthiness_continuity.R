#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#----------------------- Manifold Learning Project----------------------
#------------------------------ 2016/2017 ------------------------------

# install.packages("RANN")
library(RANN)
trustworthiness <- function(k, HD_data, LD_data){
  
  n <- nrow(HD_data)
  totalSum <-0
  print("getting hd neighborhoods")
  hd.ngh <- nn2(HD_data, k=k)
  print("getting ld neighborhoods")
  ld.ngh <- nn2(LD_data, k=k)
  
  sum <- 0
  for (i in 1:n){
    print(i)
    # better to use apply here eg : sapply(1:nrow(iris), FUN = function(i) leave.one.out(i))
    # for(m in 1:k){
    #   if(!match(ld.ngh[i,m],hd.ngh[i,])){
    #     sumi <- sumi + m
    #   }
    # }
    sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){
              if(is.na(match(ld.ngh$nn.idx[i,m],hd.ngh$nn.idx[i,]))) (m-k)
              else 0
            }))
    
  }
  result <- 1 - 2/(n*k*(2*n - 3*k -1)) * sum
  return(result)
}

continuity <- function(){
  
}