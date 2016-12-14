#--------- Margot Selosse, Hoai Thu Nguyen, Maram Romdhane -------------
#----------------------- Manifold Learning Project----------------------
#------------------------------ 2016/2017 ------------------------------

# install.packages("RANN") #.gitignore
  
library(RANN)

# trustworthiness take hd.ngh as a argument so that we compute it once
trustworthiness <- function(k, hd.ngh, ld.data){ #.gitignore
   #.gitignore
  n <- nrow(hd.ngh$nn.idx) #.gitignore
   #.gitignore
  print("getting ld neighborhoods") #.gitignore
  ld.ngh <- nn2(ld.data, k=k) #.gitignore
   #.gitignore
  sum <- 0 #.gitignore
  for (i in 1:n){
 #.gitignore
    sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){ #.gitignore
      if(is.na(match(ld.ngh$nn.idx[i,m],hd.ngh$nn.idx[i,1:k]))){ #.gitignore
        rank <- which(hd.ngh$nn.idx[i,] == ld.ngh$nn.idx[i,m]) #.gitignore
        rank-k #.gitignore
      }  #.gitignore
      else 0 #.gitignore
    })) #.gitignore
     #.gitignore
  } #.gitignore
  result <- 1 - 2/(n*k*(2*n - 3*k -1)) * sum #.gitignore
  return(result) #.gitignore
}

continuity <- function(k, hd.data, ld.data){ #.gitignore
  n <- nrow(hd.data) #.gitignore
  print("getting hd neighborhoods") #.gitignore
  hd.ngh <- nn2(hd.data, k=k) #.gitignore
  print("getting ld neighborhoods") #.gitignore
  ld.ngh <- nn2(ld.data, k=n) #.gitignore
   #.gitignore
  sum <- 0 #.gitignore
  for (i in 1:n){ #.gitignore
     #.gitignore
    sum <- sum + Reduce("+", sapply(1:k, FUN = function(m){ #.gitignore
      if(is.na(match(hd.ngh$nn.idx[i,m],ld.ngh$nn.idx[i,1:k]))){ #.gitignore
        rank <- which(ld.ngh$nn.idx[i,] == hd.ngh$nn.idx[i,m]) #.gitignore
        rank-k #.gitignore
      }  #.gitignore
      else 0 #.gitignore
    })) #.gitignore
     #.gitignore
  } #.gitignore
  result <- 1 - 2/(n*k*(2*n - 3*k -1)) * sum #.gitignore
  return(result) #.gitignore
}