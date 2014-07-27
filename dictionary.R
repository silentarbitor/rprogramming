
cacheinit <- function() {
  cachedmatrix <<- list()
  solvedmatrix <<- list()
}

getindex <- function(x) {
  answer <- -1
  
  if (length(cachedmatrix)==0){
    return(answer)
  }
  
  for (i in length(cachedmatrix)) {
    if (matrixcompare(x,cachedmatrix[[i]])) {
      answer <- i
      break
    }
  }
  return(answer) #returns -1 if no matrix is found in cache
}

lookupinverse <- function(i) {
  solvedmatrix[[i]]
}

store <- function(x,y) {
  cachedmatrix[[length(cachedmatrix)+1]] <<- x
  solvedmatrix[[length(solvedmatrix)+1]] <<- y
}

#looks at imputed matrix and stored matrix to see if any are the same.
matrixcompare <- function(x,y) { 
  if(ncol(x)==ncol(y) &  nrow(x)==nrow(y)) {
    
    veracity<-as.vector(x==y)
    
    for(i in length(veracity)) {
      if (veracity[i]==FALSE){
        return(FALSE)
        break
      }
      if(veracity[length(veracity)]==TRUE){
        return(TRUE)
      }
    }
  }
  else {
    return(FALSE)
  }
}
