cachedmatrix <<- NULL
solvedmatrix <<- NULL

cachematrix <- function(x){
  
    if (is.null(cachedmatrix)){
      cacheinit()
    }
    
    if(getindex(x) == -1) {
      print("calculating the inverse of the matrix")
      y <- solve(x)
      print(y)
      store(x,y)
    }
    else {
      print("retrieving matrix inverse from list")
      print(lookupinverse(getindex(x)))
    }
    
    
}