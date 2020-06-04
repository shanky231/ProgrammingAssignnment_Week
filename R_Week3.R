makeCacheMatrix <- function(x=matrix()){
  
  inverse = NULL
  
  set <- function(y) {
    x <<- y
    m=null
  }
  
  get <-function(){
    x
  }
  
  setInverse <- function(inv) {
    inverse= inv
  }
  
  getInverse <- function(){
    inverse
  }
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x,...){
  i=x$getInverse()
  
  if(!is.null(i)){
      return(i)
  }
  
  data=x$set()
  i <- solve(data,...)
  x$setInverse(i)
  i
}

