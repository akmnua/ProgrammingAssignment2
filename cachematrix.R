## Description: Creates an object that caches the inverse of
## a matrix and exposes set and get functions

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     ##set function for creating matrix cache object
     set <- function(y){
          x <<- y 
          inv <<- NULL 
     }
     
     ##get function for returning matrix cache object
     get <- function(){
          x
     }
     
     ##set the inverse of a matrix for caching
     setInverse <- function(inverse){
          inv <<- inverse 
     }
     
     ##get the cached inverse of a matrix 
     getInverse <- function(){
          inv
     }
     
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
     

}


## Description: Returns the inverse of a matrix 'x' created using
## the makeCacheMatrix function, checking if the solution has been 
## cached in the matrix object

cacheSolve <- function(x, ...) {
     
     result <- x$getInverse()
     
     if(is.null(result)){
          message("not found")
          
          matrixX <- x$get()
          
          invX <- solve(matrixX)
          x$setInverse(invX)
          
          return(invX)
     }
     else{
          message("pulling from cache")
          
          return(result)
     }
}
