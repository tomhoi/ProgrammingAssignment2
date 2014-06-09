## Put comments here that give an overall description of what your
## functions do

## This function is to make a list containing a function to
## 1. Set a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMat <- NULL
  set <- function(y){
    x <<- y
    inverseMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMat <<- inverse
  getInverse <- function() inverseMat
  list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## The function checks whether the inverse of a matrix has already
## existed. If so, it gets the inverse. Otherwise, it calculate the matrix.
## The calculation is to take care of square or non-square matrices.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    m
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    if( nrow(data) == ncol(data)){
      m <- solve(data)
    }else{
      m <- ginv(data)
    }
    x$setInverse(m)
    m
}
