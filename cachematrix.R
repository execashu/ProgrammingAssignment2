## Two functions below are for creation of 
## cached matrix, retrieval of cached inverse
## of matrix if available otherwise calculation 
## of the inverse

## Function is a list of functions which can 
## set a cached matrix as well as retieve 
## a cached matrix, also there are functions 
## to get/set the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get ,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function retrieves the inverse of matrix if stored
## in cache otherwise calculates the inverse using
## solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse")
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
