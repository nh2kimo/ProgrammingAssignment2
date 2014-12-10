## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## make a matrix first
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ### next 3 function are used for cachesolve
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ### return original vector
  setinverse <- function(solve) m <<- solve(x) ### called by cachesolve
  getinverse <- function() m ### return the cached value to cachesolve
  
  ### return a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## make a function to retrive cache value of matrix
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)   ##if no value in cache, then inverse matrix here
  x$setinverse(m)
  m
}
