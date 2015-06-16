## Put comments here that give an overall description of what your
## functions do

## Creates a new CacheMatrix 'object', which is really a list
## Stores 4 functions to
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse of the matrix
##    4. get the inverse of the matrix (if already set, otherwise NULL)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinverse <- function(inverse) inv<<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves for the inverse of a matrix 'x'
## where 'x' is a CacheMatrix object (list)
## If the inverse has already been calculated, is
## pulled from the cache; otherwise, it is calculateed
## and stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
