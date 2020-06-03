## A Program to use Lexical Scoping to achieve Caching of Inverse of a Matrix


## A function to create a Matrix which can cache it's inverse
makeCacheMatrix <- function(x=matrix())
  {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list (set = set, get = get, setInverse = setInverse, get = getInverse)
}

##A function which finds the inverse of a matrix. If there is a cached value, it returns the cache value.
cacheSolve <- function(x,...){
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting Cached Data..")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}