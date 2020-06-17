## it may take too long to compute the Matrix inverse, especially if it has to be computed repeatedly, 
## For this, is beneficts to caching the inverse of the matrix rather than compute it repeatedly.
## Then, we have two function that can we use to create a special object that stores a matrix and cache its inverse

## Whit this function we can create an special object ("Matrix") that can stored on cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}

## Here, the function "cacheSolve" computes the inverse of the Matrix (created with the first function)
## If the inverse of the Matrix has already been calculated and it has no changes then you should retrieve the inverse of the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
