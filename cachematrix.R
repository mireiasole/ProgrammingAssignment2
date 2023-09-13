## Put comments here that give an overall description of what your
## functions do

## A function that creates a matrix object and caches its inverse so it doesn't have to be computed repeatedly

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) i <<- inverse
  getinver <- function() i
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}



## A function that calculates the inverse of the previous matrix if it hasn't
#been calculated and cached already; or retrieves the cached inverse if it has

cacheSolve <- function(x, ...) {
  i <- x$getinver()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinver(i)
  i
}
