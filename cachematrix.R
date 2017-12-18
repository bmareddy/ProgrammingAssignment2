## The below two functions demonstrate lexical scoping and efficient way of 
## caching data and retrieving the cached data

## This function creates a special matrix which can store it's inverse in cache

makeCacheMatrix <- function(x = matrix(c(1),nrow=1,ncol=1)) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setinv <- function(i) { inv <<- i }
  getinv <- function() { inv }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks and retrieves the inverse of a given matrix from the cache
## If the inverse is not present in cache, it computes the inverse and stores it in cache 
## and returns it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("Found inverse in cache. Retrieving...")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
}
