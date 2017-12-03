## This creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
                  x <<- y
                  i <<- NULL
             }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set = set, get = get,
                             setinv = setinv,
                             getinv = getinv)
}


## This computes the inverse of the matrix returned by makeCacheMatrix.If
## the inverse has already been calculated, then cacheSolve will 
## return the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
            i <- x$getinv()
            if(!is.null(i)){
                      message("getting cached data")
                      return(i)
              }
            data <- x$get()
            i <- solve(data, ...)
            x$setinv(i)
            i 
      }
