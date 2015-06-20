## These functions allow us to take a matrix, store it in a cache and create 
## a storage space in the cache for its inverse.
## When we call cacheSolve, if the inverse of the specified matrix is stored in the
##cache then the inverse is retrieved and reported
##if it is not in the cache then it is solved and reported

## The function makeCacheMatrix takes a given matrix, saves it to a cache
## Then a space is created for saving the inverse of the matrix in the cache.


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
      x <<- y
      m <<- NULL
  }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)
  }


## This function first checks the cache to see if the matrix specified "x" already stored.
##If the matrix is not in the cache its inverse is caculated, saved in the cache and reported 
##If the matrix (and its inverse) are in the cache then the inverse of the matrix 
##is called and displayed.

      cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
  }
