## These functions will return the inverse of a matrix that is entered, but
## will only perform the inverse if the solution is not in cache memory

## This function sets up the 
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) i <<- inverse
      getinv <- function() i
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
