## These functions will return the inverse of a matrix that is entered, but
## will only perform the inverse if the solution is not in cache memory

## This function sets up the list of necessary functions
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

## This function will return an inverse of the matrix from the cache IF the
## inverse has already been saved to memory. If not, it will perform the inverse
## and save it to memory so the solving doesn't have to occur again.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x' from memory
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      ## Calculate the inverse and save to memory
      i <- solve(data, ...)
      x$setinv(i)
      i
}
