
## Creates a special matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      ## Initialize the inverse
      m <- NULL
      ## Set the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## Get the matrix
      get <- function() x
      ## Set the inverse of the matrix
      setinverse <- function(inverse) m <<- inverse
      ## Get the inverse of the matrix
      getinverse <- function() m
      ## Return a list
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}



## Compute the inverse of the special matrix
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       m <- x$getinverse()
       ## Return the inverse if it is set
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## Get the matrix
      data <- x$get()
      ## Calculate the inverse
      m <- solve(data, ...)
      ## Set the inverse
      x$setinverse(m)
      ## Return the matrix
      m
}
