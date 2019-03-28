## The following functions allow user to cache the inverse of a matrix; avoiding
## costly and repetitive computation


## This function creates a special matrix object that can cache the matrix's inverse

 makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {

    x <<- y
    m <<- NULL

  }

  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix is the same), then the
## inverse will be retrieved from the cache

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()

  if(!is.null(m)) {

    message("getting cached data")
    return(m)

    }

  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)

  m
}
