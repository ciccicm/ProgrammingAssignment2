## We first cache our inverse matrix variable so we don't have to recompute it
## as we change environments

## This cashes the inverse matrix variable `m`

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This inverts the matrix `x` to `m` (if not done so already)

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
          message("getting cached data")
          return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
      ## Return a matrix that is the inverse of 'x'
}
