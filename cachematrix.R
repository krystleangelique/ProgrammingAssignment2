## This function will show what 'cache' means. This is basically a way
## of avoiding "recalculations", but instead will just store in 'cache'
## so it can easily be retrieved.

## This function creates a 'matrix' that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y){
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setInverse <- function (inverse) m <<- inverse
    getInverse <- function () m
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by the function 
## "makeCacheMatrix" created above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
          message ("getting cached data")
          return (m)
    }
    data <- x$get()
    m <- solve (data, ...)
    x$setInverse (m)
    m
}
