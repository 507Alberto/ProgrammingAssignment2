## Uses a created matrix to transform it into a object that can cache the inverse of a matrix.

## Creates a list object with the assigned matrix

makeCacheMatrix <- function(x = matrix(rnorm(25),5,5)) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## calculates the inverse of a new matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    if(exists("datasave")) {
        if(identical(datasave,x$get())){
            message("getting cached data")
            x$setinv(msave)
            return(msave)
        }
    }
    
    data <- x$get()
    datasave <<- x$get()
    m <- solve(data)
    msave <<- m
    x$setinv(m)
    m
}