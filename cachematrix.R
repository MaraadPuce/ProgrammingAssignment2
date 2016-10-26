##  The following R functions will allow to cache the potentially time-consuming computation of calculating the inverse of a matrix. The first makeCacheMatrix function will cache the inverse of the matrix so that when we need it again it can be looked up in the cache via the cacheSolve function rather than recomputed.

## makeCacheMatrix creates a special "matrix" object than can cache its inverse and contains 4 functions: set sets the value of the matrix, get returns it, setinv and getinv store the value of the input in variable m and return it, respectively.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinv <- function(solvemat) m <<- solvemat
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. Otherwise data gets the matrix stored with makeCacheMatrix, m calculates the inverse and x$setinv(m) stores it in object m in makeCacheMatrix.

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
