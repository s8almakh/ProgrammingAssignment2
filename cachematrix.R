## This is Coursera programming assigment consisting of 2 functions that cache and return the invert of a given
## matrix


## First function creates a special matrix object that can cache its inverse; we assume that input matrix is 
## invertable

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y){
                x <<- y
                inverted <- NULL
        }
        get <- function() x
        setMatrix <- function(inverse) inverted <<- inverse
        getMatrix <- function() inverted
        list(get = get, set = set, setMatrix = setMatrix, getMatrix = getMatrix)
}



## Second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <- x$getMatrix()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data, ...)
        x$setMatrix(inverted)
        inverted
}

# let's check it
la <- matrix(c(-1, 3/2, 1, -1), nrow = 2, byrow = T)
lam <- makeCacheMatrix(la)
cacheSolve(lam)
cacheSolve(lam)

