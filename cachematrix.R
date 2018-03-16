## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

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

