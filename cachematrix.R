## The two functions work together.  The first creates a list of four functions 
## to get and set the values of the matrix and its inverse.
## Second, using the result of the first function, the next function either
## returns the inverse from the cache, or calculates, caches, and returns the inverse.

## makeCacheMatrix takes as its argument a matrix and then produces a list
## of four functions to get the value of the matrix, set the value of the matrix,
## get the value of the inverse matrix, and set the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve must be preceded by makeCacheMatrix and uses the resultant list
## of makeCacheMatrix.  First, the function checks to see if there is an inverse matrix
## already calculated, and if so, returns the inverse matrix from the cache.  
## Otherwise, it calculates the inverse matrix, sets it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
