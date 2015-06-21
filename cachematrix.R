## Put comments here that give an overall description of what your
## functions do computing the inverse of a square matrix and cache the result

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        mx_inv <- NULL
        set <- function(y) {
                x <<- y
                mx_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) mx_inv <<- inv
        getinv <- function() mx_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse matrix of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse matrix has already been calculated. If so, it gets the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates 
## the inverse matrix and sets this value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        mx_inv <- x$getinv()
        if(!is.null(mx_inv)) {
                message("getting cached data")
                return(mx_inv)
        }
        data <- x$get()
        mx_inv <- solve(data, ...)
        x$setinv(mx_inv)
        mx_inv
}