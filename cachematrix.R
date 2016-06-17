##  If the contents of a vector are not changing, it caches the inverse of the matrix 
##  so that when we need it again, it can be looked up in the cache rather than recomputed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}



## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix  above. If the inverse has already been calculated (and 
## the matrix has not changed), then  cacheSolve  should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$set_inverse(m)
        m
}
