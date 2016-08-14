# The following two functions are used to cache the inverse of a matrix

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# The following function returns the inverse of the matrix. If the inverse
# is already calculated, it gets the result. If not, it computes the 
# If the inverse has not be caluculated, it sets the value in the cache via
# setinverse function.
# Assumption: the matrix is always invertible

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

## Sample run:
## > x = rbind(c(1, -1/8), c(-1/8, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##        [,1]   [,2]
## [1,]  1.000 -0.125
## [2,] -0.125  1.000

## No cache in the first run
## > cacheSolve(m)
##       [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730
