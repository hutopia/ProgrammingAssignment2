## The functions obtain the inverse of a specified matrix of interest and cache the inverse in an external environment that is different to the current environment. Then the existance of the inverse in an environment can be examined later by consecutive functions in order to decide whether or not to skip the re-calculation of inverse, hence it will result in saving computing times as obtained inverses will not be calculated again in later exercises.

## the first function creates a list to:
## set - set the matrix
## get - get the matrix
## setinverse - set the inverse of the matrix
## getinverse - get the inverse of the matrix
## then they are put together into a list

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function calculates the inverse of the matrix specified in the previous function. It first examines if the inverse is already computed and cached in the memory or environment. If there was stored inverse, then it skips the calculation, otherwise it computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}