## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# create object that stores the matrix and caches its inverse
# The following R code defines matrix function, initialising matrix inverse and setting up matrix value to parent environment(x) and getting x.
# Similarly setting up inverse matrix value to parent environment(MatrixInv) and getting MatrixInv.

makeCacheMatrix <- function(x = matrix()) {                
        MatrixInv <- NULL                                   
        set <- function(y) {                                 
                x <<- y                                     
                MatrixInv <<- NULL
        }
        get <- function() x
        setMatrixInv <- function(inverse) MatrixInv <<- inverse   
        getMatrixInv <- function() MatrixInv
        list(set = set, get = get,
             setMatrixInv = setMatrixInv,
             getMatrixInv = getMatrixInv)
}


## Write a short comment describing this function

# This function finds the matrix inverse if it has not been computed in the above code.
# But if it has been computed in the above code then the following code will help in caching the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MatrixInv <- x$getMatrixInv()
        if(!is.null(MatrixInv)) {
                message("getting cached data")
                return(MatrixInv)
        }
        data <- x$get()
        MatrixInv <- solve(data, ...)
        x$setMatrixInv(MatrixInv)
        MatrixInv
}
