## The makeCacheMatrix function takes an invertible matrix (x) and returns a 
##list of functions that:
## 1. Sets the matrix in an environment external to the current environment
## 2. Gets the matrix
## 3. Sets the inverse of the matrix in an external environment
## 4. Gets the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function returns the inverse of a matrix. It takes the output 
## from makeCacheMatrix (i.e., a list of functions) and first checks if the 
## inverse has already been computed. If yes, then the function returns the cached
## inverse. If no, then the original matrix (the 'x' used in makeCacheMatrix) is 
## retrieved via the get() function and stored in 'data.' The inverse is computed,
## cached using 'x$setinv(i),' and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
