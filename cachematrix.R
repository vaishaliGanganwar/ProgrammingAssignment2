## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y		## caches the inputted matrix so that cacheSolve can check whether it has changed
       inv <<- NULL
    }
    get <- function() x

    setinverse <- function(inverse) inv <<- inverse

    getinverse <- function() inv

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # creates a list to house the four functions
}


# The cachesolve function returns the inverse of the matrix. 
#It first checks if the inverse has already been computed.
# If yes, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # if an inverse has already been calculated this gets it
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get() # run the getmatrix function to get the value of the input matrix
    inv <- solve(data)  # compute the value of the inverse of the input matrix
    x$setinverse(inv) # run the setinverse function on the inverse to cache the inverse
	inv
}
