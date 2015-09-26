## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Provides the basic primitives to get/set the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse to NA
    inverse <- NA
    # define the set func for setting the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NA
    }
    # define the get func for getting the matrix
    get <- function() x
    # define the get/set func  for getting/setting the inverse
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    # Return the list with the get/set fns and the
    # setinv/getinv functions
    list(set = set, get = get,
         setinv = setinverse,
         getinv = getinverse)
}


## Write a short comment describing this function
# Function returns the inverse of a matrix. 
# Method: checks to see if the inverse is cached, if NOT, then 
# the inverse is cached and saved in the cache for
# future retrieval

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check if the inverse exists in the cache ? 
    inv <- x$getinv()
    if(!is.na(inv)) {
        # Yes, the inverse exists in the cache. Return it
        message("getting cached data")
        return(inv)
    }
    # Get the matrix specified by the object
    data <- x$get()
    # Call R function to get the inverse of the matrix
    inv <- solve(data, ...)
    # save the inverse in the data object( cache), for future retrieval
    x$setinv(inv)
    # return inverse
    inv
}
