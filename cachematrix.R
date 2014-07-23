## These functions save a matrix and calculate/save the inverse of the matrix.
## They also allow you to retrive the matrix and the inverse of the matrix.
## The variables holding the matrix and inverse matrix are only accessible to these functions

## Create an object that saves a matrix and its inverse, and provides 4 methods
## for managing these fields

makeCacheMatrix <- function(x = matrix()) {

    invmatrix <- NULL #init the stored inverse matrix
    set <- function(y) {
        x <<- y # store the new matrix passed by the caller
        invmatrix <<- NULL #clear the inverse matrix, if it exists
    }
    get <- function() x # return the stored matrix
    setinverse <- function(theinvmatrix) invmatrix <<- theinvmatrix
    getinverse <- function() invmatrix # return the stored inverse matrix
    #return the methods as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Create the inverse of a matrix if it doesn't already exist and save it
## for later retrieval/use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getinverse()
    if(!is.null(invmatrix)) {
        message("getting cached data")
        return(invmatrix)
    }
    data <- x$get() # get the saved, normal matrix
    invmatrix <- solve(data, ...) # create the inverse matrix from the normal matrix
    x$setinverse(invmatrix) # store the inverse matrix
    invmatrix # return the inverse matrix to the caller
    
}
