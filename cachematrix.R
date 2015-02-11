## cacheMatrix calculates the inverse of a matrix stored using the makeCacheMatrix function
## This set of functions prevents a given matrix from being recalculated repeatedly

## To store a matrix "m", use the following command: "y <- makeCacheMatrix(m)"
## y$get() will then display m
## y$getinverse() will display the inverse once it is calculated in the cacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
## stores the matrix and clears any existing inverse
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

## running y$get() will show the value of the matrix in the command line
    get <- function() x

## setinverse is used primarily to store the result of the inverse from cacheMean
    setinverse <- function(solve) inverse <<- solve

## getinverse will show the value of the inverse matrix in the command line
    getinverse <- function() inverse

## the list is displayed to show all the functions stored in makeCacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a matrix and calculates its inverse
## If the inverse was already calculated, it accesses the cached matrix and displays a message
## For a matrix "m" stored using "y <- makeCacheMatrix(m)", run "cacheSolve(y)"

cacheSolve <- function(x, ...) {

## We look to first see if the inverse has already been calculated and stored in the cache
## If it has, the inverse matrix is retrieved and returned
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("retrieving your cached inverse matrix!")
        return(inverse)
    }

## Otherwise, the matrix is gotten from makeCacheMatrix using the get function
    data <- x$get()
    message("calculating your matrix...")

## ..and then calculated using the solve function.
    inverse <- solve(data, ...)

## Then we store the inverse back in the makeCacheMatrix variable and return it
    x$setinverse(inverse)
    inverse
}
