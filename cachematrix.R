## The first function will create an object that can cache a given matrix's
## inverse and the second function will calculate the inverse OR retrieve it if 
## it has already been computed

## This function will create "makeCacheMatrix" which is an object that can
## cache an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Compute the inverse of the matrix, unless it has already been calculated
## if already calculated, just retrieve the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
}
