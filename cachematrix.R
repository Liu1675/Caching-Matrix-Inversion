## makeCacheMatrix creates a special vector which stores a matrix vector and
## caches its inverse. This allows cacheSolve to find the cached inverse 
## without re-computing when there has been existing value calculated.

## The output is the input of cacheSolve function, a list
## containing functions to do the following:

## 1. set the matrix of the vector 
## 2. get the matrix of the vector
## 3. set the inverse of the matrix vector 
## 4. get the inverse of the matrix vector

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    ## Set the calculated inverse in the cache
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the input to makeCacheMatrix function.
## It first exams whether the inverse has already been computed.
## Get the inverse from the cache if so; calculate the inverse otherwise.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
