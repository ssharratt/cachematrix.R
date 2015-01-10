## this is code for the courera "R programming" course
## assignment 2, due January 2015

## the first part of this code, "makeCacheMatrix" is a function
## that creates a special matrix object that can cache its inverse

## the second part of this code, "cacheSolve", computes the inverse
## of the matrix  returned by "makeCacheMatrix".  If the inverse has
## already been computed (and the matrix has not changed), then
## "cacheSolve" should retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  	  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}