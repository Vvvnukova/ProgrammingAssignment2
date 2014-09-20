## Optimize inverse calculations with matrix

## Creates global variables x and m, creates functions for working with matrix 

makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solves)  m <<- solves 
        getinverse <- function()  m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If inverse matrix was calculated it takes her from cache else calc it

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