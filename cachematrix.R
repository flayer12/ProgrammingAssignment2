## The func makeCacheMatrix creates a 'matrix' object that can cache its inverse
## The inverse of a matrix is found using the 'solve' function

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        ## variables 'x', and 'm' are in the makeVector () environment
        ## functions 'set', 'get', 'setinverse','getinverse' are also in the 
        ## makeVector () environment
        ## there are no free variables in makeVector () environment
        
        ## now we are defining a new function NESTED in makeVector(). This
        ## new function, set(), has it's own environment
        
        set <- function(y= matrix()) {
                ## 'y' is in the set() environment
                ## but 'x' and 'm' are free variables in the set() environment
                ## 'x' and 'm' are defined in the makeVector() environment                 
                ## '<<-" assigns 'y' to 'x'  and NULL to 'm'
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) m <<- solve(x)
        
        ## 'solve' is in the setinverse() environmenT, but 'm' is 
        ##  in the makeVector() environment. so you have to 
        ##  use "<<-" to assign 'solve' to 'm'
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The func cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Returns a matrix that is the inverse of 'x'
}