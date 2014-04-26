## The main target of the following R Code is to create functions
## which are able to cache matrix inversion. This is usually a 
## costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly



## The following function (makeCacheMatrix) creates a special 
## "matrix" object that can cache its inverse.
## It takes an argument x of type numeric vector
## and  it returns a list with 4 list items  (they are actually 
## 4 functions wrapped in a list)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
               
}


##   The following function (cacheSolve) computes the inverse of 
##   the special "matrix" returned by makeCacheMatrix function above.
##   If the inverse has already been calculated (and the matrix has 
##   not changed), then the cachesolve should retrieve the inverse 
##   from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()         #query the x vector's cache
        if(!is.null(m)) {           #if there is a cache
                message("getting cached data")
                return(m)           #just return the cache, no computation needed  
        }
        data <- x$get()             #if there's no cache
        m <- solve(data, ...)       #we actually compute them here 
        x$setInverse(m)             #save the result back to x's cache
        m                           #return the result  
       
}