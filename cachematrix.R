## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## set x to be an empty matrix
        m <- NULL ## set inverse to equal null 
        set <- function(y) {
                x <<- y ## set function y, assigns argument to x
                m <<- NULL ## set inverse, assigns argument to null
        }
        get <- function() x
        ## get function returns the matrix
        
        setinverse <- function(solve) m <<- solve
        ## setinverse replace the previous value of m to be the inverse of matrix x
        
        getinverse <- function() m
        ## getinverse returns the fonction inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ## creates a list of the functions
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Retrives the inverse form cache
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                ## If the inverse exist and different from null, cacheSolve retrieve the inverse from the cache value        
        }
        ## If the inverse is NULL, get x 
        data <- x$get()
        ## inverse with the solve() function
        m <- solve(data, ...)
        x$setinverse(m)   
        m ##Returns the new inverse value
}
