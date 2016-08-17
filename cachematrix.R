## makeCacheMatrix receives a matrix as input, then outputs a list of four functions (setters and getters) 
## that can calculate and cache the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## retrieves matrix
        setinverse <- function(solve) m <<- solve ## for this particular environment, m is set to be the solve() function
        getinverse <- function() m
        
        ## outputs list of functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() operates in two different ways: if the inverse matrix has already been calculated (m != NULL),
## then it outputs the current value for m. If not (m == NULL), then it calculates the inverse using solve() and caches
## the value by assigning it to the m variable.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse() ## retrieves matrix
        
        ## checks if the inverse matrix has already been calculated by comparing m to NULL
        if(!is.null(m)) {
                message("getting cached data") ## m IS NOT a NULL value so no need to recompute it
                return(m)
        }
        data <- x$get() ## provided that m == NULL, get the matrix in order to compute its inverse
        m <- solve(data, ...) ## calculates the inverse matrix
        x$setinverse(m) ## assigns the inverse matrix to the variable m so it's no longer NULL and can be cached
        m ## returns the inverse matrix
}
