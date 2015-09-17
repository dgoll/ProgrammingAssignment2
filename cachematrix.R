## makeCacheMatrix and cacheSolve can be used in conjunction to calculate
## and store the inverse of square matricies. makeCacheMatrix stores a 
## list containing four functions that can be used to retreive the
## stored matrix, store a new matrix, store the inverse of the matrix and
## retreive the inverse of the matrix. A matrix is stored in the parent
## environment of these functions. cacheSolve retreives the cached
## matrix inverse, if it has been calculated before, and otherwise uses the
## solve() function to calculate the matrix inverse.

## makeCacheMatrix(x) takes a single matrix, x, as its sole argument and returns
## a list of four functions, storing x in the parent environment of the four functions. 
## set(y) ovewrites the matrix stored in the functions environment with a new 
## matrix, y, and resets any stored inverse to NULL. get() retrieves the stored 
## matrix, x. setinverse(inverse) stores the sole argument as a new object, inv, 
## in the environment of the function. getinverse() retrieves inv.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        invisible(list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse))
}


## cacheSolve takes a cached matrix, created using makeCacheMatrix, and
## either calculates or retreives its inverse. If inv has already been
## defined in the parent environment of getinverse(), it prints 'getting cached
## data' and returns the value of inv using the getinverse() function.
## If inv has not been defined (e.g. inv == NULL), it uses the get()
## function to retrieve and solve the stored matrix. It then uses the
## setinverse() function to define inv in the functions parent environment,
## so that the inverse can be retrieved if cacheSolve is called again 
## on the same cached matrix.

cacheSolve <- function(x, ...) {
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
