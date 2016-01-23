##
## For programming assignment
##

##
## Returns a list of get set methods to the matrix (x) and its corresponding 
## cached inverse matrix 
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # cache data store that retains the inverse matrix value
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inversematrix) m <<- inversematrix
    getInverseMatrix <- function() m
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


##
## Returns the inverse of the matrix specified within the function parameter x
##
## If inverse matrix is found in the cache store of input parameter x, 
##    the cached value is returned, no recomputation is performed
## If inverse  matrix is not found in the cache store of input parameter x, 
##    only then the inverse is computed using base library solve().
##    Result is then cached into the cache store of input parameter x.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m    
}
