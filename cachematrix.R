## PROGRAMMING ASSIGNMENT2: LEXICAL SCOPING 

### Catching the Inverse of a Matrix

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a matrix which is a list
# containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

# getters, setters/ mutator, accessor
# getters are program modules that retrieve (access) data within an object
# setters are program modules that set (mutate) the data values within an object

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    # setter for the matrix 
    set <- function(y) {
        x <<- matrix(y)
        m <<- NULL
    }
    # getter for the matrix x
    get <- function() x
    # setter for the inverse m
    setInverse <- function(solve) m <<- solve
    # getter for the inverse m
    getInverse <- function() m
    # assign each of the functions as an element within a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function
# The following function calculates the inverse of the matrix created 
# with the above function.  It first checks to see if the inverse
# has already been calculated. If so, it gets the inverse from the cache and
# skips the computation. Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
