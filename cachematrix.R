## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Ex: B = makeCacheMatrix()
## Ex: B$set(matrix(c(1, 3, 7, 5, 2, 6, 9, 8, 4),nrow = 3, ncol = 3))
## function creates a special list containing functions to
## set and get a matrix and functions to set and get the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## Write a short comment describing this function
## Ex: cacheSolve(B) where B is a matrix set in the
## makeCacheMatrix()
## Uses the makeCacheMatrix setsolve and get functions
## to retrieve a matrix previously set and to use the
## setsolve() function to store the inverse matrix.
## If the matrix has previously been inverted the cached
## value will be returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
