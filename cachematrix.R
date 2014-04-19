## makeCacheMatrix and cacheSolve take a matrix and cache its inverse.
## This is a useful tool that can be used for calculating inverse of a matrix only one in the program
## Inverse of a matrix in fact heavy to calculate for big matrixes, with cache you have to calculate it only once.

## This function creates an instance of the given matrix (x) and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #sets the value of the function and clean the value of the inverse (new matrix -> new inverse)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #gets the value of the matrix
    get <- function() x
    #sets the invers of the matrix
    setinverse <- function(inverse) inv <<- inverse
    #gets the inverse of the matrix
    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## solves the cache of the given matrix, matrix has to be created using makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #checks if the inverse is contained in the cache
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #gets the matrix and solve its inverse
    data <- x$get()
    inv <- solve(data, ...)
    #sets the cache to the given matrix and return the inverse value
    x$setinverse(inv)
    inv
}
