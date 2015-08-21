## makeCacheMatrix takes in a matrix as an argument and 
## and creates get and set functions for the matrix and also 
## it's inverse. After creating a matrix one can always access it 
## afterwards 
## example: 
## example <- makeCacheMatrix(matrix(1:4, c(2,2)))
## cacheSolve(example)


## takes a matrix argument and return a list of set,get,setInverse and getInverse
## variables on the matrix

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) x_inverse <<- inverse
        getInverse <- function() x_inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## takes in a cached matrix (can be created by makeCacheMatrix ) as an argument 
## and returns it's inverse, getting a cached values when already computed and 
## computing it and setting otherwise.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
