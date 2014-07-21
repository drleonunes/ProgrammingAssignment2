# It´s a function that creates a special "matrix" that can cache its inverse.

# Functions do:
# a) Set the value of the matrix
# b) Get the value of the matrix
# c) Set the value of the inverse
# d) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverte <- NULL
        set <- function(y) {
                x <<- y
                inverte <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inverte <<- inverse
        getinv <- function() inverte
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Create the inverse of the matrix. 
# If the inverse is already created, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverte <- x$getinv()
        if (!is.null(inverte)) {
                message("capturando os dados armazenados")
                return(inverte)
        }
        data <- x$get()
        inverte <- solve(data, ...)
        x$setinv(inverte)
        inverte
}
