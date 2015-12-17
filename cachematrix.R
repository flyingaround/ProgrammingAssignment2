## The script calculates and caches the inverse of a matrix

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache 
#its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set_mat <- function(y) {
        x <<- y
        m <<- NULL
    }
    get_mat <- function() x
    set_inv <- function(solve) m <<- solve
    get_inv <- function() m
    list(set_mat = set_mat, get_mat = get_mat,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Write a short comment describing this function
#This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get_mat()
    m <- solve(data, ...)
    x$set_inv(m)
    m
}
