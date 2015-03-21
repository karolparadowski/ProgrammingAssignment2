# Assignment2

# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(data_matrix = matrix()) {
    
    cached_inv_matrix <- NULL
    
    set <- function(y) {
        data_matrix <<- y
        cached_inv_matrix <<- NULL
    }
    
    get <- function() data_matrix
    
    setinverse <- function(inv_matrix) {
        cached_inv_matrix <<- inv_matrix
        cached_inv_matrix
    }
    
    getinverse <- function() cached_inv_matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x_list, ...) {
    
    cache <- x_list$getinverse()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x_list$get()
    cache <- solve(data)
    x_list$setinverse(cache)
    cache
}