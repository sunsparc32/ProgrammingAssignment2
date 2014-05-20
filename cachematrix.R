makeCacheMatrix <- function(X = numeric()){
    # X is the matrix and M is its inverse
    M <- NULL
    set <- function(Y){
    # Inititalizes the "matrix" and assigns NULL to its inverse
        X <<- Y
        M <<- NULL
    }
    get <- function() X #Simply return the matrix
    setInverse <- function(inverse) M <<- inverse
    getInverse <- function() M #Returns the inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(X, ...){
    # X is the matrix and M is its inverse
    M <- X$getInverse()
    if(!is.null(M)){
        # if the inverse is in the cache
        # we simply return it
        message("getting cached data")
        return(M)
    }
    # This is the branch to compute the invserse if it is not
    # already in the cache
    data <- X$get()
    M <- solve(data, ...) #assumes matrix is invertible (non singular). The
                       # solve function is used to compute the inverse
    X$setInverse(M)    # Cache the computed inverse
    M                  # return the inverse
}
