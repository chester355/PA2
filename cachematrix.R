## Below 2 functions cache the inverse of a matrix so that when needed, 
## cached data can be used instead of repeat computation.


## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setInversion <- function(solve) mat <<- solve
    getInversion <- function() mat
    list(set = set, get = get,
         setInversion = setInversion,
         getInversion = getInversion)
}


## This function computes the inverse of the matrix returned by above function.
## If inverse has been computed already, cacheSolve will get the data from the cache.
cacheSolve <- function(x, ...) {

    mat <- x$getInversion()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setInversion(mat)
    mat
}


## Sample Test
##
## > test <- makeCacheMatrix()
## > test$set(matrix(c(4,3,3,2), ncol = 2))
## > test$get()
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > cacheSolve(test)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(test)
## getting cached data
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
