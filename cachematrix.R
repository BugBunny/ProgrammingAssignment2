## cacheSolve uses functions defined in makeCacheMatrix
## to store or access the inverse of a matrix so as to
## avoid having to recalculate it repeatedly

## makeCacheMatrix stores a matrix and its inverse in its
## environment; it returns a list of functions used to
## access them

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function (y = matrix()) {
        x <<- y
        inverse <<- NULL
        }
    get <- function() x
    put_inverse <- function(y = matrix()) inverse <<- y
    get_inverse <- function() inverse
    list(set=set, get=get, put_inverse=put_inverse,
         get_inverse=get_inverse)
}

## cacheSolve either retrieves the inverse of a matrix
## held in an instance of makeCacheMatrix or computes
## and caches it, if it has not been cached previously

cacheSolve <- function(x = matrix(), ...) {
    inv <- x$get_inverse()
    if (!is.null(inv))
        message("getting cached inverse matrix")
    else {
        inv <- solve(x$get(), ...)
        x$put_inverse(inv)
    }
    inv
}
