# These two functions allow the user to create the inverse of a matrix.
# While that is simply done via the native solve() function, the
# advantage of this approach is that the inverse is calculated only once.
# Afterwards, the inverse is retrieved from a cache, rather than re-calulated.

# makeCacheMatrix allows the submitted matrix to be manipulated in specific ways and
# opens a location, inv_amatrix, to hold its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv_amatrix <- NULL
        set <- function(y) {
                x <<- y
                inv_amatrix <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(solve){
                inv_amatrix <<- solve      
        } 
        getinverse <- function() {
                inv_amatrix
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

# cacheSolve checks to see if the inverse has already been cached. If it hasn't,
# the function calculates the inverse and caches it.
cacheSolve <- function(x, ...) {
        inv_amatrix <- x$getinverse()
        if(!is.null(inv_amatrix)) {
                message("getting cached data")
                return(inv_amatrix)
        }
        data <- x$get()
        inv_amatrix <- solve(data, ...)
        x$setinverse(inv_amatrix)
        inv_amatrix
}