## Creates a matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(inv) inv <<- inverse
                getinv <- function()  inv
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        }



## Inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Retrieve the inverse from the cache
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
        }
