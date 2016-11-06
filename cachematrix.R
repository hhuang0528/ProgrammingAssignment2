## Overal there are 2 functions, 1st function makeCachMatrix generate a list
## store calculated inverse of matrix and 2nd function to compute inverse of
## matrix but retrived stored inverse in cach from makeCachMatrix

## Creat a list to store inputmatrix and inverse matrix calcuated

makeCachMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## retrieve inverse stored in catche if it's calculated before,
##otherwise compute the inverse of geiven square matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
