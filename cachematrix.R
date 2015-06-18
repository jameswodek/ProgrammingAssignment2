## The CacheMatrix function contains 4 functions
## 1) get
## 2) set
## 3) getsolve
## 4) setsolve
## t

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        #the functions are stored in a list below
        #which can be called later, e.g. $set, $get, etc
        list (set = set, get = get,
              setsolve = setsolve,
              getsolve = getsolve)

}


## cacheSolve will check to see if m exists (in memory) and
## is not NULL, if it does exist it returns the
## inverse of the matrix supplied earlier.
## If it doesn't exist in memory it proceeds
## to the data <-x$get(), which gets the matrix
## stored in makeCacheMatrix, and then passes
## the solve function to m. This will then return
## the inverse of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
