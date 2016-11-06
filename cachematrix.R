## These two functions work to create a matrix object with
## easily accessible getter and setter functions and the ability
## calculate and cache the inverse of a matrix when needed.

## makeCacheMatrix takes a matrix as an argument. 
## It defines getter and setter methods and works with the cacheSolve method.
## The internal functions of makeCacheMatrix are returned in a list for easy access.
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInv <- function(inv) invMat <<- inv
    getInv <- function() invMat
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve uses the getter and setter functions of an instance of makeCacheMatrix with solve()
## to store the inverse of a matrix. It outputs the resulting inverted matrix.
## If the inverse of the matrix was already cached it is returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
}
