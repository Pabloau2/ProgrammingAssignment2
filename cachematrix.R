## Takes arguments in form of a vector, creates a matrix_inv
## and calculates and stores the inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL  # provides a default value for "inv" in case its not yet calculated
   y <- NULL  # provides a default value for "inv" in case its not yet calculated
   set <- function(y) {
       x <<- y  ## caches the inputted matrix
       inv <<- NULL  ## sets the value of the inverse to NULL
   }
   get <- function() x
   setInverse <- function(inverse) inv <<- solve(x) #calculate the inverse
   getInverse <- function() inv   # store the inverse calculated above
   list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {

   inv <- x$getInverse()  # retrieves the value stored in "x" and stores in "inv"
   if (!is.null(inv)) {   # checks if value stored in "inv" is NOT NULL.
       message("getting cached data")
       return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
}
## Test by running the following:
## > m1 <- matrix(c(1,2,3,4),2,2)
## > m2 <- makeCacheMatrix(m1)
## > cacheSolve(m2)
