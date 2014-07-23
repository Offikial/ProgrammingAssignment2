## Below are two functions that are used to create a special object that stores
## an inversible matrix and cache's its inverse.

# The first function, makeVector creates a special "matrix", which is really a
# list containing a function to:
## -- set the value of the matrix, x
## -- get the value of the matrix, x
## -- set the value of the inverse matrix, m
## -- get the value of the inverse matrix, m

# The second function calculates the inverse of the special matrix (x) as needed
#  - First, it checks to see if the inverse has already been calculated.
#    If so, it gets the inverse (m) from the cache and skips the computation.
# - Otherwise, it calculates the inverse of the data and sets the value
#   of the inverse (m) in the cache.

## This function creates a special "matrix" object that can be cache data
makeCacheMatrix <- function(x = matrix()) {

       m <- NULL

       #Function that stores matrix outside of this scope
       set <- function(y) {
              x <<- y
              m <<- NULL
       }

       #Function that returns matrix to new scope
       get <- function() x

       #Function that store calculation for use outside of this scope
       setvalue <- function(solve) {
              m <<- solve
       }

       #Function that returns calculation to new scope
       getvalue <- function() m

       #Make these functions available via this list
       list(set = set,
            get = get,
            setvalue = setvalue,
            getvalue = getvalue)

}

## This function computes the inverse of the special "matrix"
cacheSolve <- function(x, ...) {

       m <- x$getvalue()

       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }

       data <- x$get()
       m <- solve(data, ...)

       x$setvalue(m)

        ## Return a matrix that is the inverse of 'x'
       m
}

#Inversible Matrix
mymatrix <- rbind(c(1, -2), c(-2, 1))

#Special Matrix
special <- makeCacheMatrix(mymatrix)

#Calculate Inverse of Special Matrix
result1 <- cacheSolve(special)

#Get Inverse of Special Matrix from Cache
result2 <- cacheSolve(special)