## Below are two functions that are used to create a special object that stores
## an inversible matrix and cache's its inverse.

# The first function, makeCacheMatrix creates a special "matrix", which is really a
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

       #Function that stores matrix x in cache
       set <- function(y) {
              x <<- y
              m <<- NULL
       }

       #Function that returns matrix x to new scope
       get <- function() x

       #Function that stores calculated value (m) in cache
       setvalue <- function(solve) {
              m <<- solve
       }

       #Function that returns calculated value (m) to new scope
       getvalue <- function() m

       #Make functions available in new scope
       list(set = set,
            get = get,
            setvalue = setvalue,
            getvalue = getvalue)

}

## This function computes the inverse of the special "matrix"
cacheSolve <- function(x, ...) {

       #Check for the inverse (m)
       m <- x$getvalue()

       #If inverse is in cache, return inverse (m)
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }

       #Otherwise, get the inverse(m) of the data (x)
       data <- x$get()
       m <- solve(data, ...)

       #Store inverse(m) in cache, for later use
       x$setvalue(m)

       ## Return a matrix that is the inverse of 'x'
       m
}

### SAMPLE PROGRAM ###

#Inversible Matrix
mymatrix <- rbind(c(1, -2), c(-2, 1))

#Special Matrix
special <- makeCacheMatrix(mymatrix)

#Calculate Inverse of Special Matrix
result1 <- cacheSolve(special)

#Get Inverse of Special Matrix from Cache
result2 <- cacheSolve(special)
