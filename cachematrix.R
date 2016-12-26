
## makeMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * setInverse     get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeMatrix <- function(x = numeric()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}
cacheInverse <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
#Return a matrix that is the inverse of 'x'

#Here is an example of how this would function:
#x<-makeMatrix(matrix(c(10,12,14,15),nrow=2,ncol = 2))
#summary(x)
#x$getInverse()
#cacheInverse(x) 
#cacheInverse(x) }

##It workeds just the way it should. Basically, it first checks to see if the
#inverse has already been calculated. If so, it gets the value stored from the ##cache directly. Is not, it calculates the inverse of the matrix and sets the
#value of the inverse in the cache via the setInverse function. 
