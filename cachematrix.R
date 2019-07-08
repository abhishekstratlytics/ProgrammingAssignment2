## This is a coursera assignment. The outcome of this assignment targerts at creating
##inverse the matrix

## The first function, makeVector creates a special "vector",
#which is a matrix containing a function to
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the inverse value
## d. get the inverse value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the matrix created with the above 
##function
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
      message("getting cached inverse")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  
}

B <- matrix(c(1,2,3,4),2,2)
C<-matrix(c(-2,1,1.5,-0.5),2,2)
B1<-makeCacheMatrix(C)
cacheSolve(B1)
