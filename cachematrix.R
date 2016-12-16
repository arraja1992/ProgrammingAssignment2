
##(1)get is a function that returns the vector x stored in the main function.
##(2)set is a function that changes the vector stored in the main function.
##(3)setinverse and getinverse are functions very similar to set and get.
##(4)They don't calculate the inverse of a matrix, they simply store the value of the input in a variable m.
##(5)into the main function makeCacheMatrix (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Function "cacheSolve" computes the inverse of the special "matrix" (which is the input of cachemean) returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache. 
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and x$setinverse(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

d<-matrix(c(3,2,5,12,1,9,5,6,0),3,3)

CachedMatrix<-makeCacheMatrix(d)
cacheSolve(CachedMatrix)
