## Put comments here that give an overall description of what your
## makecachematrix creates matrix  which can cache its inverse 
##cachesolve computes the inverse  of the matrix returned by makecachematrix, if the inverse
## has already been calculated , the function cachesolve pulls that information from the memory


makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


  
 


## As mentioned above cachesolve checks whether the matrix  is already in the memory before it 
## incerse it 


cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}


matrix1<-makecachematrix(matrix(1:4,2,2))

matrix1$get()
matrix1$getinverse()
cachesolve(matrix1)
matrix1$getinverse()


