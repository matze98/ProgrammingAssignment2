# this set of functions allows the caching and retrieval of a matrix and its inverse
# and the calculation of the inverse matrix

# makeCacheMatrix contains functions to store and retrieve a matix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xinv <<- inverse
  getinverse <- function() xinv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve generates the inverse of the matrix associated with makeCacheMatrix;
# if the inverse of the matrix is allready calculated and cached it retrieves the cached data,
# otherwise it calculates and stores the inverse of the matrix stored in makeCacheMatrix

cacheSolve <- function(x, ...) {
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinverse(xinv)
  xinv
}

