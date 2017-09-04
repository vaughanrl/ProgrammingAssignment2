## inverting a matrix might be computationallly costly
## so caching the inverse of a matrix might save time if the inverse
## needs to be computed frequently.

## makeCacheMatrix generates a list that contains a function that creates
## a special 'matrix' object tghat caches its inverse
##   - sets value of matrix
##   - gets value of matrix
##   - creates inverse of matrix
##   - gets value of inverse of inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(x2) {
    x <<- x2
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set,
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## cachSolve returns inverse of supplied matrix.
## Assumes supplied matrix is invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
