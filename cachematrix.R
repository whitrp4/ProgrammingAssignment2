## The first function creates a Cached Matrix (makeCacheMatrix). The second function uses the "solve" function to return the inverse of the matrix from Function1 and cache it.

## We create a matrix, x and its inverse m (using the solve function), and a create a list to display our functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #sets the value of m to NULL
  y <- NULL #sets the value of y to NULL
  set <- function(y) {  #set the value of the matrix
    x <<- y # caches the matrix we set up
    m <<- NULL #sets the value of m to NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the inverse of the matrix
  setmatrix <- function(solve) m <<- solve
  #get the inverse of the matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix) #creates a list to display our functions
}

## We first check to see if the inverse of our matrix, m, is already cached. If not, we use the solve function to find the inverse and cache it.

cacheSolve <- function(x, ...) {
  ##return inverse if already cached
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } #if inverse has not already been cached, return m so that we can use it again
  #if not already cached, finds inverse and caches
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
