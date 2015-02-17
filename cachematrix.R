## Please refer to README.md for details of the purpose of this code and instructions on its execution.
## Note: The return of makeCacheMatrix should be passed to cacheSolve to correctly execute this program.

## makeCacheMatrix creates the variables used to store the original matrix and the cached matrix. The cached 
## matrix is populated after the cacheSolve function has been run. The function returns a list containing a
## group of functions and data needed for caching the inverted matrix.

## makeCacheMatrix accepts one parameter `x` which must be an inversable matrix. The cachedmatrix variable is
## initally set to NULL to remove any previous data. The subfunctions created in this function are detailed below;
## get        - Function that stores the value of the inital matrix
## setmatrix  - Function that stores the inverted matrix in the cachedmatrix variable
## getmatrix  - Function that recalls the cachedmatrix

makeCacheMatrix <- function(x = matrix()) {
  cachedmatrix <- NULL
  
  get <- function() x
  setmatrix <- function(matrix) cachedmatrix <<- matrix
  getmatrix <- function() cachedmatrix
  
  list(get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve is a function that inverts the matrix stored in makeCacheMatrix.
## If the cache is clear cacheSolve computes the inverse of the original matrix and returns value as a matrix. It then
## stores the inverted matrix in the cache. If the cache contains data this is returned instead of computing again.

cacheSolve <- function(x, ...) {
  invmatrix <- x$getmatrix()
  if(!is.null(invmatrix)) {
    message("getting matrix inverse from cache")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setmatrix(invmatrix)
  invmatrix
}