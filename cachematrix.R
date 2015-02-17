makeCacheMatrix <- function(x = matrix()) {
  cachedmatrix <- NULL
  set <- function(y) {
    x <<- y
    cachedmatrix <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) cachedmatrix <<- matrix
  getmatrix <- function() cachedmatrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

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