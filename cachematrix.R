## cacheSolve calculates the inverse of a matrix, when the matrix has first
## been created using the makeCacheMatrix(x) function.

## e.g.
## > regularmatrix <- matrix(rnorm(36),6,6)
## > specialmatrix <- makeCacheMatrix(regularmatrix)
## > testinverted <- cacheSolve(specialmatrix)
## then
## > testinverted %*% regularmatrix
## should have 1 on the diagonal and 0 with in error elsewhere.
## subsequent calls to cacheSolve(testmatrix) are faster.

## return a special list which can be used as the argument to cacheSolve(x) below.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse)
    m <<- inverse
  getinverse <- function()
    m
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}



## Calculates the inverse of the special "matrix" created using the makeCacheMatrix(x) function above.
## Subsequent calls using the same special "matrix" are executed faster since the result has been cached.
cacheSolve <- function(x) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    # message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
