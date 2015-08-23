#The first function, makeVector creates a special "matrix", which is 
#really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  matinverse <- NULL
  set <- function(y) {
    x <<- y
    matinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matinverse <<- solve
  getinverse <- function() matinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The function cacheSolve returns the inverse of the matrix. First
#it checks if the matrix has already been inversed. If so, it gets the 
#result from the cache and skips the computation. If not, it calculates
#the inverse of the matrix abd sets the value if the inverse via the 
#setinverse function

cacheSolve <- function(x = matrix(), ...) {
  matinverse <- x$getinverse()
  if(!is.null(matinverse)) {
    message("getting cached data")
    return(matinverse)
  }
  data <- x$get()
  matinverse <- solve(data, ...)
  x$setinverse(matinverse)
  matinverse
}
