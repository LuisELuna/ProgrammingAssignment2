##makeCacheMatrix() function needs as an input a matrix given by the user.
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve ##the function uses the solve function that might be from a different environment
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##Using the makeCacheMatrix() function from above the following function will use those arguments as inputs
##the default value for m is Null so cachesolve() will verify if the inverse has been calculated before with getinverse if not
##it will then proceed to solve the matrix and store it as an object m and it will save the argument that m is now setinverse. If we run the function again as m
##is now the inverse matrix and not Null a message will appear with the text "getting cached data" it will skip the computation and will return the inverse.

cachesolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {   ##we check if the inverse has been calculated before
      message("getting cached data")
      return(m)         ## Return a matrix that is the inverse of 'x'
    }
    data <- x$get()
    m <- solve(data) ##The math happens here
    x$setinverse(m)
    m   ## Return a matrix that is the inverse of 'x'
}
