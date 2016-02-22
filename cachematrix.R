## Description
# Functions construct and maintain a cache used to quickly return the result
# for an inverted matrix.

# makeCacheMatrix constructs a list to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      n <- NULL
      set <- function(y) {
        x <<- y
        n <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) n <<- inverse
      getinverse <- function() n
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve return cached inverted matrix; else compute it
cacheSolve <- function(x, ...) {
      n <- x$getinverse()
      if(!is.null(n)) {
          return(n)
      }
      data <- x$get()
      n <- solve(data)
      x$setinverse(n)
      n
}
