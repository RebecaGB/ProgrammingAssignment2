##This are a pair of fuctions that cache the inverse of a matrix to avoid costly computations.


## makeCacheMatriz is a fuction that creates a special "matrix" object that can cache its inverse.

#This fuction works of the next way:
  
#1.- Set the value of the matrix
#2.- Get the value of the matrix
#3.- Set the inverse of the matrix
#4.-Get the inverse of the matrix


makeCacheMatrix <- function(u = matrix()) {
  w <- NULL
  set <- function(v) {
    u <<- v
    w <<- NULL
  }
  get <- function() u
  setSolve <- function(solve) w <<- solve
  getSolve <- function() w
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}



## cacheSolve is a fuction that computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setSolve function.

cacheSolve <- function(u, ...) {
  w <- u$getSolve()
  if(!is.null(w)) {
    message("getting cached data")
    return(w)
  }
  data <- u$get()
  w <- solve(data, ...)
  u$setSolve(w)
  w
}
