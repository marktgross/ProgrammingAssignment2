## Create and calculate the inverse of a matrix
## Store and cache the inverse matrix value

## Create a list that contains functions to set a matrix value, get
## a matrix value, set the inverse of the matrix, and get the
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
          inv_matr <- NULL
          set <- function(y) {
              x <<- y
              inv_imatr <<- NULL
            }
          get <- function() x
          setinverse <- function(inverse_matrix) inv_matr <<- inverse_matrix
          getinverse <- function() inv_matr
          list(set = set,
               get = get,
               setinverse = setinverse,
               getinverse = getinverse)
          }

## Calculate & return the inverse matrix for a new matrix or
## return cached inverse matrix if already calculated

cacheSolve <- function(x, ...) {
          inv_matr <- x$getinverse()
          if (!is.null(inv_matr)) {
              message("Getting cached data")
              return(inv_matr)
            }
          data <- x$get()
          inv_matr <- solve(data, ...)
          x$setinverse(inv_matr)
          inv_matr
        }
