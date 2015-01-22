# This code is 2 functions
# the first of which will create a matrix that will cache its inverse
# it will *cache* the inverse, not compute it, that threw me for a while!

# for the authors own reference:
## tested with the following square matrix:
# x <- matrix(c(4, 2, 7, 6), 2, 2)
# very useful!
# https://class.coursera.org/rprog-010/forum/thread?thread_id=940
# https://class.coursera.org/rprog-010/forum/thread?thread_id=364


## This is the function to cache the matrix, it doesn't create anything
## it's just a store, a lovely, confusing store.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

## This, THIS, is where the action is.  Its all like 'yo cachematrx,
## you got my inverse??' and if it looks in getinverse and is all like
## 'i have NO IDEA what you're even talking about, look in my pockets'  
## then this littlebeauty works it out, gives it over on a setinverse 
## and says 'DO NOT lose this, give it to your parent or something'

## this is my understanding of how it works. I may be wrong.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("i got yer inverse baby...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
