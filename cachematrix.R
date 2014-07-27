## The following functions cache the Inverse of a matrix so it can be reused later

## returns matrix x and a list of functions to cache the inv of matrix x
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <-NULL
  set<- function(y) {
                x <<- y
                m <<- NULL
        }
  get<- function() x
  setInv<- function(inv) invMatrix <<- inv
  getInv<- function() invMatrix
  list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Calculate the inverse of a matrix x. Check if its already been computed. 
## If the inverse has already been computed, get the cached version
## Else calcuate Inverse of x, store it in cache
cacheSolve <- function(x, ...) {
   m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
