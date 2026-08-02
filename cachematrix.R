## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # 1. Holds the cached inverse, defaults to NULL
  
  # 2. Reset function if a new matrix is passed
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clears previous cached inverse
  }
  
  # 3. Returns the stored matrix
  get <- function() x
  
  # 4. Saves the calculated inverse using <<-
  setInverse <- function(inverse) inv <<- inverse
  
  # 5. Returns the cached inverse
  getInverse <- function() 
  
  # 6. Returns a list containing all 4 functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # 1. Fetch current cached inverse value
  inv <- x$getInverse()
  
  # 2. If it's already computed, return it from memory!
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # 3. If cache is empty, get original matrix
  data <- x$get()
  
  # 4. Compute the matrix inverse using solve()
  inv <- solve(data, ...)
  
  # 5. Save inverse to cache for next time
  x$setInverse(inv)
  
  # 6. Return the inverse matrix
  inv
}
