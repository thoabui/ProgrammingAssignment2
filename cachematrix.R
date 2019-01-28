## Caching the inverse of a matrix

## function to create a special matrix

makeCacheMatrix <- function(x = matrix()) {
    # set m value to null 
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # function to get the inverse of the matrix
    get <- function()x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## function to calculate the inverse of the above matrix. 
## if the inverse already was calculated the function will retrieve the inverse 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
        if (!is.null(m)){
            return(m)
          }
          else 
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
        }



mat <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)

B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
