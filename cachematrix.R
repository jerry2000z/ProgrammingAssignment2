# Functions below create special "matrix" with its inverse cached in it.
# Try:
# x <- makeCacheMatrix()
# x$set(matrix(c(0,1,2,1,1,-1,2,4,0),3,3))
# x$get()
# cacheSolve(x)
# x$getinverse()


# Use this function to create a special "matrix", which is actually a list 
# containing functions to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
# 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y){
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) iv <<- inverse
        getinverse <- function() iv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# Use this function to get the inverse of a "matrix" create by makeCacheMatrix.
# If the inverse is already saved in the cache, it will directly use it to reduce 
# some calculation.

cacheSolve <- function(x, ...) {
        iv <- x$getinverse()
        if (!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setinverse(iv)
        iv
}
