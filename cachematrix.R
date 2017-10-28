## makeCacheMatrix() wil create a special matrix. It can also store the inverse of this matrix
## and has functions to get the matrix, its inverse and manipulate its values.


## makeCacheMatrix() will take a matrix as an argument and stores it in a list. When it is
## initialized it will return a list. The list contains the 4 functions above. So each element
## of the list corresponds to one of these functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## cacheSolve takes the special matrix created with the makeCacheMatrix() function as an argument.
## it first checks if an inverse matrix is already stored in the object, by using the
## getinverse() function and checks if there is a value. If so it returns this value and
## stops the function. Else it will get the matrix using the get() function and applies
## the solve() function to this matrix to compute the inverse. In then sets the inverse,
## by using the setinverse() function and finally returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
