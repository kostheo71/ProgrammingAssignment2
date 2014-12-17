# The following function returns a list of functions that
# set the value of the matrix mat, 
# get the value of the matrix, 
# set the value of the inverse matrix, 
# get the value of the inverse matrix

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The function returns the inverse of the matrix mat
cacheSolve <- function(mat, ...) {
        # the following lines examine whether the inverse is already calculated
        # if this is true, it returns the cached value...
        inv <- mat$getinverse()
        if(!is.null(inv)) {
                message("Get cache data.")
                return(inv)
        }
        #...otherwise it calculates the inverse of the matrix mat.
        data <- mat$get()
        inv <- solve(data)
        mat$setinverse(inv)
        inv
}
