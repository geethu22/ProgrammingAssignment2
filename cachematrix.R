## The following functions are used to get a matrix as a special object and used to cache the inverse of the matrix. 
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  # Two objects x(matrix) and i are initialized; x is a function argument; i is set to NULL to be used later in the function
                    i <- NULL 
                    set <- function(y) { # setter of the function
                                        x <<- y # set takes y argument and assigns it to x object in parent environment
                                        i <<- NULL # NULL value is assigned to i and this line of code clears any value of i that has been cached before
                    }
                    get <- function() x #getter of the function
                    setinverse <- function(solve) i <<- solve # setter for the matrix once inverse of matrix is computed assigned to i defined in parent environment of makeCacheMatrix
                    getinverse <- function() i # getter of the matrix inverse for object i defined in parent environment of makeCacheMatrix
                    list (set = set, get = get,
                          setinverse = setinverse,
                          getinverse = getinverse) # creates a list; set,get, setinverse and getinverse functions are assigned as an element within a list() and returns to its parent environment of makeCacheMatrix
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { # function that takes argument x, ... pass additional arguments
                    i <- x$getinverse() # i object defined as NULL in the makeCacheMatrix; function attempts to retrieve the matrix inverse 
                    if(!is.null(i)) {
                                        message("getting cached data")
                                        return(i)   # cached value returned if result is NULL
                    }
                    data <- x$get()
                    i <- solve(data, ...)  # else gets the matrix; matrix inverse is calculated and uses setinverse  to set the matrix inverse in input object i and returned to parent environment 
                    x$setinverse(i)
                    i
}