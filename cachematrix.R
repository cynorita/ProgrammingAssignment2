## makeCacheMatrix is a function that creates a special "matrix" object that can cashe its inverse

## makeCacheMatrix will do the following operations:
## 1. Set the value of Matrix
## 2. Get the value of Matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y){
                x <<-y
                m <<- NULL
        }
        get <- function ()x
        setinv <- function (solve) m <<- solve
        getinv <- function () m
        list (set=set, get=get,
              setinv = setinv,
              getinv = getinv)
}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has been calculated (and the matrix has not changed), then this function will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cache data")
                return (m)
        }
        data <- x$get()
        m <- solve (data, ...)
        x$setinv(m)
        m
}
