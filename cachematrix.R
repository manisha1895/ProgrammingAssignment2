## My code

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y){
             x <<- y
             mat_inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) mat_inv <<- inverse
        get_inv <- function() mat_inv
        list(set = set , get = get , set_inv = set_inv , get_inv = get_inv)
}


## Following function finds the inverse of the matrix created by makeCacheMatrix() above
## If the inverse already exists , it retrieves from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$get_inv()
        if(!is.null(mat_inv)){
               message("getting cached matrix inverse")
               return mat_inv
        }
        mat <- x$get()
        mat_inv <- solve(mat,...)
        x$get_inv(mat_inv)
        mat_inv

}
