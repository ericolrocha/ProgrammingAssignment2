## Put comments here that give an overall description of what your
## functions do

## Esta função cria um objeto "matriz" especial que pode armazenar em cache seu inverso.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                                x <<- y
                                inv <<- NULL
                           }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(   set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Esta função calcula o inverso da "matriz" especial criada pela função makeCacheMatrix. 
## Se o inverso já tiver sido calculado, então ele deve recuperar o inverso do cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                                message("getting cached data")
                                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
