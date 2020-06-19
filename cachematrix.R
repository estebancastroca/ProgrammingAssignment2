makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
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

tmatrix <- makeCacheMatrix(matrix(1:16, nrow = 4, ncol = 4))

tmatrix$get()

tmatrix$getInverse()

cacheSolve(tmatrix)

tmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
tmatrix$get()

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

pmatrix$getInverse()
cacheSolve(tmatrix)

cacheSolve(tmatrix)

tmatrix$getInverse()

#######
tmatrix <- makeCacheMatrix(matrix(1:16, nrow = 2, ncol = 2))

tmatrix$get()

tmatrix$getInverse()

cacheSolve(tmatrix)

tmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
tmatrix$get()

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

pmatrix$getInverse()
cacheSolve(tmatrix)

cacheSolve(tmatrix)

tmatrix$getInverse()

