## makeCacheMatrix function store or call an inverse matrix. 
## cacheSolve function inverse a matrix.

## Assign a 2x2 matrix to makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinv <- function(invs) {inv<<-invs}
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Assign a makeCacheMatrix function to cacheSolve

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(is.null(inv)){
                matr<-x$get()
                inv<-solve(matr)
                x$setinv(inv)
                return(inv)
        }
        print("Get inverse matrix from makeCacheMatrix.")
        inv
}
