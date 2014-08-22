## the use of these 2 fonctions allows the programmer to calculate & store in the cache the inverse of a matrice.
## This appears to be useful in a loop fonction for huge matrices, because if the value of a matrice doesn't change,
## its inverse will not be calculated as it has been store in the cache. (sorry i'm not a native english speaker)

## This function
## store the matrix on which we are working on,
## store the inverse matrix that is calculate by the second function, if not the inverse matrix is null
## if the matrix we are working on is changed, the inverse matrix is set to null and has to be recalculated
## by the second function.
makeCacheMatrix <- function(x = matrix()) {
        iMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                iMatrix <<- NULL
        }
        
        get <- function() { x }
        
        setiMatrix <- function(result) { iMatrix <<- result }
        
        getiMatrix <- function() { iMatrix }
        
        list(set = set, get = get,
             setiMatrix = setiMatrix,
             getiMatrix = getiMatrix)
}


## this function verify if the matrix we are working on as already its inverse matrix,
## if not he wil calculate & return it; if yes he will not compute & return the store data
cacheSolve <- function(x, ...) {
        im <- x$getiMatrix()
        if(!is.null(im)) {
                message("getting cached matrix")
                return(im) # exit ici
        }
        matrix_data <- x$get()
        
        im <- solve(matrix_data, ...)
        
        x$setiMatrix(im)
        
        message("computing the matrix inversion")
        im
}
