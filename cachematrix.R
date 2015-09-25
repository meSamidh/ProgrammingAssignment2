## This program finds out the inverse of a matrix. If it finds the results in cache, it retrieves from there. else calculates

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. get the values of the matrix
        ##              2. set the values of the matrix
        ##              3. get the inverse of the matrix
        ##              4. set the inverse of the matrix
        ##         this list is used as the input to cacheSolve()
        
        matinv = NULL
        set = function(y) {
                x <<- y
                matinv <<- NULL
        }
        get = function() x
        setinv = function(inverse) matinv <<- inverse 
        getinv = function() matinv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function takes the output of  makeCacheMatrix() as input and inverses the original matrix
cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        matinv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(matinv)){
                message("Data retrieved from cache")
                return(matinv)
        }
        
        
        mat = x$get()

        matinv = solve(mat, ...)        
        
        x$setinv(matinv)
        
        return(matinv)
}
