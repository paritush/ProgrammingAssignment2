
## This function is used to return a list where we store the matrix and inverse of the matrix. It creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {            
  
  Mat_Inv <- NULL                                         # Initializing our Matrix inverse as Null
  
  set <- function(y){                                     # Y parameter matrix is cached and stored as x and Inverse matrix is initialized as NULL
    x <<- y
    Mat_Inv <<- NULL
    
  }
  
  get <- function() x                                     # Returning the value of x matrix which is the input matrix.
  
  ##setInverse <- function(inv) Mat_Inv <<- inv
  setInverse <- function(inverse) Mat_Inv <<- inverse     # Caching and Storing the inverse of the matrix in Mat_Inv 
  
  getInverse  <- function() Mat_Inv                       # returing the Mat_Inv after caching from setInverse
  
  list(set = set,                                         # Creating list to return the calculations from each functions 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                           # Gets list as parameter from function makeCacheMatrix
  
  Mat_Inv <- x$getInverse()                                # Return a matrix from cache to check if the Mat_Inv has inverse matrix stored in cache or not
  
  if(!is.null(Mat_Inv)){                                   # If the Mat_Inv already has cached value,then it is returned directly.
    print("Getting cached matrix")
    return(Mat_Inv)
  }
  
  data <- x$get()                                         
  Mat_Inv <- solve(data, ...)                              # Calculates inverse of the matrix 
  x$setInverse(Mat_Inv)                                    # Passing the inverse matrix as paramter to setInverse function
  Mat_Inv                                                  # Returning the inverse matrix if not found in cache.
  
}


Mat = matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)       # Creating a 2x2 matrix 
Mat_cache = makeCacheMatrix(Mat)                          # Calling makeCacheMatrix using our matrix Mat
cacheSolve(Mat_cache)                                     # Calling cacheSolve using the returned list from makeCacheMatrix

