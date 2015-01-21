## Assignment 2 is so exciting because it's peer-reviewed!

#Order of proceedings:
#1. make a matrix a
#2. Call makeCacheMatrix to make a new CacheMatrix object b
#3. call cacheSolve() to yield inverse c
#4. check a%*% c to get Identity matrix

#NB hilariously, depending on the original matrix fed in, this function
#will occasionally provide parts of the identity matrix in scientific notation.


## this function caches previously calculated inverse matrices.
#following the example given, it does four things
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse matrix
#4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 
  #here, i is the inverse matrix. We initialize with nothing
  i <- NULL
    
    #part 1: set value of original matrix and associate it with no inverse, yet
  set <- function(y) {
    x <<- y
    i <<- NULL
    message('New matrix, eh? Inverse flushed!')
  }
  
  #part 2: get value of the original matrix
  get <- function() x
  
  #part 3: set value of the inverse matrix
  setinvert <- function(inv) i <<- inv
  
  #part 4: get value of the inverse matrix
  getinvert <- function() i
  
  #the special "vector: that will do all of the above
  list(set=set, get=get, getinvert = getinvert, setinvert=setinvert)
}


## this function reads in some matrix x and returns its inverse.
#Special quality here is that the function will not calculate the inverse
#if it has been calculated previously.
#NB we assume that matrix x always has some inverse

#This function pretty much just follows the logic of the assignment example
#we first check to see if an inverse has been calculated already
#if so, we just return the previously calculated inverse
#if not, we calculate the inverse, assign it to cache, then return the result

cacheSolve <- function(x, ...) {
    
    #getinvert() is a function set in makeCacheMatrix
    #that checks to see if the matrix has previously been solved
  
    i <- x$getinvert()
    
    #if the inv variable is not NULL, return the solution
    #and everyone can go home happy
    if(!is.null(i))
    {
      message("Getting cached inverse matrix!")
      return(i)
    }
    
   
    #if not, then we would have to solve it, for the first time for the last time
    #this part is almost an exact duplicate of the example, except that the
    #a variable name has changed. m is not the first letter in inverse, after all
    data <- x$get()
    i <- solve(data)   
    x$setinvert(i)
    i
}
