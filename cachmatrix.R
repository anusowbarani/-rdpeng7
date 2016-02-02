## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##first we need to create a function that returns a list containing 
##function to get,set value of the matrix and get,set the inverse of 
##the matrix

makeCacheMatrix <- function(x) {
	##initially the inverse value is null
	cacheInverse <- NULL
	  set <- function(userValue = matrix()) {

    x <<- userValue 

    cacheInverse <<- NULL

  }

  

  get <- function() x

  

  ##set inverse variable in parent env to desired value and return the value as a convenience

  setInverse <- function(inverse) {

    cacheInverse <<- inverse 

    return(cacheInverse)

  }

  

  getInverse  <- function() cacheInverse

  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## checking whether the inverse is already calculated and if so retrieving the value

  calculatedInv <- x$getInverse() 

  

  ##checking if inverse already calculated and its a matrix

  if(!is.null(calculatedInv) && is.matrix(calculatedInv)) { 

    message("We found!!!! the inverse value is already calculated and it's cached")

    return(calculatedInv)

  }
  else{
  	message("sorry the inverse is newly calculated")
  	}

  

  ## otherwise get the matrix

  matrixToInvert <- x$get()  

  

  ## solve the matrix and get the possible error and warnings if thrown 
  ##with a try catch statement because some matrix may not be invertible

  calculatedInv <- tryCatch({ 

    solve(matrixToInvert)

  }, warning=function(warn) {

    message("This warning is thrown while evaluating your matrix")

    message(warn)

  }, error=function(err) {

    message("Something is wrong with your matrix try with someother matrix")

    message(err)

   

  })
  ##whether the matrix is inverted or not set the value of calculatedInv to null or the original result

  



  message("the inverse of your given matrix") 

  x$setInverse(calculatedInv)



        ## Return a matrix that is the inverse of 'x'
}
