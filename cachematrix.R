## cachematrix.R has two funtions. The first is really a collection of functions that lets you cache your matrix 
## and her inverse with a series of getters and setters. The second function checks to see if an inverse matrix has 
## already been cached. If it has, it retrieves the information for you instead of running throught he calculations
## again. If there is no cached inverse of your matrix, it calculates the inverse for you and uses the setter function,
## defined previously, to cache the inverse so you won't have to calculate it again the next time.

makeCacheMatrix <- function(x = matrix()) { 
## makeCachematrix gives you a collection of getters and setters to cache your matrix and her inverse. To use this 
## function create an object by assigning the function to a variable, like so: "some_variable <- makeCacheMatrix(x)" 
## where x is an invertable matrix. Use the getter (makeCacheMatrix$get()) to view the stored matrix. Use the setter
## (makeCacheMatrix$set(x); where x is your new matrix) to overwrite your matrix. getInverse and setInverse are provided
## for cacheSolve to use, and should not be used to manually manipulate the chache of the inverted matrix.
        
the_inverse <- NULL #this ensures that you're starting out with no value for your inverse (since none has been calculated)
set <- function(y) {
        x <<- y #the matrix declared with $set becomes the new value for the cached matrix
        the_inverse <<- NULL #cached inverse set back to null, since any stored value will be for an old matrix
        }
get <- function() x #gets the cached value
getInverse <- function() the_inverse #gets the calculated inverse (if any)
setInverse <- function(solve) the_inverse <<- solve #sets the value of the_inverse as the value of the parameter for setInverse
list(set = set, get = get, setInverse=setInverse, getInverse=getInverse) #so that object takes all four functions
}

cacheSolve <- function(x, ...) {
## cacheSolve checks our object for a cached value for the inverse of the matrix. If none exists, it calculates
## the inverse and stores that as the new cached value.

the_inverse <- x$getInverse() #allows you to use the_inverse value from the object within this function
if(!is.null(the_inverse)) { #checks to see a the inverse exists
        message("The inverse has already been calculated and stored; getting cached data") 
                #if the inverse exists, the function let's you know.
        return(the_inverse) #you're given the stored data and the function ends here
        }
data <- x$get() #if the inverse hasn't been calculated, it retrieves the value for the matrix
the_inverse <- solve(data, ...) #the value of the inverse is calculated and assigned to the_inverse
x$setInverse(the_inverse) #setter is used to store the value in the cached object
the_inverse #prints the value of the invers
}