User
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  
  getmatrix <- function() m
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
         m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

# Créer un objet cacheMatrix
cacheMatrix <- makeCacheMatrix()

# Définir une matrice
mat <- matrix(c(1, 2, 3, 4), nrow = 2)

# Stocker la matrice dans l'objet cacheMatrix
cacheMatrix$set(mat)

# Utiliser cacheSolve pour obtenir l'inverse de la matrice (sera calculé la première fois)
inverse_mat <- cacheSolve(cacheMatrix)
print("Inverse de la matrice:")
print(inverse_mat)

# Utiliser à nouveau cacheSolve (l'inverse sera obtenu à partir de la cache cette fois)
inverse_mat_cached <- cacheSolve(cacheMatrix)
print("Inverse de la matrice (à partir de la cache):")
print(inverse_mat_cached)