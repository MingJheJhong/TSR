#' @title
#' TenStepsReturn
#'
#'
#' @description
#' To find the maximum of ten-step return after testing at a square matrix with random number{-1, 0, 1}
#' where 0, 1 are rewards and the point -1 can not passthrough.
#'
#'
#' @param size Size of the environment, it is a square matrix with random number{-1, 0, 1}.
#' @param row Row of the start point.
#' @param col Column of the start point.
#' @param test The counts of testing.
#' @param discount The discount rate for rewards every step.
#'
#'
#' @return
#' N_left:The numbers of times moving to the left at that point.
#' N_up:The numbers of times moving up at that point.
#' N_right:The numbers of times moving to the right at that point.
#' N_down:The numbers of times moving downward at that point.
#' Environment:The environment with reward 0 and 1, and the point -1 can not passthrough.
#' Path:The passed points.
#' Return:The maximum of ten-step return after testing.
#'
#'
#' @export
#'
#'
#' @examples
#' example = TenStepsReturn(5,3,2)
#' # The environment is a 5*5 square matrix
#' # The start point is 3rd row and 2nd column.
#' # Testing 1000 times.
#' # Discount = 0.9.
#' example # Result



TenStepsReturn = function(size, row, col ,test = 1000, discount = 0.9){

  n = size
  X = matrix(as.integer(runif(n^2, -2, 2)), nrow = n)
  if(X[row, col] < 0) {X[row, col] <- 0}
  L = matrix(rep(0, n^2), nrow = n)
  U = matrix(rep(0, n^2), nrow = n)
  R = matrix(rep(0, n^2), nrow = n)
  D = matrix(rep(0, n^2), nrow = n)

  for(j in 1:test){
    x = row
    y = col
    for (i in 1:10) {
        p = sample(1:4, size = 1)
        if (p == 1) { if ((y != 1)&&(X[x, y-1] != -1)) {
          if(X[x, y-1] == 1) L[x, y] = L[x, y]+10 else L[x, y] = L[x, y]+1
          y = y-1} else y = y }
        else if (p == 2) { if ((x != 1)&&(X[x-1, y] != -1)) {
          if(X[x-1, y] == 1) U[x, y] = U[x, y]+10 else U[x, y] = U[x, y]+1
          x = x-1} else x = x }
        else if (p == 3) { if ((y != 5)&&(X[x, y+1] != -1)) {
          if(X[x, y+1] == 1) R[x, y] = R[x, y]+10 else R[x, y] = R[x, y]+1
          y = y+1} else y = y }
        else { if ((x != 5)&&(X[x+1, y] != -1)) {
          if(X[x+1, y] == 1) D[x, y] = D[x, y]+10 else D[x, y] = D[x, y]+1
          x = x+1} else x = x }
    }
  }
  x = row
  y = col
  P = matrix(rep(0,22), nrow = 11)
  r = X[x, y]
  for (i in 1:10) {
    P[i, 1] = x
    P[i, 2] = y
    p = sample(c(rep(1,L[x,y]),rep(2,U[x,y]),rep(3,R[x,y]),rep(4,D[x,y])),size = 1)
    if (p == 1) { if ((y != 1)&&(X[x, y-1] != -1)) y = y-1 else y = y }
    else if (p == 2) { if ((x != 1)&&(X[x-1, y] != -1)) x = x-1 else x = x }
    else if (p == 3) { if ((y != 5)&&(X[x, y+1] != -1)) y = y+1 else y = y }
    else { if ((x != 5)&&(X[x+1, y] != -1)) x = x+1 else x = x }
    r = r + (discount^i)*X[x, y]
  }
  P[11, 1] = x
  P[11, 2] = y

  result = list(N_left = L, N_up = U, N_right = R, N_down = D, Environment = X, Path = P, Return = r)
  return(result)
}
