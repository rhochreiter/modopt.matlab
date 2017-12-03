#' @title MatLab(R)-style Linear Programming in R using ROI
#'
#' @description
#' \code{linprog} provides a simple interface to ROI using the optimization
#' model specification of MatLab(R)
#' 
#' minimize in x: f'*x
#' subject to: A*x <= b 
#' subject to: Aeq*x == beq
#' x >= lb
#' x <= ub
#'
#' @param f Linear term (vector) of the objective function
#' @param A Inequality constraints (left-hand side)
#' @param b Inequality constraints (right-hand side)
#' @param Aeq Equality constraints (left-hand side)
#' @param beq Equality constraints (right-hand side)
#' @param lb Lower bound
#' @param ub Upper bound
#' @param x0 Initial solution
#' @param options Additional optimization parameters
#' 
#' @return The solution vector in \code{x} as well as the objective value
#' in \code{fval}
#' @export
#' @examples
#' # maximize: 2*x1 + x2;
#' # subject to: x1 + x2 <= 5;
#' # subject to: x1 <= 3;
#' # x1 >= 0, x2 >= 0
#' 
#' f <- c(2, 1)
#' A <- matrix(c(1, 1, 1, 0), nrow=2, byrow=TRUE)
#' b <- c(5, 3)
#' 
#' sol <- linprog(-f, A, b)
#' sol$x
linprog <- function(f, A=NULL, b=NULL, Aeq=NULL, beq=NULL, lb=NULL, ub=NULL, x0=NULL, options=NULL) {
  return(intlinprog(f, NULL, A, b, Aeq, beq, lb, ub, x0, options))  
}
