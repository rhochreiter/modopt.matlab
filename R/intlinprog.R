#' @title MatLab(R)-style Mixed Integer Linear Programming in R using ROI
#'
#' @author Ronald Hochreiter, \email{ron@@hochreiter.net}
#'
#' @description
#' \code{intlinprog} provides a simple interface to ROI using the optimization
#' model specification of MatLab(R)
#' 
#' minimize in x: f'*x
#' subject to
#'   A*x <= b 
#'   Aeq*x == beq
#'   x >= lb
#'   x <= ub
#'
#' @param f Linear term (vector) of the objective function
#' @param intcon Vector of which variables are integer
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
#' in \code{fval}.
#' 
#' @export
#' 
#' @examples

#' # minimize 8x1 + x2
#' # subject to
#' #   x1 + 2x2 >= -14
#' #   -4x1 - 1x2 <= -33
#' #   2x1 + x2 <= 20
#' #   x1, x2 integer
#' 
#' f <- c(8, 1)
#' A <- matrix(c(-1, -2, -4, -1, 2, 1), nrow=3, byrow=TRUE)
#' b <- c(14, -33, 20)
#' 
#' sol <- intlinprog(f, c(1, 2), A, b)
#' sol <- intlinprog(f, NULL, A, b)
#' 
#' sol$x
#' 
intlinprog <- function(f, intcon=NULL, A=NULL, b=NULL, Aeq=NULL, beq=NULL, lb=NULL, ub=NULL, x0=NULL, options=NULL) {
  # parse options
  roi_solver <- "glpk"
  if("solver" %in% names(options)) { roi_solver <- options$solver }
  
  # objective function
  obj <- f
  max <- FALSE
  
  # variables
  variables <- length(f)
  types <- rep("C", variables)
  if (!is.null(intcon)) { types[intcon] <- "I" }
  
  # constraints
  if (is.null(A)) {
    constraints_leq <- 0
  } else { constraints_leq <- length(b) }
  if (is.null(Aeq)) { 
    constraints_eq <- 0
  } else { constraints_eq <- length(beq) }
  constraints <- constraints_leq + constraints_eq
  
  mat <- rbind(A, Aeq)
  rhs <- c(b, beq)
  dir <- c(rep("<=", constraints_leq), rep("==", constraints_eq))
  
  # bounds
  index <- 1:variables
  if (is.null(lb)) { lb <- rep(0, variables) }  
  if (is.null(ub)) { ub <- rep(Inf, variables) }
  bounds <- list(lower=list(ind=index, val=lb), upper=list(ind=index, val=ub))
  
  # solve
  lp <- OP(objective = obj, 
           L_constraint(L = mat, dir = dir, rhs = rhs), 
           maximum = max,
           bounds = V_bound(li=index, lb=lb, ui=index, ub=ub),
           types=types)
  
  sol <- ROI_solve(lp, solver = roi_solver)
  
  # solution
  result <- list()
  result$x <- sol$solution
  result$fval <- sol$objval
  result$exitflag <- sol$status$msg$roi_code
  result$output <- sol$status$msg
  result$lambda <- NULL
  return(result)  
}
