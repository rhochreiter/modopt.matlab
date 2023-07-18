#' @title MatLab(R)-style Quadratic Programming in R using ROI
#'
#' @author Ronald Hochreiter, \email{ron@@hochreiter.net}
#'
#' @description
#' \code{quadprog} provides a simple interface to ROI using the optimization
#' model specification of MatLab(R)
#' 
#' minimize in x: f'*x + 0.5*x'*H*x
#' subject to: 
#'   A*x <= b 
#'   Aeq*x == beq
#'   x >= lb
#'   x <= ub
#' 
#' @param H Quadratic term (matrix) of the objective function
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
#' in \code{fval}.
#' 
#' @export
#' 
#' @examples
#' # Covariance matrix of four stocks (weekly returns from 2011):
#' #
#' #              AAPL          IBM         MSFT         ORCL
#' # AAPL 0.0014708114 0.0006940036 0.0006720841 0.0008276391
#' # IBM  0.0006940036 0.0009643581 0.0006239411 0.0011266429
#' # MSFT 0.0006720841 0.0006239411 0.0009387707 0.0008728736
#' # ORCL 0.0008276391 0.0011266429 0.0008728736 0.0021489512
#' 
#' covariance = matrix(c(0.0014708114, 0.0006940036, 0.0006720841, 0.0008276391, 
#'                       0.0006940036, 0.0009643581, 0.0006239411, 0.0011266429, 
#'                       0.0006720841, 0.0006239411, 0.0009387707, 0.0008728736, 
#'                       0.0008276391, 0.0011266429, 0.0008728736, 0.0021489512), 
#'                       nrow=4, byrow=TRUE)
#' assets <- dim(covariance)[1]
#' 
#' H <- covariance
#' f <- rep(0, assets)
#' Aeq <- rep(1, assets)
#' beq <- 1
#' lb <- rep(0, assets)
#' ub <- rep(1, assets)
#' 
#' solution <- quadprog(H, f, NULL, NULL, Aeq, beq, lb, ub)
#' portfolio <- solution$x
#' print(portfolio)
#' 
quadprog <- function(H, f, A=NULL, b=NULL, Aeq=NULL, beq=NULL, lb=NULL, ub=NULL, x0=NULL, options=NULL) {  
  # parse options
  roi_solver <- "quadprog"
  if("solver" %in% names(options)) { roi_solver <- options$solver }
  
  # objective function
  objective_quad <- H
  objective_linear <- f
  max <- FALSE
  
  # variables
  variables <- length(f)
  types <- rep("C", variables)
  
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

  # solution
  qp <- OP(Q_objective(Q = H, 
                       L = f), 
           L_constraint(L = mat, dir = dir, rhs = rhs),
           bounds = V_bound(li=index, lb=lb, ui=index, ub=ub))

  sol <- ROI_solve(qp, solver = roi_solver)
  
  # solution
  result <- list()
  result$x <- sol$solution
  result$fval <- sol$objval
  result$exitflag <- sol$status$msg$roi_code
  result$output <- sol$status$msg
  result$lambda <- NULL
  return(result)    
}
