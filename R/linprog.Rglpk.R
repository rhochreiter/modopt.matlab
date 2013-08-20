### MatLab(R)-style Linear Programming using Rglpk

# minimize in x: f'*x
# subject to: A*x <= b 
# subject to: Aeq*x == beq
# x >= lb
# x <= ub

linprog.Rglpk <- function(f, A=NULL, b=NULL, Aeq=NULL, beq=NULL, lb=NULL, ub=NULL, x0=NULL, options=NULL) {
  require("Rglpk")
  
  # objective
  obj <- f
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
  
  # solve
  sol <- Rglpk_solve_LP(obj, mat, dir, rhs, types=types, max=max, bounds=bounds)
  solution <- sol$solution
  objective_value <- sol$optimum
  
  # return solution
  result <- list()
  result$x <- solution
  return(result)
}