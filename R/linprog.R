### MatLab(R)-style Linear Programming

# minimize in x: f'*x
# subject to: A*x <= b 
# subject to: Aeq*x == beq
# x >= lb
# x <= ub

linprog <- function(f, A=NULL, b=NULL, Aeq=NULL, beq=NULL, lb=NULL, ub=NULL, x0=NULL, options=NULL) {
  # currently only Rglpk is supported
  result <- linprog.Rglpk(f, A, b, Aeq, beq, lb, ub, x0, options)
  return(result)
}