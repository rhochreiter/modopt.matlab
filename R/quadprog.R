### MatLab(R)-style Quadratic Programming

# minimize in x: f'*x + 0.5*x'*H*x 
# subject to: A*x <= b 
# subject to: Aeq*x == beq
# x >= lb
# x <= ub

quadprog <- function(H, f, A=NULL, b=NULL, Aeq=NULL, beq=NULL, lb=NULL, ub=NULL, x0=NULL, options=NULL) {  
  # currently only quadprog is supported
  result <- quadprog.quadprog(H, f, A, b, Aeq, beq, lb, ub, x0, options)
  return(result)
}
