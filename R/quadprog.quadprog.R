### MatLab(R)-style Quadratic Programming using quadprog

# minimize in x: f'*x + 0.5*x'*H*x 
# subject to: A*x <= b 
# subject to: Aeq*x == beq
# x >= lb
# x <= ub

quadprog.quadprog <- function(H, f, A=NULL, b=NULL, Aeq=NULL, beq=NULL, lb=NULL, ub=NULL, x0=NULL, options=NULL) {  
  require("quadprog")
  
  # build matrix A
  if (!is.null(Aeq)) { Amat <- Aeq }
  if (!is.null(A)) {if (exists("Amat")) { Amat <- rbind(Amat, -A) } else { Amat <- -A } }
  
  # build vector b
  if (!is.null(beq)) { bvec <- beq }
  if (!is.null(b)) {if (exists("bvec")) { bvec <- c(bvec, -b) } else { bvec <- -b } }
  
  # specify number of equality constraints
  meq <- length(beq)
  
  # add lower and upper bounds
  if (!is.null(lb)) { 
    for(i in 1:length(lb)) {
      lhs_lb = rep(0, length(lb))
      lhs_lb[i] = 1
      Amat <- rbind(Amat, lhs_lb)
      bvec <- c(bvec, lb[i])
    }
  }
  
  if (!is.null(ub)) { 
    for(i in 1:length(ub)) {
      lhs_ub = rep(0, length(ub))
      lhs_ub[i] = 1
      Amat <- rbind(Amat, -lhs_ub)
      bvec <- c(bvec, -ub[i])
    }
  }
  
  # use quadprog
  sol <- solve.QP(H, f, t(Amat), bvec, meq)
  
  # return solution
  result <- list()
  result$x <- sol$solution
  return(result)
}
