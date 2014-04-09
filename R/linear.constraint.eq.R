# linear constraint: Aeq(range) * factors == beq
linear.constraint.eq <- function(constraints.linear, range, beq, factors=NULL) {
  add_a <- rep(0, constraints.linear$n)
  if (is.null(factors)) { add_a[range] <- 1 } else { add_a[range] <- factors }
  if (!is.null(constraints.linear$Aeq)) { constraints.linear$Aeq <- rbind(constraints.linear$Aeq, add_a) } else { constraints.linear$Aeq <- add_a }
  if (!is.null(constraints.linear$beq)) { constraints.linear$beq <- c(constraints.linear$beq, beq) } else { constraints.linear$beq <- beq }
  return(constraints.linear)
}