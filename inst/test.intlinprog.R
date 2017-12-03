f <- c(8, 1)
A <- matrix(c(-1, -2, -4, -1, 2, 1), nrow=3, byrow=TRUE)
b <- c(14, -33, 20)

sol <- intlinprog(f, c(1, 2), A, b)
sol <- intlinprog(f, NULL, A, b)
