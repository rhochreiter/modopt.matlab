### Example 1

# maximize: 2*x1 + x2;
# subject to: x1 + x2 <= 5;
# subject to: x1 <= 3;
# x1 >= 0, x2 >= 0

f <- c(2, 1)
A <- matrix(c(1, 1, 1, 0), nrow=2, byrow=TRUE)
b <- c(5, 3)

sol <- linprog(-f, A, b)
sol$x

### Example 2: from book

sol <- linprog(f=-c(0.13, 0.1),
               A=matrix(c(1.5, 1, 1, 1, 0.3, 0.5), nrow = 3, byrow=TRUE),
               b=c(27000, 21000, 9000),
               lb=c(0, 0),
               ub=c(15000, 16000))
sol$x
