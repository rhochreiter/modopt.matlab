\name{linprog}
\alias{linprog}
\title{Linear Programming}
\description{Solve a standard Linear Programming optimization problem}

\details{This function solves a standard Linear Programming problem in the following form:\cr\cr
  minimize in x: f'*x\cr
  subject to: A*x <= b\cr
  subject to: Aeq*x == beq\cr
  x >= lb\cr
  x <= ub
}

\usage{
  linprog(f, A = NULL, b = NULL, Aeq = NULL, beq = NULL, lb = NULL, ub = NULL, x0 = NULL, options = NULL, solver = "Rglpk")
}

\arguments{
  \item{f}{Objective function}
  \item{A}{Inequality constraints (left-hand side)}
  \item{b}{Inequality constraints (right-hand side)}
  \item{Aeq}{Equality constraints (left-hand side)}
  \item{beq}{Equality constraints (right-hand side)}
  \item{lb}{Lower bound}
  \item{ub}{Upper bound}
  \item{x0}{Initial solution}
  \item{options}{Additional optimization parameters}
  \item{solver}{Solver}
}

\value{
  \item{x}{Solution vector}
}

\references{\url{http://www.modopt.com/}}

\author{Ronald Hochreiter \email{ron@hochreiter.net}}

\examples{
# maximize: 2*x1 + x2;
# subject to: x1+x2 <= 5;
# subject to: x1 <= 3;
# x1 >= 0, x2 >= 0

f <- c(2, 1)
A <- matrix(c(1,1, 1,0), nrow=2, byrow=TRUE)
b <- c(5, 3)

solution <- linprog(-f, A, b)
print(solution$x)
}

\keyword{Optimization}
\keyword{Linear Programming}