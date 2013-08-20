\name{quadprog}
\alias{quadprog}
\title{Quadratic Programming}

\description{Solve a standard Quadratic Programming problem}

\details{This function solves a standard Quadratic Programming problem:\cr\cr
  minimize in x: f'*x + 0.5*x'*H*x\cr
  subject to: A*x <= b\cr
  subject to: Aeq*x == beq\cr
  x >= lb\cr
  x <= ub
}

\usage{
  quadprog(H, f, A = NULL, b = NULL, Aeq = NULL, beq = NULL, lb = NULL, ub = NULL, x0 = NULL, options = NULL, solver = "quadprog")
}

\arguments{
  \item{H}{Quadratic term (matrix) of the objective function}
  \item{f}{Linear term (vector) of the objective function}
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
Covariance matrix of four stocks (weekly returns from 2011):
\preformatted{
#              AAPL          IBM         MSFT         ORCL
# AAPL 0.0014708114 0.0006940036 0.0006720841 0.0008276391
# IBM  0.0006940036 0.0009643581 0.0006239411 0.0011266429
# MSFT 0.0006720841 0.0006239411 0.0009387707 0.0008728736
# ORCL 0.0008276391 0.0011266429 0.0008728736 0.0021489512}
covariance = matrix(c(0.0014708114, 0.0006940036, 0.0006720841, 0.0008276391, 0.0006940036, 0.0009643581, 0.0006239411, 0.0011266429, 0.0006720841, 0.0006239411, 0.0009387707, 0.0008728736, 0.0008276391, 0.0011266429, 0.0008728736, 0.0021489512), nrow=4, byrow=TRUE)
assets <- dim(covariance)[1]

H <- covariance
f <- rep(0, assets)
Aeq <- rep(1, assets)
beq <- 1
lb <- rep(0, assets)
ub <- rep(1, assets)

solution <- quadprog(H, f, NULL, NULL, Aeq, beq, lb, ub)
portfolio <- solution$x
print(portfolio)
}

\keyword{Optimization}

\keyword{Quadratic Programming}