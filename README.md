## modopt.matlab

Modeling Optimization Problems with R - MatLab compatibility functions. This package allows for MatLab-Style notation of matrix-based optimization models and reformats them to the appropriate `ROI` formulation.

### Example Usage

#### Optimization Problem

| maximize: 2x1 + x2
| subject to: 
|   x1 + x2 <= 5
|   x1 <= 3
|   x1 >= 0, x2 >= 0

#### Solution

Matrix formulation and solution using `modopt.matlab` and the function `linprog()`

```
f <- c(2, 1)
A <- matrix(c(1, 1, 1, 0), nrow=2, byrow=TRUE)
b <- c(5, 3)

sol <- linprog(-f, A, b)
sol$x
```

### Install modopt.matlab from CRAN

    install.packages("modopt.matlab")

### Install latest development version of modopt.matlab

    devtools::install_github("rhochreiter/modopt.matlab")
    library("modopt.matlab")
