rm(list=ls())
library(gear)
## https://rdrr.io/cran/gear/man/solve_chol.html

set.seed(10)
n=100
# create positive definite matrix a
A = crossprod(matrix(rnorm(n^2), nrow = n))
# create vector x and matrix b
# x can be used to check the stability of the solution
x = matrix(rnorm(n))
b = A %*% x


# standard solve
x1 = solve(A, b)
print(all.equal(x, x1))

# solve using cholesky decomposition
chola = chol(A)
x2 = solve_chol(chola, b)
print(all.equal(x, x2))

# solve using qr decomposition
qra = qr(A)
x3 = solve.qr(qra, b)
print(all.equal(x, x2))

# compare direct inversion
ai1 = solve(A)
ai2 = solve_chol(chola) #using cholesky decomposition
print(all.equal(ai1, ai2)) # should be TRUE