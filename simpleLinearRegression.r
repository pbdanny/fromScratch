# ---- Simple Linear Regression ----
# reference : https://www.colorado.edu/amath/sites/default/files/attached-files/ch12_0.pdf
# reference : https://www.ncl.ac.uk/webtemplate/ask-assets/external/maths-resources/statistics/regression-and-correlation/simple-linear-regression.html
# objective : describe correlation between dependent and independent variable

# Least Square Regression Line ----
# objective : minimize sum square error between the regressed line and data

# Prove of ols estimator 
# based on summation formular : https://are.berkeley.edu/courses/EEP118/current/derive_ols.pdf

x <- c(5, 7, 12, 16, 20)
y <- c(40, 120, 180, 210, 240)

# Proof 1 : calcuate coef from summation formular
a_1 <- (sum(x * y) - (mean(y) * sum(x))) / (sum(x^2) - mean(x) * sum(x))
a_0 <- mean(y) - a_1 * mean(x)
plot(x, y)
lines(range(1, 20), a_0 + a_1 * range(1, 20), col = "green")
text(mean(x), mean(y), paste0("y = ", round(a_1, 3), " + ", round(a_1, 3), "*x"))

# Proof 2 : matrix solution
library(matlib)
X <- cbind(rep(1, length((x))[1]), unname(x))
Y <- matrix(y, ncol = 1)
# matrix from Y = XA, solution X^{-1}Y = A
# since X is not squre matrix
# use left inverse matrix to sove equation
# https://math.stackexchange.com/questions/1335693/invertible-matrix-of-non-square-matrix

X_inv <- inv(t(X) %*% X) %*% t(X)
A <- X_inv %*% Y
A

# Check regression
# F-stat : told if really there is a liner relation between x and y
# R square : how good the regression cover the varience
# t test : H0 the parameter = 0 (no correlation coef with result)
simple_lm <- lm(y ~ x)
summary(simple_lm)
plot(simple_lm)
