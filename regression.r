# ---- Simple Linar Regression ----
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

# ---- Logistic Regression ----
# link function = logit 1 / (1+e ^{-XA})
# reference : https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Logistic-Regression/Logistic-Regression-in-R---An-Example/index.html
# reference : https://uc-r.github.io/logistic_regression

data <- ISLR::Default
set.seed(123)
sam <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.6, 0.4))
train <- data[sam, ]
test <- data[!sam, ]

logit_model <- glm(default ~ balance, family = "binomial", data = train)
summary(logit_model)

# The regress equation is log-odd, only a_0 that we could convert to probability
# by exp(b_0) -> odd -> 1 / (1 + odd) -> probability coef of b_0
# bub exp(b_1) -> odd !-> probability coef of b_1
# from the proof below
# reference : https://www.bookdown.org/rwnahhas/RMPH/
# reference : https://www.researchgate.net/post/How_to_convert_odds_ratio_to_probability
exp(coef(logit_model))

# add type = "response" for prediction as probability
pred <- predict(logit_model,  data.frame(balance = c(1000, 2000)), type = "response")
pred

# Model validation (out-of-sample) ----
test.pred <- predict(logit_model, newdata = test, type = "response")

# confusion matrix
cof_tab <- table(test$default, test.pred > 0.5)
cof_tab
precision_manul <- 40 / (40 + 12)
drecall_manul <- 40 / (40 + 98)
f1_manual <- 2 / ( precision_manul^(-1) + recall_manul^(-1) )

library(yardstick)
fact_test.pred <- as.factor(ifelse(test.pred > 0.5, "Yes", "No"))
cof_tab <- table(test$default, fact_test.pred)
cof_tab
# Yardstick use first level as target (interest) class, for default "Yes" 
# must set level to the second level
precision_manul
precision_vec(test$default, fact_test.pred, event_level = "second")
recall_manul
recall_vec(test$default, fact_test.pred, event_level = "second")
f1_manual
f_meas_vec(test$default, fact_test.pred, event_level = "second")

# ROC curve : TPR = recall vs FPR
# PR curve : preciaion vs recall <- for imbalance
# reference : https://www.evidentlyai.com/classification-metrics/explain-roc-curve
# reference : https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
library(ROCR)
# prediction first parameter = probability
pred <- prediction(test.pred, test$default)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE)
auc <- performance(pred, measure = "auc")
auc@y.values

# pr_curve
pr_perf <- performance(pred, measure = "prec", x.measure = "rec")
plot(pr_perf, colorize = TRUE)
pr_auc <- performance(pred, measure = "aucpr")
pr_auc@y.values
