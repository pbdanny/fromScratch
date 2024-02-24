# ---- T.test & Welch t-statistic ----
# Reference : http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
# Test ojbective : mean of 2 groups is difference
# Assumption :
#   1) 2 groups are normally distributed
#   2) 2 groups have same varience
#      If 2 groups have differecen varice, use Welch t-statistic
# 2 type of t.test :
#   1) Paried : related / dependent between 2 group eg. test on same person but across time
#   2) Unparied : unrelated / independent between 2 group

library(dplyr)
library(ggplot2)

# Data in two numeric vectors
women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)
# Create a data frame
my_data <- data.frame(
  group = as.factor(rep(c("Woman", "Man"), each = 9)),
  weight = c(women_weight,  men_weight)
)
my_data %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(weight, nr.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
)

my_data %>%
  ggplot(aes(x = group, y = weight, fill = group)) +
  geom_boxplot() +
  labs(colour = "group")

#---- Check normality
# Reference : http://www.sthda.com/english/wiki/normality-test-in-r
# Method
# 1) plot test : density, q-q
# 2) Shapiro-Wilk test :
#   Null hypthesis = sample is normally distributed
#   Alternate hypothesis = sample is not normally distributed
# if p-value < critical value, reject Null hypothesis -> sample is not normally distributed

my_data %>%
    ggplot(aes(x = weight, colour = group)) +
    geom_density() +
    labs(color = "group")

my_data %>%
    ggplot(aes(sample = weight, colour = group)) +
    geom_qq() + geom_qq_line()
     + labs(colour = "group")

shapiro.test(my_data[my_data$group == "Man", "weight"])$p.value
shapiro.test(my_data[my_data$group == "Woman", "weight"])$p.value
# Both p-value > critical (0.1) = fail to reject null hypothesis = sample from
# normally distributed data

#---- Check same varince for both group
# Use var.test (F.test)
# Null hypothesis : varience group 1 / varience of group 2 = 1
var.test(men_weight, women_weight)
# p.value > critical (0.1) fail to reject null hypothesis -> both group same varience

# My func : t.test
myfunc_t_test_indep <- function(g1, g2) {
    g1_mean <- mean(g1)
    g2_mean <- mean(g2)
    g1_n <- length(g1)
    g2_n <- length(g2)
    pulled_varience <- (sum((g1 - g1_mean)^2) + sum((g2 - g2_mean)^2)) / (g1_n + g2_n - 2)
    pulled_stddev <- sqrt(pulled_varience)
    df <- g1_n + g2_n - 2
    lower_qt <- qt(0.025, df, lower.tail = TRUE)
    upper_qt <- qt(0.025, df, lower.tail = FALSE)
    pulled_stderr <- pulled_stddev * sqrt((1 / g1_n + 1 / g2_n))
    diff_mean <- g1_mean - g2_mean
    t <- (diff_mean) / sqrt((pulled_varience / g1_n) + (pulled_varience / g2_n))
    lower_conf <- diff_mean + (lower_qt * pulled_stderr)
    upper_conf <- diff_mean + (upper_qt * pulled_stderr)
    return(c(g1_mean - g2_mean, t, df, lower_conf, upper_conf))
}

myfunc_t_test_indep(women_weight, men_weight)
# t-statistic = -2.784235

t.test(women_weight, men_weight, var.equal = TRUE)
# t-statistic = -2.7842

# My fuct : Welch t-statistic
myfunc_welch_t_test <- function(g1, g2) {
    g1_mean <- mean(g1)
    g2_mean <- mean(g2)
    g1_n <- length(g1)
    g2_n <- length(g2)
    g1_var <- var(g1)
    g2_var <- var(g2)
    t <- (g1_mean - g2_mean) / sqrt((g1_var / g1_n) + (g2_var / g2_n))
    return(t)
}

myfunc_welch_t_test(women_weight, men_weight)
# t-statistic = -2.784235

t.test(women_weight, men_weight, var.equal = FALSE)
# t-statistic = -2.7842

# Welch t-statistic yield same value since both data group have same varience

# ---- Bernoulli trial & Binomial Stat Test ----
# Reference : https://online.stat.psu.edu/stat504/lesson/2/2.2
# Reference : https://bookdown.org/mpfoley1973/statistics/one-sample-proportion-z-test.html
# Reference : https://influentialpoints.com/Training/binomial_and_related_tests-principles-properties-assumptions.htm
#
# Bernoulli distribution
#   discreate probability distribution of random variable that take value = 1 with probablity = p and take value
#   as 0 with propbability = (1-p)
#   Ex. coin flipping with probability of head (H) = p and tail (T) = 1-p
# Binomial distribution
#   discreate probability distribution of random variable that take exactly k success in n independent trial
#   Pr(X = k) = n!/k!*(n-k)! * p^k (1-p)^(n-k) = Br(n)
# Binomail test
# Assumption
#   1) Each trials are independent
#   2) Prob (p) are same for all n
# Type of  Binomial test
#   1) Large sample approximation : Use Law of large number
#     Sample mean have normal distribution with sample mean estimator = p
#     And std error of sample mean = sqrt(pq/n)
#     Proof : https://stats.stackexchange.com/questions/29641/standard-error-for-the-mean-of-a-sample-of-binomial-random-variables
#     Assumption for Large sample approximation
#       - at lest 5 success, and 5 failure
#       - sample size >= 30
#       - expected prop not extreme (0.2 - 0.8)
#     Reference : https://bookdown.org/mpfoley1973/statistics/one-sample-proportion-z-test.html
#     Under null hypothesis P0
#     z statistic = p - P0 / sqrt(PQ/n)
s <- 20
n <- 40
pi <- s / n
pi_0 <- 0.6
se <- sqrt(pi_0 * (1 - pi_0) / n)
z <- (pi - pi_0) / se
# P-value from manual 
pnorm(z, lower.tail = TRUE)
# equal with prop.test
prop.test(20, 40, p = 0.6, alternative = "less", correct = FALSE)
# prop.test CI use method wilson
DescTools::BinomCI(20, 40, sides = "right", method = "wilson")
# CI from z score, use wald method : replace pi_0 (null hypothesis) with pi (sample prop)
# z = pi - pi_0 / sqrt(pi * (1 - pi) / n)

#   2) Small sample test
#     Use Exact test : use probability mass function to find 1-side only
binom.test(20, 40, p = 0.6, alternative = "less")
pbinom(q = 20, size = 40, p = 0.6, lower.tail = TRUE)