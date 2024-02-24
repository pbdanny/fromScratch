# ----- Outlier Detection ----
# Reference : https://www.reneshbedre.com/blog/find-outliers.html
# Reference : https://en.wikipedia.org/wiki/Median_absolute_deviation
# Reference : https://stats.stackexchange.com/questions/121071/can-we-use-leave-one-out-mean-and-standard-deviation-to-reveal-the-outliers/121075#121075

# study data
x <- c(10, 4, 6, 8, 9, 8, 7, 6, 12, 14, 11, 9, 8, 4, 5, 10, 14, 12, 15, 7, 10, 14, 24, 28)

# Boxplot & IQR ----
boxplot(x)
IQR(x)
x_med <- median(x)
q_25 <- quantile(x, probs = 0.25)
q_75 <- quantile(x, probs = 0.75)
iqr <- q_75 - q_25
iqr
t_min <- q_25 - (1.5 * iqr)
t_max <- q_75 + (1.5 * iqr)
# outlier
x[which(x < t_min | x > t_max)]
x_rm_outlier <- x[which(x > t_min & x < t_max)]
# top whisker max after remove outlier
max(x_rm_outlier)
min(x_rm_outlier)

# MED-MAD : Median & MAD ----
med <- median(x)
normal_distribution_constant <- 1.4826
abs_dev <- abs(x - med)
mad <- normal_distribution_constant * median(abs(abs_dev))
outlier_level <- 3
t_min <- med - (outlier_level * mad)
t_max <- med + (outlier_level * mad)
x[which(x < t_min | x > t_max)]

# Mean & SD ----
x_mean <- mean(x)
x_sd <- sd(x)
t_min <- x_mean - (outlier_level * x_sd)
t_max <- x_mean + (outlier_level * x_sd)
x[which(x < t_min | x > t_max)]
