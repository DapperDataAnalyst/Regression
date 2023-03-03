data = data.frame(read.csv(file.choose()))

#------------
model = lm(formula = y ~ x2 + x3, data = data)
#------------


n = 50
p = 3
y = matrix(data[,1], nrow = n, ncol = 1)
x = as.matrix(data.frame(data[,-1]))
xt = t(x)
xtx = xt%*%x

# Problem 1.1
beta_hat = solve(xtx)%*%xt%*%y
yh = x%*%beta_hat
ehat = y-yh
sqr_resid = t(ehat)%*%ehat
sigmasqr_hat = sqr_resid/(n-p)
sigmasqr_hat = sigmasqr_hat[1,1] # This extracts the value from the matrix cell
cov_matrix = sigmasqr_hat*solve(xtx)

## Beta hat is normally distributed, mean = true beta, variance = 0.0217758277*sigma^2

# Problem 1.2
q = 2
x_red = x[,c(1,2)]
beta_hat_red = solve(t(x_red)%*%x_red)%*%t(x_red)%*%y
yh_red = x_red%*%beta_hat_red
ehat_red = y-yh_red
sqr_resid_red = t(ehat_red)%*%ehat_red
F_stat = ((sqr_resid_red-sqr_resid)/(p-q)) / ((sqr_resid)/(n-p))
F_stat = F_stat[1,1] # Extract value from this 1x1 matrix
F_crit = qf(0.99, (p-q), (n-p))

## Calculated F stat is 6.039699, which is less than the critical value of 7.206839, so we fail to reject the null hypothesis that beta3 = 0

# Problem 1.3
b3_hat = beta_hat[3]
b3_hat_var = cov_matrix[3,3]
T_stat = (b3_hat/sqrt(b3_hat_var)) / sqrt((t(ehat)%*%ehat)/(sigmasqr_hat*(n-p)))
T_stat = T_stat[1,1] # Extract value from matrix
T_pval = 2*(1-pt(T_stat, n-p))

## T_pval of 0.01773173 is greater than the significance level of 0.01, so we fail to reject the null hypothesis that beta3 = 0

# Problem 1.4
H_matrix = x%*%solve(t(x)%*%x)%*%t(x)
leverages = matrix(diag(H_matrix), n, 1)
lev1 = leverages[1,]
lev2 = leverages[2,]

## First leverage score = 0.03693637 and second leverage score = 0.03166807. Since the leverage score of the first observation is greater than that of the second, the first observation is more influential.
## An issue may arise if an observation that is far removed from the other observations has a high leverage score. In such a case, this outlier will drag the fitted line toward itself and possibly away from most other observations, making the model less accurate for the majority of observations.

# Problem 1.5
new_x = c(1, 0.12, 0.56)
y_pred = (t(new_x)%*%beta_hat)[1,1]
var_y_pred = (sigmasqr_hat*t(new_x)%*%solve(xtx)%*%new_x)[1,1]
critical_t = qt(0.975, n-p)
upper_bound = y_pred + critical_t*sqrt(var_y_pred)
lower_bound = y_pred - critical_t*sqrt(var_y_pred)

## Predicted y value is -0.5097454, confidence interval is (-0.585196, -0.4342948)

# Problem 2.1
## The elements of this diagonal matrix are in fact the eigenvalues, and the variance of beta_hat_j = 1/d_j (or 1/nu). When d_j (or nu) is very small, the variance of the estimator becomes very large, making the estimator unreliable.

# Problem 2.2
## Variance of ridge estimator is shown in the third lecture, timestamp 14:00

# Problem 2.3
## Bias of ridge estimator is shown in the third lecture, timestamp 14:35

# Problem 2.4
## A lambda value that is too small will not sufficiently reduce the variance of the estimator. A lambda value that is too large will introduce an overly large amount of bias to the estimator.

# Problem 2.5
## Formula for the MSE of beta_hat_r is shown in the third lecture, timestamp 15:13. MSE is bias squared plus variance.







