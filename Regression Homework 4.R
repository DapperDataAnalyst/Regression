data = data.frame(read.csv(file.choose()))

# Problem 1
y_values = data[,1]
ybar = mean(y_values)
xi = 0.01
sigma_sqr = var(y_values)
lambda = 1/sigma_sqr
n = length(y_values)
a = 0.01
b = 0.01

mu = (n*ybar) / (n+xi)
xi_n = n + xi

a_n = a + 0.5*n
b_n = b + 0.5*((t(y_values)%*%y_values) - (((n^2)*ybar^2)/(n+xi)))
b_n = b_n[1.1]


# Problem 2
# Part (1)
# Slide 13
b3_hat / (1 + xi)

# Part (2)
# Slide 11
p1_cov_matrix = solve(t(x)%*%x) / (1 + xi)
p1_cov_matrix[3,3]

# Part (3)
# Slide 22

# Part (4)
# Slide 22
p = 3
q = 2
log_bayes = 0.5*(((t(beta_hat)%*%t(x)%*%x%*%beta_hat) / (1+xi)) - ((t(beta_hat_red)%*%t(x_red)%*%x_red%*%beta_hat_red) / (1+xi))) + 0.5*(p-q) * log(xi/(1+xi))
log_bayes = log_bayes[1,1]

# Part (5)
# Slide 28
new_x = c(1, 0.12, 0.56)
beta_tilde = beta_hat/(1+xi)
Ey = t(new_x)%*%beta_tilde
Ey = Ey[1,1]
var_y = 1 + ((t(new_x)%*%solve(t(x)%*%x)%*%new_x) / (1+xi))
var_y = var_y[1,1]
