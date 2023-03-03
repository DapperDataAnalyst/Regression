# Problem 1.1
x = c(1.1, 0.3, 1, 0.75, 1.8)
y = c(4.5, 2, 5, 2.4, 4.8)
n = length(x)

xbar = mean(x)
ybar = mean(y)
sx = sum(x^2)
sy = sum(y^2)

correlation = cor(x,y)

# Problem 1.2
bh = (sum(x*y)-n*xbar*ybar)/(sx-n*xbar^2)
ah = ybar-bh*xbar

# Problem 1.3
yh = ah+bh*x
sh_squared = sum((y-yh)^2)/(n-2)

# Problem 1.5
true_sigma_squared = 0.8
alpha = 0.05
xnorm = x-xbar
sxnorm = sum(xnorm^2)
z = bh/sqrt(true_sigma_squared/sxnorm)
p = 2*(1-pnorm(z, 0, 1))


# Problem 2.1
t_3 = bh*sqrt(sxnorm)/sqrt(sh_squared)

# Problem 2.2
var_ei = (1-1/n-(xnorm[1]^2)/sxnorm)

# Problem 2.3
e = y-yh
e_tilda = e/(sqrt(sh_squared)*sqrt(1-1/n-((xnorm^2)/sxnorm)))

# Problem 2.4
h = 1/n + (xnorm^2)/sxnorm

# Problem 2.5
new_x = 0.5
y_pred = ah + bh*new_x
new_variance = true_sigma_squared*(1/n + (new_x^2)/sxnorm)
upper_bound = qnorm(1-alpha/2, y_pred, sqrt(new_variance))
lower_bound = qnorm(alpha/2, y_pred, sqrt(new_variance))



#-------------------
#-------------------