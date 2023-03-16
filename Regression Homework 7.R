data = read.csv('HW7_data.csv')

# Problem 1.1 & 1.2
y = unlist(data[1])
x = unlist(data[2])
n = nrow(data)

t = 1

for (k in 2:1000)
{
  u = sum((y-exp(t*x))*(-x*exp(t*x)))
  d = sum(-x^2*y*exp(t*x) + 2*x^2*exp(2*t*x))
  t = t - u/d
}


# Problem 1.3 & 1.4
ttt = 0
ttt_test = 0

for(kk in 1:100)
{
  q = c(1:n)
  qq = sample(q,replace=TRUE)
  xx = x[qq]
  yy = y[qq]
  sigma_hat = sqrt((1/(n-1)) * sum((y-exp(t*x))^2))
  #sigma_hat = sqrt(var((yy-exp(t*x))))
  yyy = exp(t*x) + sigma_hat*rnorm(n)
  
  ttt[1] = 1
  for (k in 2:300)
  {
    uu = sum(-x*yyy*exp(x*ttt[k-1])+x*exp(2*ttt[k-1]*x))
    dd = sum(-x^2*yyy*exp(x*ttt[k-1]) + (2*x^2)*exp(2*ttt[k-1]*x))
    ttt[k] = ttt[k-1] - uu/dd
  }
  
  ttt_test[kk] = ttt[300]
}

# Problem 1.5
alpha = 0.05
x_bar = mean(x)
log_y_hat = ttt_test*x_bar
crit_val = 1.96
lower_bound = mean(log_y_hat) - crit_val*sqrt(var(log_y_hat))
upper_bound = mean(log_y_hat) + crit_val*sqrt(var(log_y_hat))
