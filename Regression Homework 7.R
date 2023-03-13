data = read.csv('HW7_data.csv')

# Problem 1.1 & 1.2
y = unlist(data[1])
x = unlist(data[2])
n = nrow(data)

t = 5

for (k in 2:1000)
{
  u = sum((y-exp(t*x))*(-x*exp(t*x)))
  d = sum(-x^2*y*exp(t*x) + 2*x^2*exp(2*t*x))
  t = t - u/d
}

y_hat = exp(t*x)
e_hat = y - y_hat


# Problem 1.3
ttt = 0
ttt_test = 0

for(kk in 1:100)
{
  q = c(1:n)
  qq = sample(q,replace=TRUE)
  xx = x[qq]
  yy = y[qq]
  sigma_sqr = (1/(n-1)) * sum((yy-exp(t*x))^2)
  yyy = exp(t*x) + sqrt(sigma_sqr)*rnorm(n)
  
  ttt[1] = 1
  for (k in 2:3000)
  {
    uu = sum(-x*yyy*exp(x*ttt[k-1])+x*exp(2*ttt[k-1]*x))
    dd = sum(-x^2*yyy*exp(x*ttt[k-1]) + (2*x^2)*exp(2*ttt[k-1]*x))
    ttt[k] = ttt[k-1] - uu/dd
  }
  
  ttt_test[kk] = ttt[3000]
}

