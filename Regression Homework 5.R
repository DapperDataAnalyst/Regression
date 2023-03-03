data = data.frame(read.csv("~/Downloads/HW5_data.csv", header = TRUE))
x = as.matrix(data.frame(data[,-1]))
y = matrix(data[,1], nrow = n, ncol = 1)
n = 200
p = 5
P = t(svd(x, nu = n, nv = p)$u)
Q = t(svd(x, nu = n, nv = p)$v)
big_lambda = diag(svd(x, nu = n, nv = p)$d) # Contains the eigenvalues, not squared
D = P%*%x%*%t(Q)
z = P%*%y


# Problem 1.4
gamma_hat = 0
for (i in 1:p){
  gamma_hat[i] = z[i]/big_lambda[i,i]
}

vect_o_eigens = 0
for (i in 1:p) {
  vect_o_eigens[i] = big_lambda[i,i]
}

# Problem 1.5
ss_eigens = sum(vect_o_eigens^2)

quotient1 = vect_o_eigens[1]^2/ss_eigens
quotient2 = sum(vect_o_eigens[1:2]^2)/ss_eigens
quotient3 = sum(vect_o_eigens[1:3]^2)/ss_eigens
quotient4 = sum(vect_o_eigens[1:4]^2)/ss_eigens
quotient5 = sum(vect_o_eigens[1:5]^2)/ss_eigens

# So then...
m = 4

bh_pca = t(Q[1:m,1:m])%*%gamma_hat[1:m]





